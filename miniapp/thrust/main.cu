// ******************************************
// implicit time stepping implementation of 2D diffusion problem
// Ben Cumming, CSCS
// *****************************************

// A small benchmark app that solves the 2D fisher equation using second-order
// finite differences.

// Syntax: ./main nx ny nt t
#include <algorithm>
#include <iostream>
#include <sstream>
#include <fstream>

#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <cstring>

#include <thrust/fill.h>
#include <thrust/device_ptr.h>
#include <thrust/device_vector.h>

#include <omp.h>

#include "linalg.h"
#include "operators.h"
#include "data.h"
#include "stats.h"

using namespace linalg;
using namespace operators;
using namespace data;
using namespace stats;

// read command line arguments
static void readcmdline(Discretization& options, int argc, char* argv[])
{
    if (argc<5 || argc>6 ) {
        std::cerr << "Usage: main nx ny nt t\n";
        std::cerr << "  nx  number of gridpoints in x-direction\n";
        std::cerr << "  ny  number of gridpoints in y-direction\n";
        std::cerr << "  nt  number of timesteps\n";
        std::cerr << "  t   total time\n";
        std::cerr << "  v   [optional] turn on verbose output\n";
        exit(1);
    }

    // read nx
    options.nx = atoi(argv[1]);
    if (options.nx < 1) {
        std::cerr << "nx must be positive integer\n";
        exit(-1);
    }

    // read ny
    options.ny = atoi(argv[2]);
    if (options.ny < 1) {
        std::cerr << "ny must be positive integer\n";
        exit(-1);
    }

    options.N = options.nx*options.ny;

    // read nt
    options.nt = atoi(argv[3]);
    if (options.nt < 1) {
        std::cerr << "nt must be positive integer\n";
        exit(-1);
    }

    // read total time
    double t = atof(argv[4]);
    if (t < 0) {
        std::cerr << "t must be positive real value\n";
        exit(-1);
    }

    verbose_output = false;
    if( argc==6 ) {
        verbose_output = true;
    }

    // compute timestep size
    options.dt = t / options.nt;

    // compute the distance between grid points
    // assume that x dimension has length 1.0
    options.dx = 1. / (options.nx - 1);

    // set alpha, assume diffusion coefficient D is 1
    options.alpha = (options.dx * options.dx) / (1. * options.dt);
}

// ==============================================================================

int main(int argc, char* argv[])
{
    // read command line arguments
    readcmdline(options, argc, argv);
    int nx = options.nx;
    int ny = options.ny;
    int nt = options.nt;

    // initialize cuda
    int device_count;
    cuda_check_status( cudaGetDeviceCount(&device_count) );
    if(device_count < 1) {
        std::cerr << "error: there should be at least one device per node" << std::endl;
        exit(-1);
    }
    cuda_check_status( cudaSetDevice(0) );

    // get the cublas handle to force cublas initialization outside the main time
    // stepping loop, to ensure that the timing doesn't count initialization costs
    auto handle = cublas_handle();

    // set iteration parameters
    int max_cg_iters     = 200;
    int max_newton_iters = 50;
    double tolerance     = 1.e-6;
    int length = nx*ny;

    std::cout << "========================================================================" << std::endl;
    std::cout << "                      Welcome to mini-stencil!" << std::endl;
    std::cout << "version   :: C++ with CUDA" << std::endl;
    std::cout << "mesh      :: " << options.nx << " * " << options.ny << " dx = " << options.dx << std::endl;
    std::cout << "time      :: " << nt << " time steps from 0 .. " << options.nt*options.dt << std::endl;;
    std::cout << "iteration :: " << "CG "          << max_cg_iters
                                 << ", Newton "    << max_newton_iters
                                 << ", tolerance " << tolerance << std::endl;;
    std::cout << "========================================================================" << std::endl;

    thrust::device_vector<double> X_OLD(length);
    thrust::device_vector<double> B(length);
    thrust::device_vector<double> DELTAX(length);

    // set dirichlet boundary conditions to 0 all around
    thrust::device_vector<double> BND_E(ny,0.0);
    thrust::device_vector<double> BND_W(ny,0.0);
    thrust::device_vector<double> BND_S(nx,0.0);
    thrust::device_vector<double> BND_N(nx,0.0);

    // set the initial condition
    // a circle of concentration 0.1 centred at (xdim/4, ydim/4) with radius
    // no larger than 1/8 of both xdim and ydim

//    double *x_new = (double*) malloc(length*sizeof(*x_new));

    thrust::host_vector<double> x_new(length,0.0);

    double xc = 1.0 / 4.0;
    double yc = (ny - 1) * options.dx / 4;
    double radius = fmin(xc, yc) / 2.0;
    for (int j = 0; j < ny; j++)
    {
        double y = (j - 1) * options.dx;
        for (int i = 0; i < nx; i++)
        {
            double x = (i - 1) * options.dx;
            if ((x - xc) * (x - xc) + (y - yc) * (y - yc) < radius * radius)
                x_new[i+nx*j] = 0.1;
        }
    }

    thrust::device_vector<double> X_NEW(x_new);

    flops_bc = 0;
    flops_diff = 0;
    flops_blas1 = 0;
    iters_cg = 0;
    iters_newton = 0;

    // start timer
    double timespent = -omp_get_wtime();

    double dxs = 1000. * (options.dx * options.dx);

    // main timeloop
    for (int timestep = 1; timestep <= nt; timestep++)
    {
        // set x_new and x_old to be the solution
        X_OLD = X_NEW;

        double residual_thrust;
        bool converged = false;
        int it;
        for (it=0; it<max_newton_iters; it++)
        {
            // compute residual : requires both x_new and x_old

            diffusion_thrust(nx,ny,options.alpha,dxs, BND_W, BND_E, BND_S, BND_N, X_OLD, X_NEW, B);

            residual_thrust = norm2_thrust(B);
            // check for convergence
            if (residual_thrust < tolerance)
            {
                converged = true;
                break;
            }

            // solve linear system to get -deltax
            bool cg_converged_thrust = false;
            cg_thrust( BND_W, BND_E, BND_S, BND_N, X_OLD, DELTAX, B, max_cg_iters, tolerance, cg_converged_thrust);

            // check that the CG solver converged
            if (!cg_converged_thrust) break;

            // update solution
            axpy_thrust( -1.0, DELTAX, X_NEW );  // Thrust
        }
        iters_newton += it+1;

        // output some statistics
        if (converged && verbose_output) {
            std::cout << "step " << timestep
                      << " required " << it
                      << " iterations for residual " << residual_thrust
                      << std::endl;
        }
        if (!converged) {
            std::cerr << "step " << timestep
                      << " ERROR : nonlinear iterations failed to converge" << std::endl;
            break;
        }
    }

    // get times
    timespent += omp_get_wtime();

    ////////////////////////////////////////////////////////////////////
    // write final solution to BOV file for visualization
    ////////////////////////////////////////////////////////////////////

    x_new = X_NEW;

    // binary data
    FILE* output = fopen("output.bin", "w");
    fwrite(x_new.data(), sizeof(double), nx * ny, output);
    fclose(output);

    // meta data
    std::ofstream fid("output.bov");
    fid << "TIME: 0.0" << std::endl;
    fid << "DATA_FILE: output.bin" << std::endl;
    fid << "DATA_SIZE: " << options.nx << " " << options.ny << " 1" << std::endl;;
    fid << "DATA_FORMAT: DOUBLE" << std::endl;
    fid << "VARIABLE: phi" << std::endl;
    fid << "DATA_ENDIAN: LITTLE" << std::endl;
    fid << "CENTERING: nodal" << std::endl;
    fid << "BRICK_SIZE: 1.0 " << (options.ny-1)*options.dx << " 1.0" << std::endl;

    // print table sumarizing results
    std::cout << "--------------------------------------------------------------------------------"
              << std::endl;
    std::cout << "simulation took " << timespent << " seconds" << std::endl;
    std::cout << int(iters_cg) << " conjugate gradient iterations, at rate of "
              << float(iters_cg)/timespent << " iters/second" << std::endl;
    std::cout << iters_newton << " newton iterations" << std::endl;
    std::cout << "--------------------------------------------------------------------------------"
              << std::endl;

    std::cout << "Goodbye!" << std::endl;

    return 0;
}

