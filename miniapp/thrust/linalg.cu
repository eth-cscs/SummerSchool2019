// linear algebra subroutines
// Ben Cumming @ CSCS
// Will Sawyer @ CSCS (Thrust implementation)

#include <iostream>

#include <cmath>
#include <cstdio>
#include <thrust/fill.h>
#include <thrust/functional.h>
#include <thrust/device_vector.h>
#include <thrust/transform.h>
#include <thrust/inner_product.h>
#include <thrust/execution_policy.h>
#include <thrust/for_each.h>
#include <thrust/iterator/zip_iterator.h>

#include "linalg.h"
#include "operators.h"
#include "data.h"
#include "stats.h"

struct diff_functor
{
    __host__ __device__
        double operator()(const double& x, const double& y) const {
            return x - y;
        }
};

struct square_functor
{
    __host__ __device__
        double operator()(const double& x) const { 
            return x * x;
        }
};

struct axpy_functor
{
    const double a;

    axpy_functor(double _a) : a(_a) {}

    __host__ __device__
        double operator()(const double& x, const double& y) const {
            return a * x + y;
        }
};

struct scale_functor
{
    const double a;

    scale_functor(double _a) : a(_a) {}

    __host__ __device__
        double operator()(const double& x) const {
            return a * x ;
        }
};

struct scaled_diff_functor
{
    const double a;

    scaled_diff_functor(double _a) : a(_a) {}

    __host__ __device__
        double operator()(const double& x, const double& y) const {
            return a * (x - y);
        }
};

struct lcomb_functor
{
    const double a, b;

    lcomb_functor(double _a, double _b) : a(_a), b(_b) {}

    __host__ __device__
        double operator()(const double& x, const double& y) const {
// TODO:  program the return value
        }
};

struct add_scaled_diff_functor
{
    const double a;
    add_scaled_diff_functor(double _a) : a(_a) {}

     template <typename Tuple>
    __host__ __device__
        void operator()(Tuple t) const {
// TODO: program Y = X + a * (L - R); where the arguments of the tuple are O:X  1:L  2:R  3:Y
        }
};

namespace linalg {

bool cg_initialized = false;

int calculate_grid_dim(const int block_dim, int n) {
    return (n-1)/block_dim + 1;
}

using namespace operators;
using namespace stats;

////////////////////////////////////////////////////////////////////////////////
//  blas level 1 reductions
////////////////////////////////////////////////////////////////////////////////

// computes the inner product of x and y
// x and y are vectors

double dot_thrust(thrust::device_vector<double>& X, thrust::device_vector<double>& Y)
{
         return thrust::inner_product(X.begin(), X.end(), Y.begin(), 0.0);
}

// computes the 2-norm of x
// x is a vector
double norm2_thrust(thrust::device_vector<double>& X)
{
// TODO: implement the norm using sqrt and thrust::inner_product
}

////////////////////////////////////////////////////////////////////////////////
//  blas level 1 vector-vector operations
////////////////////////////////////////////////////////////////////////////////

// computes y = x + alpha*(l-r)
// y, x, l and r are vectors
// alpha is a scalar
void add_scaled_diff_thrust(double A, thrust::device_vector<double>& X, thrust::device_vector<double>& L,
                            thrust::device_vector<double>& R, thrust::device_vector<double>& Y)
{
// TODO:  make tuple using make_zip_iterator where T = (X,L,R,Y)
}

// copy one vector into another y := x
// x and y are vectors of length N
void copy_thrust(thrust::device_vector<double>& X, thrust::device_vector<double>& Y)
{
    Y=X;
}

// sets x := value
// x is a vector
// value is a scalar
void fill_thrust(double A, thrust::device_vector<double>& X)
{
    thrust::fill(X.begin(),X.end(),A);
}

// computes y := alpha*x + y
// x and y are vectors
// alpha is a scalar
void axpy_thrust(double A, thrust::device_vector<double>& X, thrust::device_vector<double>& Y)
{
    // Y <- A * X + Y
    thrust::transform(X.begin(), X.end(), Y.begin(), Y.begin(), axpy_functor(A));
}

// computes y = alpha*(l-r)
// y, l and r are vectors of length N
// alpha is a scalar
void scaled_diff_thrust(double A, thrust::device_vector<double>& X, thrust::device_vector<double>& Y, thrust::device_vector<double>& Z)
{
    // Z <- A * (X - Y)
    thrust::transform(X.begin(), X.end(), Y.begin(), Z.begin(), scaled_diff_functor(A));
}

// computes y := alpha*x
// alpha is scalar
// y and x are vectors
void scale_thrust(double A, thrust::device_vector<double>& X, thrust::device_vector<double>& Y)
{
    thrust::transform(X.begin(), X.end(), Y.begin(), scale_functor(A));
}

// computes linear combination of two vectors y := alpha*x + beta*z
// alpha and beta are scalar
// y, x and z are vectors
void lcomb_thrust(double A, double B, thrust::device_vector<double>& X, thrust::device_vector<double>& Y, thrust::device_vector<double>& Z)
{
    // Z <- A * X + B * Y)
    thrust::transform(X.begin(), X.end(), Y.begin(), Z.begin(), lcomb_functor(A,B));
}


// conjugate gradient solver
// solve the linear system A*x = b for x
// the matrix A is implicit in the objective function for the diffusion equation
// the value in x constitute the "first guess" at the solution
// x(N)
// ON ENTRY contains all parameters BND_X and X_OLD, and the initial guess for the solution,
// ON EXIT  contains the solution

void cg_thrust(thrust::device_vector<double>& BND_W, thrust::device_vector<double>& BND_E, 
               thrust::device_vector<double>& BND_S, thrust::device_vector<double>& BND_N, 
               thrust::device_vector<double>& X_OLD, thrust::device_vector<double>& X, thrust::device_vector<double>& B, 
               const int maxiters, const double tol, bool& success)
{
    // this is the dimension of the linear system that we are to solve
    int nx = data::options.nx;
    int ny = data::options.ny;
    double ALPHA = data::options.alpha;
    double dxs = 1000. * (data::options.dx * data::options.dx);

    int length = nx*ny;

    // epsilon value use for matrix-vector approximation
    double eps     = 1.e-8;
    double eps_inv = 1. / eps;

// initialize memory for temporary storage
    
    thrust::device_vector<double> V(length, 0.0);
    thrust::device_vector<double> R(length, 0.0);
    thrust::device_vector<double> P(length, 0.0);
    thrust::device_vector<double> Ap(length, 0.0);
    thrust::device_vector<double> Fx(length, 0.0);
    thrust::device_vector<double> Fxold(length, 0.0);
    thrust::device_vector<double> Xhold(X);

    // matrix vector multiplication is approximated with
    // A*v = 1/epsilon * ( F(x+epsilon*v) - F(x) )
    //     = 1/epsilon * ( F(x+epsilon*v) - Fxold )
    // we compute Fxold at startup
    // we have to keep x so that we can compute the F(x+exps*v)
    // diffusion_raw(X_dv_ptr, Fxold_dv_ptr);

    diffusion_thrust( nx, ny, ALPHA, dxs, BND_W, BND_E, BND_S, BND_N, X_OLD, X, Fxold );

    // v = x + epsilon*x
    scale_thrust( (1.0+eps), X, V );

    // Fx = F(v)
    diffusion_thrust( nx, ny, ALPHA, dxs, BND_W, BND_E, BND_S, BND_N, X_OLD, V, Fx );

    // r = b - A*x
    // where A*x = (Fx-Fxold)/eps
    add_scaled_diff_thrust( -eps_inv, B, Fx, Fxold, R );

    // p = r
    P = R;

    // rold = <r,r>
    double rold = dot_thrust(R, R);
    double rnew = rold;

    // check for convergence
    success = sqrt(rold) < tol;
    if (success) {
        return;
    }

    int iter;
    for(iter=0; iter<maxiters; iter++) {
        // Ap = A*p
        lcomb_thrust( 1.0, eps, Xhold, P, V );

        diffusion_thrust( nx, ny, ALPHA, dxs, BND_W, BND_E, BND_S, BND_N, X_OLD, V, Fx );

        //ss_scaled_diff(Ap, eps_inv, Fx, Fxold);
        scaled_diff_thrust( eps_inv, Fx, Fxold, Ap );

        // alpha = rold / p'*Ap
        double alpha = rold / dot_thrust(P, Ap);

        // x += alpha*p
        axpy_thrust(alpha, P, X);

        // r -= alpha*Ap
        axpy_thrust(-alpha, Ap, R );

        // find new norm
        rnew = dot_thrust(R,R);

        // test for convergence
        if (sqrt(rnew) < tol) {
            success = true;
            break;
        }

        // p = r + (rnew/rold) * p
        lcomb_thrust( 1.0, rnew / rold, R, P, P);

        rold = rnew;
    }
    stats::iters_cg += iter + 1;

    if (!success) {
        std::cerr << "ERROR: CG_thrust failed to converge after " << iter
                  << " iterations, with residual " << sqrt(rnew)
                  << std::endl;
    }
}

} // namespace linalg
