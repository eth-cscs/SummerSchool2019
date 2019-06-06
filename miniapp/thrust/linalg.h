// linear algebra subroutines
// Ben Cumming @ CSCS
// Will Sawyer @ CSCS (Thrust implementation)

#ifndef LINALG_H
#define LINALG_H

#include <thrust/device_vector.h>

namespace linalg
{
    extern bool cg_initialized;

    ////////////////////////////////////////////////////////////////////////////////
    //  blas level 1 reductions
    ////////////////////////////////////////////////////////////////////////////////

    // computes the inner product of x and y
    double dot_thrust(thrust::device_vector<double>& X, thrust::device_vector<double>& Y);

    // computes the 2-norm of x
    // x is a vector on length N
    double norm2_thrust(thrust::device_vector<double>& X);

    // sets entries in a vector to value
    // x is a vector on length N
    // value is th
    void fill_thrust(double A, thrust::device_vector<double>& X);

    ////////////////////////////////////////////////////////////////////////////////
    //  blas level 1 vector-vector operations
    ////////////////////////////////////////////////////////////////////////////////

    // computes y := alpha*x + y
    // x and y are vectors on length N
    // alpha is a scalar
    void axpy_thrust(double A, thrust::device_vector<double>& X, thrust::device_vector<double>& Y);

    // computes y = x + alpha*(l-r)
    // y, x, l and r are vectors of length N
    // alpha is a scalar
    void add_scaled_diff_thrust(double A, thrust::device_vector<double>& X, thrust::device_vector<double>& L,
                                thrust::device_vector<double>& R, thrust::device_vector<double>& Y);
    // computes y = alpha*(l-r)
    // y, l and r are vectors of length N
    // alpha is a scalar
    void scaled_diff_thrust(double A, thrust::device_vector<double>& X, thrust::device_vector<double>& Y, thrust::device_vector<double>& Z);

    // computes y := alpha*x
    // alpha is scalar
    // y and x are vectors on length n
    void scale_thrust(double A, thrust::device_vector<double>& X, thrust::device_vector<double>& Y);

    // computes linear combination of two vectors y := alpha*x + beta*z
    // alpha and beta are scalar
    // y, x and z are vectors on length n
    void lcomb_thrust(double A, double B, thrust::device_vector<double>& X, thrust::device_vector<double>& Y, thrust::device_vector<double>& Z);

    // copy one vector into another y := x
    // x and y are vectors of length N
    void copy_thrust(thrust::device_vector<double>& X, thrust::device_vector<double>& Y);

    // conjugate gradient solver
    // solve the linear system A*x = b for x
    // the matrix A is implicit in the objective function for the diffusion equation
    // the value in x constitute the "first guess" at the solution
    // x(N)
    // ON ENTRY contains the initial guess for the solution
    // ON EXIT  contains the solution
    void cg_thrust(thrust::device_vector<double>& BND_W, thrust::device_vector<double>& BND_E, 
                   thrust::device_vector<double>& BND_S, thrust::device_vector<double>& BND_N, 
                   thrust::device_vector<double>& X_OLD, thrust::device_vector<double>& X, thrust::device_vector<double>& B, 
                   const int maxiters, const double tol, bool& success);
}

#endif // LINALG_H

