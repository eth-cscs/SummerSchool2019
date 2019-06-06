//******************************************
// operators.f90
// based on min-app code written by Oliver Fuhrer, MeteoSwiss
// modified by Ben Cumming, CSCS
// *****************************************

// Description: Contains simple operators which can be used on 3d-meshes

#ifndef OPERATORS_H
#define OPERATORS_H

#include <thrust/device_vector.h>

namespace operators
{
void diffusion_thrust(int nx, int ny, double alpha, double dxs,
                      thrust::device_vector<double>& BND_W, thrust::device_vector<double>& BND_E,
                      thrust::device_vector<double>& BND_S, thrust::device_vector<double>& BND_N,
                      thrust::device_vector<double>& X_OLD, thrust::device_vector<double>& U, thrust::device_vector<double>& S
		      );
} // namespace operators

#endif // OPERATORS_H
