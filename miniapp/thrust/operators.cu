//******************************************
// operators
// based on min-app code written by Oliver Fuhrer, MeteoSwiss
// modified by Ben Cumming, CSCS
// thrust implementation by Will Sawyer, CSCS
//
// implements
// *****************************************

// Description: Contains simple operators which can be used on 3d-meshes

#include "data.h"
#include "operators.h"
#include "stats.h"
#include <thrust/for_each.h>
#include <thrust/device_vector.h>
#include <thrust/iterator/zip_iterator.h>
#include <thrust/iterator/counting_iterator.h>


namespace operators {

struct boundary_functor
{
    const int nx, ny;
    const double alpha, dxs;

    boundary_functor(int _nx, int _ny, double _alpha, double _dxs) : nx(_nx), ny(_ny), alpha(_alpha), dxs(_dxs) {}
    
    template <typename Tuple>   //     arguments: 0:count  1:BND_W  2:BND_E  3:BND_S  4:BND_N  5:X_OLD  6:U  7:S
    __host__ __device__
    void operator()(Tuple t)
    {
      int n = thrust::get<0>(t); 
      int i = n%nx;
      int j = n/nx;
      int nmi = n-i;
      int nmj = n-j;

      // On boundary but not on corner
      bool is_west  = (i==0) && (j>0) && (j<ny-1);
      bool is_east  = (i==nx-1) && (j>0) && (j<ny-1);
      bool is_south = (j==0) && (i>0) && (i<nx-1);
      bool is_north = (j==ny-1) && (i>0) && (i<nx-1);
      bool is_sw    = (i==0) && (j==0);
      bool is_nw    = (i==0) && (j==ny-1); 
      bool is_se    = (i==nx-1) && (j==0);
      bool is_ne    = (i==nx-1) && (j==ny-1);
      double my_val = thrust::get<6>(t);
      
      if(is_west) {
        thrust::get<7>(t) = -(4. + alpha) * my_val
                                                     + *(&thrust::get<6>(t)+1)
                          + *(&thrust::get<6>(t)+nx) + *(&thrust::get<6>(t)-nx)
                          + *(&thrust::get<1>(t)-nmj) // BND_W(j)
                          + alpha * thrust::get<5>(t)
                          + dxs * my_val * (1.0 - my_val);
      }
      if(is_east) {
        thrust::get<7>(t) = -(4. + alpha) * my_val
                          + *(&thrust::get<6>(t)-1) 
                          + *(&thrust::get<6>(t)+nx) + *(&thrust::get<6>(t)-nx)
                          + *(&thrust::get<2>(t)-nmj) // BND_E(j)
                          + alpha * thrust::get<5>(t)
                          + dxs * my_val * (1.0 - my_val);
      }
      if(is_south) {
        thrust::get<7>(t) = -(4. + alpha) * my_val
                          + *(&thrust::get<6>(t)-1) + *(&thrust::get<6>(t)+1)
                          + *(&thrust::get<6>(t)+nx) 
                          + *(&thrust::get<3>(t)-nmi) // BND_S(i)
                          + alpha * thrust::get<5>(t)
                          + dxs * my_val * (1.0 - my_val);
      }
      if(is_north) {
        thrust::get<7>(t) = -(4. + alpha) * my_val
                          + *(&thrust::get<6>(t)-1) + *(&thrust::get<6>(t)+1)
                                                    + *(&thrust::get<6>(t)-nx)
                          + *(&thrust::get<4>(t)-nmi) // BND_N(i)
                          + alpha * thrust::get<5>(t)
                          + dxs * my_val * (1.0 - my_val);
      }

// TODO:  implement the four corners:  SW, NW, SE, NE
      if(is_sw) {
      }
      if(is_nw) {
      }
      if(is_se) {
      }
      if(is_ne) {
      }
    }
};

struct interior_functor
{
    const int nx, ny;
    const double alpha, dxs;

    interior_functor(int _nx, int _ny, double _alpha, double _dxs) : nx(_nx), ny(_ny), alpha(_alpha), dxs(_dxs) {}
    
    template <typename Tuple>  // arguments: 0:count 1:X_OLD  2:U  3:S
    __host__ __device__
    void operator()(Tuple t)
    {
        int n = thrust::get<0>(t); // this is the counting iterator
        int i = n%nx;
        int j = n/nx;
        bool is_interior = i<(nx-1) && j<(ny-1) && i>0 && j>0;
        if(is_interior) {
           thrust::get<3>(t) = -(4. + alpha) * thrust::get<2>(t)          // central point
                                   + *(&thrust::get<2>(t)-1) + *(&thrust::get<2>(t)+1)    // east and west
                                   + *(&thrust::get<2>(t)-nx) + *(&thrust::get<2>(t)+nx)  // north and south
                                   + alpha * thrust::get<1>(t)
                                   + dxs * thrust::get<2>(t) * (1.0 - thrust::get<2>(t));
        }          
    }
};


void diffusion_thrust(int nx, int ny, double alpha, double dxs,
                      thrust::device_vector<double>& BND_W, thrust::device_vector<double>& BND_E,
                      thrust::device_vector<double>& BND_S, thrust::device_vector<double>& BND_N,
                      thrust::device_vector<double>& X_OLD, thrust::device_vector<double>& U, 
                      thrust::device_vector<double>& S
                     )
{
    const int N = nx*ny;
    thrust::counting_iterator<uint> n_first(0);
    thrust::counting_iterator<uint> n_last = n_first + N;

    // apply the transformation
    thrust::for_each(thrust::make_zip_iterator(thrust::make_tuple(n_first, X_OLD.begin(), U.begin(), S.begin())),
                     thrust::make_zip_iterator(thrust::make_tuple(n_last, X_OLD.end(),   U.end(), S.end())),
                     interior_functor(nx,ny,alpha,dxs));

// TODO:  zip up the tuple for the boundary_functor and invoke with for_each
}


} // namespace operators

