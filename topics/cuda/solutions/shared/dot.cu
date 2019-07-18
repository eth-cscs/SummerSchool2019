#include <iostream>

#include <cuda.h>

#include "util.hpp"

// host implementation of dot product
double dot_host(const double *x, const double* y, int n) {
    double sum = 0;
    for(auto i=0; i<n; ++i) {
        sum += x[i]*y[i];
    }
    return sum;
}

template <int THREADS>
__global__
void dot_gpu_kernel(const double *x, const double* y, double *result, int n) {
    __shared__ double buf[THREADS];
    int i = threadIdx.x;

    // initialize this thread's contribution to the reduction
    buf[i] = 0;
    if (i<n) buf[i] = x[i]*y[i];

    // perform binary reduction
    int mid = THREADS/2;
    while (mid) {
        __syncthreads();
        if (i<mid)
            buf[i] += buf[i+mid];
        mid /= 2;
    }

    // the first thread writes the result
    // no need to __syncthreads()
    if (i==0)
        *result = buf[0];
}

template <int THREADS>
__global__
void dot_gpu_kernel_full(const double *x, const double* y, double *result, int n) {
    __shared__ double buf[THREADS];
    int lid = threadIdx.x;
    int i = threadIdx.x+blockIdx.x*blockDim.x;

    buf[lid] = i<n? x[i]*y[i]: 0;

    int mid = THREADS/2;

    while (mid) {
        __syncthreads();
        if (lid<mid)
            buf[lid] += buf[lid+mid];
        mid /= 2;
    }

    if (lid==0) atomicAdd(result, buf[0]);
}

double dot_gpu(const double *x, const double* y, int n) {
    static double* result = malloc_managed<double>(1);
    *result = 0.;
    //dot_gpu_kernel<1024><<<1, 1024>>>(x, y, result, n);
    dot_gpu_kernel_full<1024><<<(n+1023)/1024, 1024>>>(x, y, result, n);
    cudaDeviceSynchronize();
    return *result;
}

int main(int argc, char** argv) {
    size_t n = read_arg(argc, argv, 1, 4);

    auto size_in_bytes = n * sizeof(double);

    std::cout << "dot product CUDA of length n = " << n
              << " : " << size_in_bytes*1e-9 << "MB\n";

    auto x_h = malloc_host<double>(n, 2.);
    auto y_h = malloc_host<double>(n);
    for(auto i=0; i<n; ++i) {
        y_h[i] = rand()%10;
    }

    auto x_d = malloc_device<double>(n);
    auto y_d = malloc_device<double>(n);

    // copy initial conditions to device
    copy_to_device<double>(x_h, x_d, n);
    copy_to_device<double>(y_h, y_d, n);

    auto result   = dot_gpu(x_d, y_d, n);
    auto expected = dot_host(x_h, y_h, n);
    printf("expected %f got %f\n", (float)expected, (float)result);

    return 0;
}

