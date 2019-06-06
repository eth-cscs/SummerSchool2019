#include "linalg.h"

template <typename F>
bool run_test(F f, const char* name) {
    auto success = f();
    printf("%-25s : ", name);
    if(!success) {
        printf("\033[1;31mfailed\033[0m\n");
        return false;
    }
    printf("\033[1;32mpassed\033[0m\n");
    return true;
} 
template <typename T>
bool check_value(T value, T expected, T tol) {
    if(std::fabs(value-expected)>tol) {
        std::cout << "  expected " << expected << " got " << value << std::endl;
        return false;
    }
    return true;
}

bool test_scaled_diff() {
    auto n = 5;
    thrust::device_vector<double> y(n,0.0);
    thrust::device_vector<double> l(n,7.0);
    thrust::device_vector<double> r(n,2.0);
    
    linalg::scaled_diff_thrust(2.0, l, r, y);
    thrust::host_vector<double> y_out(y);

    bool status = true;
    for(auto i=0; i<n; ++i) {
        status = status && check_value(y_out[i], 10.0, 1.e-13);
    }
    return status;
}

bool test_fill() {
    auto n = 5;
    thrust::device_vector<double> x(n,3.0);
    linalg::fill_thrust(2.0,x);
    thrust::host_vector<double> x_out(x);

    bool status = true;
    for(auto i=0; i<n; ++i) {
        status = status && check_value(x_out[i], 2.0, 1.e-13);
    }
    return status;
}

bool test_axpy() {
    auto n = 5;
    thrust::device_vector<double> x(n,3.0);
    thrust::device_vector<double> y(n,5.0);
    linalg::axpy_thrust(0.5, x, y);

    thrust::host_vector<double> y_out(y);

    bool status = true;
    for(auto i=0; i<n; ++i) {
        status = status && check_value(y_out[i], (0.5*3.0 + 5.0), 1.e-13);
    }
    return status;
}

bool test_add_scaled_diff() {
    auto n = 5;
    thrust::device_vector<double> y(n,0.0);
    thrust::device_vector<double> x(n,3.0);
    thrust::device_vector<double> l(n,7.0);
    thrust::device_vector<double> r(n,2.0);

    linalg::add_scaled_diff_thrust(1.5, x, l, r, y);
    thrust::host_vector<double> y_out(y);

    bool status = true;
    for(auto i=0; i<n; ++i) {
        status = status && check_value(y_out[i], 3. + 1.5 * (7. - 2.), 1.e-13);
    }
    return status;
}

bool test_scale() {
    auto n = 5;
    thrust::device_vector<double> y(n,0.0);
    thrust::device_vector<double> x(n,3.0);

    for(auto i=0; i<n; ++i) {
        x[i] = 3.0;
    }

    linalg::scale_thrust(0.5, x, y);
    thrust::host_vector<double> y_out(y);

    bool status = true;
    for(auto i=0; i<n; ++i) {
        status = status && check_value(y_out[i], 1.5, 1.e-13);
    }
    return status;
}

bool test_lcomb() {
    auto n = 5;
    thrust::device_vector<double> y(n,0.0);
    thrust::device_vector<double> x(n,3.0);
    thrust::device_vector<double> z(n,7.0);

    linalg::lcomb_thrust(0.5, 2.0, x, z, y);
    thrust::host_vector<double> y_out(y);

    bool status = true;
    for(auto i=0; i<n; ++i) {
        status = status && check_value(y_out[i], (0.5*3. + 2.*7.), 1.e-13);
    }
    return status;
}

bool test_copy() {
    auto n = 5;
    thrust::device_vector<double> y(n,0.0);
    thrust::device_vector<double> x(n,3.0);

    linalg::copy_thrust(x,y);
    thrust::host_vector<double> x_out(x);
    thrust::host_vector<double> y_out(y);

    bool status = true;
    for(auto i=0; i<n; ++i) {
        status = status && check_value(y_out[i], x_out[i], 1.e-13);
    }

    return status;
}

bool test_dot() {
    auto n = 5;
    thrust::device_vector<double> y(n,7.0);
    thrust::device_vector<double> x(n,3.0);

    auto result = linalg::dot_thrust(x, y);

    return check_value(result, n*3.*7., 1.e-13);
}

bool test_norm2() {
    auto n = 5;
    thrust::device_vector<double> x(n,2.0);

    auto result = linalg::norm2_thrust(x);

    return check_value(result, sqrt(2.0 * 2.0 * 5.0), 1.e-13);
}

////////////////////////////////////////////////////////////////////////////////
// main
////////////////////////////////////////////////////////////////////////////////
int main(void) {
    run_test(test_scaled_diff,  "scaled_diff_thrust");
    run_test(test_fill,         "fill_thrust");
    run_test(test_axpy,         "axpy_thrust");
    run_test(test_add_scaled_diff, "add_scaled_diff_thrust");
    run_test(test_scale,        "scale_thrust");
    run_test(test_lcomb,        "lcomb_thrust");
    run_test(test_copy,         "copy_thrust");
    run_test(test_dot,          "dot_thrust");
    run_test(test_norm2,        "norm2_thrust");
}

