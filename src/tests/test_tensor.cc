#include <vector>
#include "eckit/linalg/Tensor.h"
#include "eckit/exception/Exceptions.h"


// ========= preliminary checks on tensor size, rank and layout flag =========
template<typename T>
void cxx_check_tensor(void* tensor_cptr, size_t expected_size, size_t expected_rank, int layout) {
    typename eckit::linalg::Tensor<T>* tensor_ptr = static_cast<typename eckit::linalg::Tensor<T>*>(tensor_cptr);
    ASSERT(tensor_ptr->size() == expected_size);
    ASSERT(tensor_ptr->shape().size() == expected_rank);
    ASSERT(static_cast<int>(tensor_ptr->layout()) == layout);
}


// ========= checks on tensor data =========
template <typename T, typename LayoutChecker>
void cxx_check_tensor_layout(void* tensor_cptr, T* data, size_t data_size, LayoutChecker layoutChecker) {
    typename eckit::linalg::Tensor<T>* tensor_ptr = static_cast<typename eckit::linalg::Tensor<T>*>(tensor_cptr);

    // expected data size
    ASSERT(tensor_ptr->size() == data_size);
    
    // first of all, assert that the data is correctly wrapped..
    for (int i=0; i<data_size; i++){
        ASSERT(*(tensor_ptr->data()+i) == *(data+i));
    }

    // check layout
    layoutChecker(data, tensor_ptr);
};

// (note this could be templetized further..)
// data checker rank 3
template <typename T>
void layout_check_rank_3(T* data, typename eckit::linalg::Tensor<T>* tensor_ptr) {
    if (tensor_ptr->layout() == eckit::linalg::Tensor<T>::Layout::RowMajor){ // row-major layout
        int count=0;
        for (int i=0; i<tensor_ptr->shape()[0]; i++){
            for (int j=0; j<tensor_ptr->shape()[1]; j++){
                for (int k=0; k<tensor_ptr->shape()[2]; k++){
                    ASSERT( (*tensor_ptr)(i,j,k) == *(data+count));
                    count++;
                }
            }
        }
    } else { // col-major layout
        int count=0;
        for (int i=0; i<tensor_ptr->shape()[2]; i++){
            for (int j=0; j<tensor_ptr->shape()[1]; j++){
                for (int k=0; k<tensor_ptr->shape()[0]; k++){
                    ASSERT( (*tensor_ptr)(k,j,i) == *(data+count));
                    count++;
                }
            }
        }
    }
};

// data checker rank 4
template <typename T>
void layout_check_rank_4(T* data, typename eckit::linalg::Tensor<T>* tensor_ptr) {
    if (tensor_ptr->layout() == eckit::linalg::Tensor<T>::Layout::RowMajor){ // row-major layout
        int count=0;
        for (int i=0; i<tensor_ptr->shape()[0]; i++){
            for (int j=0; j<tensor_ptr->shape()[1]; j++){
                for (int k=0; k<tensor_ptr->shape()[2]; k++)
                    for (int l=0; l<tensor_ptr->shape()[3]; l++){
                    ASSERT( (*tensor_ptr)(i,j,k,l) == *(data+count));
                    count++;
                }
            }
        }
    } else { // col-major layout
        int count=0;
        for (int i=0; i<tensor_ptr->shape()[3]; i++){
            for (int j=0; j<tensor_ptr->shape()[2]; j++){
                for (int k=0; k<tensor_ptr->shape()[1]; k++)
                    for (int l=0; l<tensor_ptr->shape()[0]; l++){
                    ASSERT( (*tensor_ptr)(l,k,j,i) == *(data+count));
                    count++;
                }
            }
        }
    }
};


template <typename T>
T* create_tensor_filled(int* shape, int shape_size, typename T::Layout layout) {    

    // shape
    ASSERT(shape_size>0);
    std::vector<size_t> shape_vec;
    for (int i=0; i<shape_size; i++) {
        ASSERT(*(shape+i) > 0);
        shape_vec.push_back(*(shape+i));
    }

    T* ptr = new T(shape_vec, layout);
    ptr->zero(); // zero-initialise it (to make sure it is writable)

    return ptr;
}


extern "C" {


// ---------- TensorFloat checks..

void cxx_check_tensor_float(void* tensor_cptr, size_t expected_size, size_t expected_rank, int layout) {
    return cxx_check_tensor<float>(tensor_cptr, expected_size, expected_rank, layout);
}

void cxx_check_tensor_float_layout_rank3(void* tensor_cptr, float* data, size_t data_size) {
    return cxx_check_tensor_layout<float>(
                tensor_cptr, 
                data, 
                data_size,
                layout_check_rank_3<float>
            );
}

void cxx_check_tensor_float_layout_rank4(void* tensor_cptr, float* data, size_t data_size) {
    return cxx_check_tensor_layout<float>(
                tensor_cptr, 
                data, 
                data_size,
                layout_check_rank_4<float>
            );
}

void* cxx_create_tensor_float_filled(int* shape, int shape_size, int layout) {    
    return create_tensor_filled<eckit::linalg::TensorFloat>(
                shape,
                shape_size,
                static_cast<eckit::linalg::TensorFloat::Layout>(layout)
            );
}

void cxx_delete_tensor_float(eckit::linalg::TensorFloat* ptr) {
    delete ptr;
}

// ---------- TensorDouble checks..

void cxx_check_tensor_double(void* tensor_cptr, size_t expected_size, size_t expected_rank, int layout) {
    return cxx_check_tensor<eckit::linalg::TensorDouble>(tensor_cptr, expected_size, expected_rank, layout);
}

void cxx_check_tensor_double_layout_rank3(void* tensor_cptr, double* data, size_t data_size) {
    return cxx_check_tensor_layout<double>(
                tensor_cptr, 
                data, 
                data_size, 
                layout_check_rank_3<double>
            );
}

void cxx_check_tensor_double_layout_rank4(void* tensor_cptr, double* data, size_t data_size) {
    return cxx_check_tensor_layout<double>(
                tensor_cptr, 
                data, 
                data_size, 
                layout_check_rank_4<double>
            );
}

void* cxx_create_tensor_double_filled(int* shape, int shape_size, int layout) {    
    return create_tensor_filled<eckit::linalg::TensorDouble>(
                shape,
                shape_size,
                static_cast<eckit::linalg::TensorDouble::Layout>(layout)
            );
}

void cxx_delete_tensor_double(eckit::linalg::TensorDouble* ptr) {
    delete ptr;
}

}