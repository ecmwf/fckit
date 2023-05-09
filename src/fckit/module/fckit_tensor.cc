#include <vector>
#include <algorithm>

#include "fckit/fckit.h"

#include "eckit/linalg/Tensor.h"


namespace fckit {
extern "C" {


using eckit::linalg::TensorFloat;
using eckit::linalg::TensorDouble;


// ------ layout codes
int c_fckit_tensor_layout_right() {
    return static_cast<int>(eckit::linalg::TensorFloat::Layout::Right);
}

int c_fckit_tensor_layout_left() {
    return static_cast<int>(eckit::linalg::TensorFloat::Layout::Left);
}

int c_fckit_tensor_layout_rowmajor() {
    return static_cast<int>(eckit::linalg::TensorFloat::Layout::RowMajor);
}

int c_fckit_tensor_layout_colmajor() {
    return static_cast<int>(eckit::linalg::TensorFloat::Layout::ColMajor);
}


// ======================================================
//                  fckit_tensor_real32
// ======================================================


TensorFloat* c_fckit_tensor_real32_empty_new(int layout) {
    return new TensorFloat(static_cast<TensorFloat::Layout>(layout));
}

TensorFloat* c_fckit_tensor_real32_from_shape_new(size_t rank, size_t* shape, int layout) {
    std::vector<eckit::linalg::Size> shapeVec(rank);
    shapeVec.assign(shape, shape + rank);
    return new TensorFloat(shapeVec, static_cast<TensorFloat::Layout>(layout));
}

TensorFloat* c_fckit_tensor_real32_from_array_rank1_new(size_t rank, size_t* shape, float* data_vec, int layout) {
    std::vector<eckit::linalg::Size> shapeVec;
    shapeVec.assign(shape, shape + rank);
    return new TensorFloat(data_vec, shapeVec, static_cast<TensorFloat::Layout>(layout));
}

void c_fckit_tensor_real32_delete(TensorFloat* h) {
    ASSERT(h);
    delete h;
    h = nullptr;
}

size_t c_fckit_tensor_real32_size(TensorFloat* h) {
    ASSERT(h);
    return h->size();
}

size_t c_fckit_tensor_real32_rank(TensorFloat* h) {
    ASSERT(h);
    return h->shape().size();
}

void c_fckit_tensor_real32_shape(TensorFloat* h, size_t*& shape_cptr, size_t& rank) {
    ASSERT(h);
    rank = h->shape().size();
    shape_cptr = new size_t[rank];
    std::vector<size_t> tensor_shape = h->shape();
    std::copy(tensor_shape.begin(), tensor_shape.end(), shape_cptr);
}

void c_fckit_tensor_real32_fill(TensorFloat* h, float val) {
    ASSERT(h);
    h->fill(val);
}

void c_fckit_tensor_real32_zero(TensorFloat* h) {
    ASSERT(h);
    h->zero();
}

int c_fckit_tensor_real32_layout(TensorFloat* h) {
    ASSERT(h);
    return static_cast<int>(h->layout());
}


// ======================================================
//                  fckit_tensor_real64
// ======================================================

TensorDouble* c_fckit_tensor_real64_empty_new(int layout) {
    return new TensorDouble(static_cast<TensorDouble::Layout>(layout));
}

TensorDouble* c_fckit_tensor_real64_from_shape_new(size_t rank, size_t* shape, int layout) {
    std::vector<eckit::linalg::Size> shapeVec(rank);
    shapeVec.assign(shape, shape + rank);
    return new TensorDouble(shapeVec, static_cast<TensorDouble::Layout>(layout));
}

TensorDouble* c_fckit_tensor_real64_from_array_rank1_new(size_t rank, size_t* shape, double* data_vec, int layout) {
    std::vector<eckit::linalg::Size> shapeVec;
    shapeVec.assign(shape, shape + rank);
    return new TensorDouble(data_vec, shapeVec, static_cast<TensorDouble::Layout>(layout));
}

void c_fckit_tensor_real64_delete(TensorDouble* h) {
    ASSERT(h);
    delete h;
    h = nullptr;
}

size_t c_fckit_tensor_real64_size(TensorDouble* h) {
    ASSERT(h);
    return h->size();
}

size_t c_fckit_tensor_real64_rank(TensorDouble* h) {
    ASSERT(h);
    return h->shape().size();
}

void c_fckit_tensor_real64_shape(TensorDouble* h, size_t*& shape_cptr, size_t& rank) {
    ASSERT(h);
    rank = h->shape().size();
    shape_cptr = new size_t[rank];
    std::vector<size_t> tensor_shape = h->shape();
    std::copy(tensor_shape.begin(), tensor_shape.end(), shape_cptr);
}

void c_fckit_tensor_real64_fill(TensorDouble* h, double val) {
    ASSERT(h);
    h->fill(val);
}

void c_fckit_tensor_real64_zero(TensorDouble* h) {
    ASSERT(h);
    h->zero();
}

int c_fckit_tensor_real64_layout(TensorDouble* h) {
    ASSERT(h);
    return static_cast<int>(h->layout());
}


} // extern "C"
} // fckit namespace