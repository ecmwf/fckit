#ifndef FORD


interface


! --- tensor layouts ---
function c_fckit_tensor_layout_right() bind(C,name="c_fckit_tensor_layout_right")
  use, intrinsic :: iso_c_binding, only : c_int
  integer(c_int) :: c_fckit_tensor_layout_right
end function

function c_fckit_tensor_layout_left() bind(C,name="c_fckit_tensor_layout_left")
  use, intrinsic :: iso_c_binding, only : c_int
  integer(c_int) :: c_fckit_tensor_layout_left
end function

function c_fckit_tensor_layout_rowmajor() bind(C,name="c_fckit_tensor_layout_rowmajor")
  use, intrinsic :: iso_c_binding, only : c_int
  integer(c_int) :: c_fckit_tensor_layout_rowmajor
end function

function c_fckit_tensor_layout_colmajor() bind(C,name="c_fckit_tensor_layout_colmajor")
  use, intrinsic :: iso_c_binding, only : c_int
  integer(c_int) :: c_fckit_tensor_layout_colmajor
end function


! ======================================================
!                  fckit_tensor_real32
! ======================================================


!-------------------------------------------------------------------------------
! TensorFloat* c_fckit_tensor_real32_empty_new (int layout)
!-------------------------------------------------------------------------------
function c_fckit_tensor_real32_empty_new(layout) bind(C,name="c_fckit_tensor_real32_empty_new")
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int), value       :: layout
    type(c_ptr) :: c_fckit_tensor_real32_empty_new
end function
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! TensorFloat* c_fckit_tensor_real32_from_shape_new(size_t rank, size_t* shape, int layout)
!-------------------------------------------------------------------------------
function c_fckit_tensor_real32_from_shape_new(rank, shape, layout) &
& bind(C,name="c_fckit_tensor_real32_from_shape_new")
    use iso_c_binding, only: c_ptr, c_size_t, c_int
    integer(c_size_t), value        :: rank
    integer(c_size_t), dimension(*) :: shape
    integer(c_int), value        :: layout
    type(c_ptr)                  :: c_fckit_tensor_real32_from_shape_new    
end function
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! TensorFloat* c_fckit_tensor_real32_from_array_rank1_new(size_t rank, size_t* shape, float* data_vec, int layout)
!-------------------------------------------------------------------------------
function c_fckit_tensor_real32_from_array_rank1_new(rank, shape, data_vec, layout) &
& bind(C,name="c_fckit_tensor_real32_from_array_rank1_new")
    use iso_c_binding, only: c_ptr, c_size_t, c_float, c_int
    integer(c_size_t), value        :: rank
    integer(c_size_t), dimension(*) :: shape
    real(c_float), dimension(*)  :: data_vec
    integer(c_int), value        :: layout
    type(c_ptr)                  :: c_fckit_tensor_real32_from_array_rank1_new    
end function
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_tensor_real32_delete (TensorFloat* this)
!-------------------------------------------------------------------------------
subroutine c_fckit_tensor_real32_delete( this ) bind(C,name="c_fckit_tensor_real32_delete")
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: this
end subroutine
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! size_t c_fckit_tensor_real32_size (TensorFloat* this)
!-------------------------------------------------------------------------------
function c_fckit_tensor_real32_size( this ) bind(C,name="c_fckit_tensor_real32_size")
    use iso_c_binding, only: c_ptr, c_size_t
    type(c_ptr), intent(in), value :: this
    integer(c_size_t) :: c_fckit_tensor_real32_size
end function
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! size_t c_fckit_tensor_real32_rank (TensorFloat* this)
!-------------------------------------------------------------------------------
function c_fckit_tensor_real32_rank( this ) bind(C,name="c_fckit_tensor_real32_rank")
    use iso_c_binding, only: c_ptr, c_size_t
    type(c_ptr), intent(in), value :: this
    integer(c_size_t) :: c_fckit_tensor_real32_rank
end function
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_tensor_real32_shape(TensorFloat* this, size_t*& shape_cptr, size_t& rank) 
!-------------------------------------------------------------------------------
subroutine c_fckit_tensor_real32_shape( this, shape_cptr, rank ) bind(C,name="c_fckit_tensor_real32_shape")
    use iso_c_binding, only: c_ptr, c_size_t
    type(c_ptr),        intent(in), value  :: this
    type(c_ptr)                            :: shape_cptr
    integer(c_size_t)                      :: rank
end subroutine
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_tensor_real32_fill (TensorFloat* this)
!-------------------------------------------------------------------------------
subroutine c_fckit_tensor_real32_fill( this, val ) bind(C,name="c_fckit_tensor_real32_fill")
    use iso_c_binding, only: c_ptr, c_float
    type(c_ptr), intent(in), value :: this
    real(c_float), intent(in), value :: val
end subroutine
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_tensor_real32_zero (TensorFloat* this)
!-------------------------------------------------------------------------------
subroutine c_fckit_tensor_real32_zero( this ) bind(C,name="c_fckit_tensor_real32_zero")
    use iso_c_binding, only: c_ptr
    type(c_ptr), intent(in), value :: this
end subroutine
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! int c_fckit_tensor_real32_layout (TensorFloat* this)
!-------------------------------------------------------------------------------
function c_fckit_tensor_real32_layout( this ) bind(C,name="c_fckit_tensor_real32_layout")
    use iso_c_binding, only: c_ptr, c_int
    type(c_ptr), intent(in), value :: this
    integer(c_int) :: c_fckit_tensor_real32_layout
end function
!-------------------------------------------------------------------------------




! ======================================================
!                  fckit_tensor_real64
! ======================================================


!-------------------------------------------------------------------------------
! TensorDouble* c_fckit_tensor_real64_empty_new (int layout)
!-------------------------------------------------------------------------------
function c_fckit_tensor_real64_empty_new(layout) bind(C,name="c_fckit_tensor_real64_empty_new")
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int), value        :: layout
    type(c_ptr) :: c_fckit_tensor_real64_empty_new
end function
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! TensorDouble* c_fckit_tensor_real64_from_shape_new(size_t rank, size_t* shape, int layout)
!-------------------------------------------------------------------------------
function c_fckit_tensor_real64_from_shape_new(rank, shape, layout) &
& bind(C,name="c_fckit_tensor_real64_from_shape_new")
    use iso_c_binding, only: c_ptr, c_size_t, c_int
    integer(c_size_t), value        :: rank
    integer(c_size_t), dimension(*) :: shape
    integer(c_int), value        :: layout
    type(c_ptr)                  :: c_fckit_tensor_real64_from_shape_new    
end function
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! TensorDouble* c_fckit_tensor_real64_from_array_rank1_new(size_t rank, size_t* shape, double* data_vec, int layout)
!-------------------------------------------------------------------------------
function c_fckit_tensor_real64_from_array_rank1_new(rank, shape, data_vec, layout) &
& bind(C,name="c_fckit_tensor_real64_from_array_rank1_new")
    use iso_c_binding, only: c_ptr, c_size_t, c_double, c_int
    integer(c_size_t), value        :: rank
    integer(c_size_t), dimension(*) :: shape
    real(c_double), dimension(*)  :: data_vec
    integer(c_int), value        :: layout
    type(c_ptr)                  :: c_fckit_tensor_real64_from_array_rank1_new    
end function
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_tensor_real64_delete (TensorDouble* this)
!-------------------------------------------------------------------------------
subroutine c_fckit_tensor_real64_delete( this ) bind(C,name="c_fckit_tensor_real64_delete")
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: this
end subroutine
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! size_t c_fckit_tensor_real64_size (TensorDouble* this)
!-------------------------------------------------------------------------------
function c_fckit_tensor_real64_size( this ) bind(C,name="c_fckit_tensor_real64_size")
    use iso_c_binding, only: c_ptr, c_size_t
    type(c_ptr), intent(in), value :: this
    integer(c_size_t) :: c_fckit_tensor_real64_size
end function
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! size_t c_fckit_tensor_real64_rank (TensorDouble* this)
!-------------------------------------------------------------------------------
function c_fckit_tensor_real64_rank( this ) bind(C,name="c_fckit_tensor_real64_rank")
    use iso_c_binding, only: c_ptr, c_size_t
    type(c_ptr), intent(in), value :: this
    integer(c_size_t) :: c_fckit_tensor_real64_rank
end function
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_tensor_real64_shape(TensorDouble* this, size_t*& shape_cptr, size_t& rank) 
!-------------------------------------------------------------------------------
subroutine c_fckit_tensor_real64_shape( this, shape_cptr, rank ) bind(C,name="c_fckit_tensor_real64_shape")
    use iso_c_binding, only: c_ptr, c_size_t
    type(c_ptr),        intent(in), value  :: this
    type(c_ptr)                            :: shape_cptr
    integer(c_size_t)                     :: rank
end subroutine
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_tensor_real64_fill (TensorDouble* this)
!-------------------------------------------------------------------------------
subroutine c_fckit_tensor_real64_fill( this, val ) bind(C,name="c_fckit_tensor_real64_fill")
    use iso_c_binding, only: c_ptr, c_double
    type(c_ptr), intent(in), value :: this
    real(c_double), intent(in), value :: val
end subroutine
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_tensor_real64_zero (TensorDouble* this)
!-------------------------------------------------------------------------------
subroutine c_fckit_tensor_real64_zero( this ) bind(C,name="c_fckit_tensor_real64_zero")
    use iso_c_binding, only: c_ptr
    type(c_ptr), intent(in), value :: this
end subroutine
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! int c_fckit_tensor_real64_layout (TensorDouble* this)
!-------------------------------------------------------------------------------
function c_fckit_tensor_real64_layout( this ) bind(C,name="c_fckit_tensor_real64_layout")
    use iso_c_binding, only: c_ptr, c_int
    type(c_ptr), intent(in), value :: this
    integer(c_int) :: c_fckit_tensor_real64_layout
end function
!-------------------------------------------------------------------------------


end interface

#endif