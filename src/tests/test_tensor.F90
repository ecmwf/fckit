#include "fckit/fctest.h"

! test fixture for f2c interop
module fcta_tensor_f_fxt

  use fckit_array_module, only: array_view1d

interface


! float tensor C++ checks..
subroutine cxx_check_tensor_float(cptr, esize, erank, layout) bind(c,name="cxx_check_tensor_float")
  use iso_c_binding, only : c_ptr, c_size_t, c_int
  type(c_ptr), value :: cptr
  integer(c_size_t), value :: esize
  integer(c_size_t), value :: erank
  integer(c_int), value :: layout
end subroutine

subroutine cxx_check_tensor_float_layout_r3(cptr, data, data_size) &
  & bind(c,name="cxx_check_tensor_float_layout_rank3")
  use iso_c_binding, only : c_ptr, c_float, c_size_t, c_int
  type(c_ptr), value :: cptr
  real(c_float), dimension(*) :: data
  integer(c_size_t), intent(in), value :: data_size
end subroutine

subroutine cxx_check_tensor_float_layout_r4(cptr, data, data_size) &
  & bind(c,name="cxx_check_tensor_float_layout_rank4")
  use iso_c_binding, only : c_ptr, c_float, c_size_t, c_int
  type(c_ptr), value :: cptr
  real(c_float), dimension(*) :: data
  integer(c_size_t), intent(in), value :: data_size
end subroutine

function cxx_create_tensor_float_filled(shape, shape_size, layout) &
  & bind(c,name="cxx_create_tensor_float_filled")
  use iso_c_binding, only : c_ptr, c_int
  integer(c_int), intent(in), dimension(*) :: shape
  integer(c_int), intent(in), value :: shape_size
  integer(c_int), intent(in), value :: layout
  type(c_ptr) :: cxx_create_tensor_float_filled
end function

subroutine cxx_delete_tensor_float(ptr) &
  & bind(c,name="cxx_delete_tensor_float")
  use iso_c_binding, only : c_ptr
  type(c_ptr), intent(in), value :: ptr
end subroutine


! double tensor C++ checks..
subroutine cxx_check_tensor_double(cptr, esize, erank, layout) bind(c,name="cxx_check_tensor_double")
  use iso_c_binding, only : c_ptr, c_size_t, c_int
  type(c_ptr), value :: cptr
  integer(c_size_t), value :: esize
  integer(c_size_t), value :: erank
  integer(c_int), value :: layout
end subroutine

subroutine cxx_check_tensor_double_layout_r3(cptr, data, data_size) &
  & bind(c,name="cxx_check_tensor_double_layout_rank3")
  use iso_c_binding, only : c_ptr, c_double, c_size_t, c_int
  type(c_ptr), value :: cptr
  real(c_double), dimension(*) :: data
  integer(c_size_t), intent(in), value :: data_size
end subroutine

subroutine cxx_check_tensor_double_layout_r4(cptr, data, data_size) &
  & bind(c,name="cxx_check_tensor_double_layout_rank4")
  use iso_c_binding, only : c_ptr, c_double, c_size_t, c_int
  type(c_ptr), value :: cptr
  real(c_double), dimension(*) :: data
  integer(c_size_t), intent(in), value :: data_size
end subroutine

function cxx_create_tensor_double_filled(shape, shape_size, layout) &
  & bind(c,name="cxx_create_tensor_double_filled")
  use iso_c_binding, only : c_ptr, c_int
  integer(c_int), intent(in), dimension(*) :: shape
  integer(c_int), intent(in), value :: shape_size
  integer(c_int), intent(in), value :: layout
  type(c_ptr) :: cxx_create_tensor_double_filled
end function

subroutine cxx_delete_tensor_double(ptr) &
  & bind(c,name="cxx_delete_tensor_double")
  use iso_c_binding, only : c_ptr
  type(c_ptr), intent(in), value :: ptr
end subroutine

end interface

contains

! float tensor checks..
subroutine check_tensor_float(cptr, esize, erank, layout)
  use iso_c_binding, only : c_ptr, c_size_t, c_int
  type(c_ptr) :: cptr
  integer(c_size_t) :: esize
  integer(c_size_t) :: erank
  integer(c_int) :: layout
  call cxx_check_tensor_float(cptr, esize, erank, layout)
end subroutine

subroutine check_tensor_float_layout_r3(cptr, data)
  use iso_c_binding, only : c_ptr, c_float, c_size_t, c_int
  type(c_ptr), value :: cptr
  real(c_float), intent(in), target :: data(:,:,:)
  real(c_float), pointer :: data_vec(:)
  integer(c_size_t)  :: shape_vec(3)
  integer(c_size_t)  :: data_rank
  integer(c_size_t)  :: data_size 
  data_vec => array_view1d( data )
  shape_vec = shape(data)
  data_rank = size(shape_vec)
  data_size = size(data)
  call cxx_check_tensor_float_layout_r3(cptr, data_vec, data_size )
end subroutine

subroutine check_tensor_float_layout_r4(cptr, data)
  use iso_c_binding, only : c_ptr, c_float, c_size_t, c_int
  type(c_ptr), value :: cptr
  real(c_float), intent(in), target :: data(:,:,:,:)
  real(c_float), pointer :: data_vec(:)
  integer(c_size_t)  :: shape_vec(4)
  integer(c_size_t)  :: data_rank
  integer(c_size_t)  :: data_size
  data_vec => array_view1d( data )
  shape_vec = shape(data)
  data_rank = size(shape_vec)
  data_size = size(data)
  call cxx_check_tensor_float_layout_r4(cptr, data_vec, data_size )
end subroutine

function create_tensor_float_filled(shape, shape_size, layout)
  use iso_c_binding, only : c_ptr, c_int
  integer(c_int), intent(in), dimension(*) :: shape
  integer(c_int), intent(in), value :: shape_size
  integer(c_int), intent(in), value :: layout
  type(c_ptr) :: create_tensor_float_filled
  create_tensor_float_filled = cxx_create_tensor_float_filled(shape, shape_size, layout)
end function

subroutine delete_tensor_float(ptr)
  use iso_c_binding, only : c_ptr
  type(c_ptr), intent(in) :: ptr
  call cxx_delete_tensor_float(ptr)
end subroutine


! double tensor checks..
subroutine check_tensor_double(cptr, esize, erank, layout)
  use iso_c_binding, only : c_ptr, c_size_t, c_int
  type(c_ptr) :: cptr
  integer(c_size_t) :: esize
  integer(c_size_t) :: erank
  integer(c_int) :: layout
  call cxx_check_tensor_double(cptr, esize, erank, layout)
end subroutine

subroutine check_tensor_double_layout_r3(cptr, data)
  use iso_c_binding, only : c_ptr, c_double, c_size_t, c_int
  type(c_ptr), value :: cptr
  real(c_double), intent(in), target :: data(:,:,:)
  real(c_double), pointer :: data_vec(:)
  integer(c_size_t)  :: shape_vec(3)
  integer(c_size_t)  :: data_rank
  integer(c_size_t)  :: data_size
  data_vec => array_view1d( data )
  shape_vec = shape(data)
  data_rank = size(shape_vec)
  data_size = size(data)
  call cxx_check_tensor_double_layout_r3(cptr, data_vec, data_size )
end subroutine

subroutine check_tensor_double_layout_r4(cptr, data)
  use iso_c_binding, only : c_ptr, c_double, c_size_t, c_int
  type(c_ptr), value :: cptr
  real(c_double), intent(in), target :: data(:,:,:,:)
  real(c_double), pointer :: data_vec(:)
  integer(c_size_t)  :: shape_vec(4)
  integer(c_size_t)  :: data_rank
  integer(c_size_t)  :: data_size 
  data_vec => array_view1d( data )
  shape_vec = shape(data)
  data_rank = size(shape_vec)
  data_size = size(data)
  call cxx_check_tensor_double_layout_r4(cptr, data_vec, data_size )
end subroutine

function create_tensor_double_filled(shape, shape_size, layout)
  use iso_c_binding, only : c_ptr, c_int
  integer(c_int), intent(in), dimension(*) :: shape
  integer(c_int), intent(in), value :: shape_size
  integer(c_int), intent(in), value :: layout
  type(c_ptr) :: create_tensor_double_filled
  create_tensor_double_filled = cxx_create_tensor_double_filled(shape, shape_size, layout)
end function

subroutine delete_tensor_double(ptr)
  use iso_c_binding, only : c_ptr
  type(c_ptr), intent(in) :: ptr
  call cxx_delete_tensor_double(ptr)
end subroutine


end module fcta_tensor_f_fxt

! -----------------------------------------------------------------------------

TESTSUITE_WITH_FIXTURE(fctest_test_tensor_suite, fcta_tensor_f_fxt)

! -----------------------------------------------------------------------------

TESTSUITE_INIT
  use fckit_module
  call fckit_main%init()
END_TESTSUITE_INIT

! -----------------------------------------------------------------------------

TESTSUITE_FINALIZE
  use fckit_module
  call fckit_main%final()
END_TESTSUITE_FINALIZE

! -----------------------------------------------------------------------------


! ============= TensorFloat tests =============
TEST( test_float_tensor_creation )
#if 1

use iso_c_binding, only : c_float, c_size_t
use fckit_tensor_module, only : fckit_tensor_real32

type(fckit_tensor_real32) :: tensor_empty
type(fckit_tensor_real32) :: tensor_from_shape
type(fckit_tensor_real32) :: tensor_rank1
type(fckit_tensor_real32) :: tensor_rank2
type(fckit_tensor_real32) :: tensor_rank3
type(fckit_tensor_real32) :: tensor_rank4

integer(c_size_t)  :: shape(4) = (/ 4,2,3,4 /)
real(c_float)   :: data_rank1(10) = 0.0
real(c_float)   :: data_rank2(3,4)
real(c_float)   :: data_rank3(3,3,3)
real(c_float)   :: data_rank4(2,3,4,5)

integer(c_size_t) :: tensor_size
integer(c_size_t) :: rank
integer(c_size_t), allocatable :: tensor_shape(:)

! an empty tensor
tensor_empty = fckit_tensor_real32()

! a tensor from shape
tensor_from_shape = fckit_tensor_real32(shape)

! a tensor from an array
tensor_rank1 = fckit_tensor_real32(data_rank1)

! a tensor from an array rank 2
data_rank2(1,:) = 0.0
data_rank2(2,:) = 1.0
data_rank2(3,:) = 2.0
tensor_rank2 = fckit_tensor_real32(data_rank2)

! a tensor from an array rank 3
data_rank3 = 0.0
tensor_rank3 = fckit_tensor_real32(data_rank3)

! a tensor from an array rank 4
data_rank4 = 0.0
tensor_rank4 = fckit_tensor_real32(data_rank4)


! check rank1 - tensor size/shape
tensor_size = tensor_rank1%size()
rank = tensor_rank1%rank()
tensor_shape = tensor_rank1%shape()
FCTEST_CHECK_EQUAL( tensor_size , 10 )
FCTEST_CHECK_EQUAL( size(tensor_shape) , 1 )
FCTEST_CHECK_EQUAL( tensor_shape(1) , 10 )
deallocate(tensor_shape)

! check rank2 - tensor size/shape
tensor_size = tensor_rank2%size()
rank = tensor_rank2%rank()
tensor_shape = tensor_rank2%shape()
FCTEST_CHECK_EQUAL( tensor_size , 3*4 )
FCTEST_CHECK_EQUAL( size(tensor_shape) , 2 )
FCTEST_CHECK_EQUAL( tensor_shape(1) , 3 )
FCTEST_CHECK_EQUAL( tensor_shape(2) , 4 )
deallocate(tensor_shape)

! check rank3 - tensor size/shape
tensor_size = tensor_rank3%size()
rank = tensor_rank3%rank()
tensor_shape = tensor_rank3%shape()
FCTEST_CHECK_EQUAL( tensor_size , 3*3*3 )
FCTEST_CHECK_EQUAL( size(tensor_shape) , 3 )
FCTEST_CHECK_EQUAL( tensor_shape(1) , 3 )
FCTEST_CHECK_EQUAL( tensor_shape(2) , 3 )
FCTEST_CHECK_EQUAL( tensor_shape(3) , 3 )
deallocate(tensor_shape)

! check rank4 - tensor size/shape
tensor_size = tensor_rank4%size()
rank = tensor_rank3%rank()
tensor_shape = tensor_rank4%shape()
FCTEST_CHECK_EQUAL( tensor_size , 2*3*4*5 )
FCTEST_CHECK_EQUAL( size(tensor_shape) , 4 )
FCTEST_CHECK_EQUAL( tensor_shape(1) , 2 )
FCTEST_CHECK_EQUAL( tensor_shape(2) , 3 )
FCTEST_CHECK_EQUAL( tensor_shape(3) , 4 )
FCTEST_CHECK_EQUAL( tensor_shape(4) , 5 )
deallocate(tensor_shape)


! finalise
call tensor_empty%final()
call tensor_from_shape%final()
call tensor_rank1%final()
call tensor_rank2%final()
call tensor_rank3%final()
call tensor_rank4%final()


#else
#warning Test "test_float_tensor_creation" disabled
#endif
END_TEST



TEST( test_tensor_float_interop )
#if 1

use iso_c_binding, only : c_float, c_size_t
use fckit_tensor_module, only : fckit_tensor_real32, fckit_tensor

type(fckit_tensor_real32) :: tensor_r3
type(fckit_tensor_real32) :: tensor_r4
real(c_float) :: data_r3(3,3,3)
real(c_float) :: data_r4(2,4,5,6)
integer(c_size_t) :: expected_size
integer(c_size_t) :: expected_rank

integer(c_int) :: expected_layout

! ---------------------- rank 3
data_r3(1,:,:) = 0.04
data_r3(2,:,:) = 1.04
data_r3(3,:,:) = 2.04
tensor_r3 = fckit_tensor_real32(data_r3, fckit_tensor%layout_rowmajor())

expected_size = 3*3*3
expected_rank = 3
expected_layout = fckit_tensor%layout_rowmajor()

! check tensor info from c++
call check_tensor_float(tensor_r3%c_ptr(), expected_size, expected_rank, expected_layout)

! check internal data.. 
call check_tensor_float_layout_r3(tensor_r3%c_ptr(), data_r3)


! ---------------------- rank 4
data_r4(1,:,:,:) = 1.14
data_r4(2,:,:,:) = 2.24
data_r4(:,:,:,6) = 3.34
tensor_r4 = fckit_tensor_real32(data_r4) ! layout is colmajor (by default)

expected_size = 2*4*5*6
expected_rank = 4
expected_layout = fckit_tensor%layout_colmajor()

! check tensor info from c++
call check_tensor_float(tensor_r4%c_ptr(), expected_size, expected_rank, expected_layout)

! check internal data.. 
call check_tensor_float_layout_r4(tensor_r4%c_ptr(), data_r4)


#else
#warning Test "test_tensor_float_interop" disabled
#endif
END_TEST
  

TEST ( test_tensor_float_creation_from_cpp )
#if 1

use iso_c_binding, only : c_ptr, c_int, c_size_t
use fckit_tensor_module, only : fckit_tensor_real32, fckit_tensor

type(fckit_tensor_real32) :: tensor

integer(c_int) :: tensor_shape_input(4) = (/ 4,2,3,4 /)
integer(c_size_t), allocatable :: tensor_shape(:)
integer(c_size_t) :: expected_size = 4*2*3*4
integer(c_size_t) :: expected_rank = 4
integer(c_size_t) :: expected_shape(4) = (/ 4,2,3,4 /)

call tensor%reset_c_ptr(create_tensor_float_filled(tensor_shape_input, size(tensor_shape_input), fckit_tensor%layout_colmajor()))

FCTEST_CHECK_EQUAL( tensor%size(), expected_size )
FCTEST_CHECK_EQUAL( tensor%rank(), expected_rank )

tensor_shape = tensor%shape()

FCTEST_CHECK_EQUAL( tensor_shape(1), expected_shape(1) )
FCTEST_CHECK_EQUAL( tensor_shape(2), expected_shape(2) )
FCTEST_CHECK_EQUAL( tensor_shape(3), expected_shape(3) )
FCTEST_CHECK_EQUAL( tensor_shape(4), expected_shape(4) )

FCTEST_CHECK_EQUAL( tensor%layout(), fckit_tensor%layout_colmajor() )

! deallocation
deallocate(tensor_shape)
call delete_tensor_float(tensor%c_ptr())

#else
#warning Test "test_tensor_float_creation_from_cpp" disabled
#endif
END_TEST


! ============= TensorDouble tests =============
TEST( test_double_tensor_creation )
#if 1

use iso_c_binding, only : c_double, c_size_t
use fckit_tensor_module, only : fckit_tensor_real64

type(fckit_tensor_real64) :: tensor_empty
type(fckit_tensor_real64) :: tensor_from_shape
type(fckit_tensor_real64) :: tensor_rank1
type(fckit_tensor_real64) :: tensor_rank2
type(fckit_tensor_real64) :: tensor_rank3
type(fckit_tensor_real64) :: tensor_rank4

integer(c_size_t)  :: shape(4) = (/ 4,2,3,4 /)
real(c_double)   :: data_rank1(10) = 0.0
real(c_double)   :: data_rank2(3,4)
real(c_double)   :: data_rank3(3,3,3)
real(c_double)   :: data_rank4(2,3,4,5)

integer(c_size_t) :: tensor_size
integer(c_size_t) :: rank
integer(c_size_t), allocatable :: tensor_shape(:)

! an empty tensor
tensor_empty = fckit_tensor_real64()

! a tensor from shape
tensor_from_shape = fckit_tensor_real64(shape)

! a tensor from an array
tensor_rank1 = fckit_tensor_real64(data_rank1)

! a tensor from an array rank 2
data_rank2(1,:) = 0.06
data_rank2(2,:) = 1.06
data_rank2(3,:) = 2.06
tensor_rank2 = fckit_tensor_real64(data_rank2)

! a tensor from an array rank 3
data_rank3 = 0.0
tensor_rank3 = fckit_tensor_real64(data_rank3)

! a tensor from an array rank 4
data_rank4 = 0.0
tensor_rank4 = fckit_tensor_real64(data_rank4)


! check rank1 - tensor size/shape
tensor_size = tensor_rank1%size()
rank = tensor_rank1%rank()
tensor_shape = tensor_rank1%shape()
FCTEST_CHECK_EQUAL( tensor_size , 10 )
FCTEST_CHECK_EQUAL( size(tensor_shape) , 1 )
FCTEST_CHECK_EQUAL( tensor_shape(1) , 10 )
deallocate(tensor_shape)

! check rank2 - tensor size/shape
tensor_size = tensor_rank2%size()
rank = tensor_rank2%rank()
tensor_shape = tensor_rank2%shape()
FCTEST_CHECK_EQUAL( tensor_size , 3*4 )
FCTEST_CHECK_EQUAL( size(tensor_shape) , 2 )
FCTEST_CHECK_EQUAL( tensor_shape(1) , 3 )
FCTEST_CHECK_EQUAL( tensor_shape(2) , 4 )
deallocate(tensor_shape)

! check rank3 - tensor size/shape
tensor_size = tensor_rank3%size()
rank = tensor_rank3%rank()
tensor_shape = tensor_rank3%shape()
FCTEST_CHECK_EQUAL( tensor_size , 3*3*3 )
FCTEST_CHECK_EQUAL( size(tensor_shape) , 3 )
FCTEST_CHECK_EQUAL( tensor_shape(1) , 3 )
FCTEST_CHECK_EQUAL( tensor_shape(2) , 3 )
FCTEST_CHECK_EQUAL( tensor_shape(3) , 3 )
deallocate(tensor_shape)

! check rank4 - tensor size/shape
tensor_size = tensor_rank4%size()
rank = tensor_rank3%rank()
! allocate(tensor_shape(rank))
tensor_shape = tensor_rank4%shape()
FCTEST_CHECK_EQUAL( tensor_size , 2*3*4*5 )
FCTEST_CHECK_EQUAL( size(tensor_shape) , 4 )
FCTEST_CHECK_EQUAL( tensor_shape(1) , 2 )
FCTEST_CHECK_EQUAL( tensor_shape(2) , 3 )
FCTEST_CHECK_EQUAL( tensor_shape(3) , 4 )
FCTEST_CHECK_EQUAL( tensor_shape(4) , 5 )
deallocate(tensor_shape)


! finalise
call tensor_empty%final()
call tensor_from_shape%final()
call tensor_rank1%final()
call tensor_rank2%final()
call tensor_rank3%final()
call tensor_rank4%final()


#else
#warning Test "test_double_tensor_creation" disabled
#endif
END_TEST



TEST( test_tensor_double_interop )
#if 1

use iso_c_binding, only : c_double, c_size_t
use fckit_tensor_module, only : fckit_tensor_real64, fckit_tensor

type(fckit_tensor_real64) :: tensor_r3
type(fckit_tensor_real64) :: tensor_r4
real(c_double) :: data_r3(3,3,3)
real(c_double) :: data_r4(2,4,5,6)

integer(c_size_t) :: expected_size
integer(c_size_t) :: expected_rank
integer(c_int) :: expected_layout

! ---------------------- rank 3
data_r3(1,:,:) = 0.06
data_r3(2,:,:) = 1.06
data_r3(3,:,:) = 2.06
tensor_r3 = fckit_tensor_real64(data_r3, fckit_tensor%layout_rowmajor())

expected_size = 3*3*3
expected_rank = 3
expected_layout = fckit_tensor%layout_rowmajor()

! check tensor info from c++
call check_tensor_double(tensor_r3%c_ptr(), expected_size, expected_rank, expected_layout)

! check internal data.. 
call check_tensor_double_layout_r3(tensor_r3%c_ptr(), data_r3)


! ---------------------- rank 4
data_r4(1,:,:,:) = 1.16
data_r4(2,:,:,:) = 2.26
data_r4(:,:,:,6) = 3.36
tensor_r4 = fckit_tensor_real64(data_r4)  ! layout is colmajor (by default)

expected_size = 2*4*5*6
expected_rank = 4
expected_layout = fckit_tensor%layout_colmajor()

! check tensor info from c++
call check_tensor_double(tensor_r4%c_ptr(), expected_size, expected_rank, expected_layout)

! check internal data.. 
call check_tensor_double_layout_r4(tensor_r4%c_ptr(), data_r4)


#else
#warning Test "test_tensor_double_interop" disabled
#endif
END_TEST



TEST ( test_tensor_double_creation_from_cpp )
#if 1

use iso_c_binding, only : c_ptr, c_int, c_size_t
use fckit_tensor_module, only : fckit_tensor_real64, fckit_tensor

type(fckit_tensor_real64) :: tensor

integer(c_int) :: tensor_shape_input(4) = (/ 4,2,3,4 /)
integer(c_size_t), allocatable :: tensor_shape(:)
integer(c_size_t) :: expected_size = 4*2*3*4
integer(c_size_t) :: expected_rank = 4
integer(c_size_t) :: expected_shape(4) = (/ 4,2,3,4 /)

call tensor%reset_c_ptr(create_tensor_double_filled(tensor_shape_input, size(tensor_shape_input), fckit_tensor%layout_colmajor()))

FCTEST_CHECK_EQUAL( tensor%size(), expected_size )
FCTEST_CHECK_EQUAL( tensor%rank(), expected_rank )


tensor_shape = tensor%shape()

FCTEST_CHECK_EQUAL( tensor_shape(1), expected_shape(1) )
FCTEST_CHECK_EQUAL( tensor_shape(2), expected_shape(2) )
FCTEST_CHECK_EQUAL( tensor_shape(3), expected_shape(3) )
FCTEST_CHECK_EQUAL( tensor_shape(4), expected_shape(4) )

FCTEST_CHECK_EQUAL( tensor%layout(), fckit_tensor%layout_colmajor() )

! deallocation
deallocate(tensor_shape)
call delete_tensor_double(tensor%c_ptr())

#else
#warning Test "test_tensor_double_creation_from_cpp" disabled
#endif
END_TEST


END_TESTSUITE
