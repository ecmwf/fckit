#include "fckit/fckit.h"

module fckit_tensor_module

use iso_c_binding, only : c_int, &
                          c_ptr, &
                          c_size_t, &
                          c_float, &
                          c_double, &
                          c_f_pointer

use fckit_array_module, only: array_view1d
use fckit_c_interop_module, only : c_ptr_free
use fckit_shared_object_module, only : fckit_shared_object, &
                                       fckit_c_deleter, &
                                       fckit_c_nodeleter

implicit none

public :: fckit_tensor_real32
public :: fckit_tensor_real64

private

#include "fckit_tensor.inc"



! ======================================================
!                   fckit_tensor_real32
! ======================================================
type, extends(fckit_shared_object) :: fckit_tensor_real32
contains
  procedure         :: size     => fckit_tensor_real32__size
  procedure         :: rank     => fckit_tensor_real32__rank
  procedure         :: shape    => fckit_tensor_real32__shape
  procedure         :: fill     => fckit_tensor_real32__fill
  procedure         :: zero     => fckit_tensor_real32__zero
  procedure         :: layout   => fckit_tensor_real32__layout

  ! tensor layouts
  procedure, nopass :: layout_right    => fckit_tensor_layout_right
  procedure, nopass :: layout_left     => fckit_tensor_layout_left
  procedure, nopass :: layout_rowmajor => fckit_tensor_layout_rowmajor
  procedure, nopass :: layout_colmajor => fckit_tensor_layout_colmajor

#if FCKIT_FINAL_NOT_INHERITING
  final :: fckit_tensor_real32__final_auto
#endif
end type fckit_tensor_real32


! ======================================================
!                  fckit_tensor_real64
! ======================================================
type, extends(fckit_shared_object) :: fckit_tensor_real64
contains
  procedure         :: size     => fckit_tensor_real64__size
  procedure         :: rank     => fckit_tensor_real64__rank
  procedure         :: shape    => fckit_tensor_real64__shape
  procedure         :: fill     => fckit_tensor_real64__fill
  procedure         :: zero     => fckit_tensor_real64__zero
  procedure         :: layout   => fckit_tensor_real64__layout

  ! tensor layouts
  procedure, nopass :: layout_right    => fckit_tensor_layout_right
  procedure, nopass :: layout_left     => fckit_tensor_layout_left
  procedure, nopass :: layout_rowmajor => fckit_tensor_layout_rowmajor
  procedure, nopass :: layout_colmajor => fckit_tensor_layout_colmajor

#if FCKIT_FINAL_NOT_INHERITING
  final :: fckit_tensor_real64__final_auto
#endif
end type fckit_tensor_real64


interface fckit_tensor_real32
  module procedure ctor_float_empty
  module procedure ctor_float_from_cptr
  module procedure ctor_float_from_shape
  module procedure ctor_from_float_array_rank1
  module procedure ctor_from_float_array_rank2
  module procedure ctor_from_float_array_rank3
  module procedure ctor_from_float_array_rank4
end interface


interface fckit_tensor_real64
  module procedure ctor_double_empty
  module procedure ctor_double_from_cptr
  module procedure ctor_double_from_shape
  module procedure ctor_from_double_array_rank1
  module procedure ctor_from_double_array_rank2
  module procedure ctor_from_double_array_rank3
  module procedure ctor_from_double_array_rank4
end interface


! used to access layout "nopass" methods
type(fckit_tensor_real32) :: fckit_tensor

public :: fckit_tensor

private :: c_int, c_ptr, c_size_t, c_float, c_double, c_f_pointer
private :: fckit_shared_object
private :: fckit_c_deleter
private :: fckit_c_nodeleter

contains


! --- tensor layouts ---
function fckit_tensor_layout_right() result(code)
  integer(c_int) :: code
  code = c_fckit_tensor_layout_right()
end function

function fckit_tensor_layout_left() result(code)
  integer(c_int) :: code
  code = c_fckit_tensor_layout_left()
end function

function fckit_tensor_layout_rowmajor() result(code)
  integer(c_int) :: code
  code = c_fckit_tensor_layout_rowmajor()
end function

function fckit_tensor_layout_colmajor() result(code)
  integer(c_int) :: code
  code = c_fckit_tensor_layout_colmajor()
end function



! ctor's
function ctor_float_empty(layout) result(this)
  type(fckit_tensor_real32) :: this
  integer(c_int), intent(in), optional :: layout
  integer(c_int) :: layout_actual = 1 ! left-layout (col-major)
  type(c_ptr) :: obj

  if (present(layout)) layout_actual = layout

  obj = c_fckit_tensor_real32_empty_new(layout_actual)
  call this%reset_c_ptr( obj, fckit_c_deleter(c_fckit_tensor_real32_delete) )
  call this%return() 
end function

function ctor_float_from_cptr(cptr, own) result(this)
  type(c_ptr), value :: cptr
  type(fckit_tensor_real32) :: this
  logical, optional :: own
  logical :: opt_own
  opt_own = .false.
  if( present(own) ) opt_own = own
  if( opt_own ) then
    call this%reset_c_ptr( cptr, fckit_c_deleter(c_fckit_tensor_real32_delete) )
  else
    call this%reset_c_ptr( cptr, fckit_c_nodeleter() )
  endif
  call this%return()
end function


function ctor_float_from_shape(shape, layout) result(this)
  type(fckit_tensor_real32) :: this
  integer(c_size_t),  intent(in) :: shape(:)
  integer(c_int), intent(in), optional :: layout
  integer(c_int) :: layout_actual = 1 ! left-layout (col-major)
  integer(c_size_t) :: rank

  if (present(layout)) layout_actual = layout
  rank = size(shape)
  call this%reset_c_ptr( c_fckit_tensor_real32_from_shape_new(rank, shape, layout_actual), &
                         fckit_c_deleter(c_fckit_tensor_real32_delete) )
  call this%return()
end function


function ctor_from_float_array_rank1(tensor, layout) result(this)
  type(fckit_tensor_real32) :: this
  real(c_float), intent(in) :: tensor(:)
  integer(c_int), intent(in), optional :: layout
  integer(c_int) :: layout_actual = 1 ! left-layout (col-major)
  real(c_float), pointer :: data_vec(:)
  integer(c_size_t) :: shape_vec(1)
  integer(c_size_t) :: rank

  if (present(layout)) layout_actual = layout
  data_vec => array_view1d( tensor )
  shape_vec = shape(tensor)
  rank = size(shape_vec)

  call this%reset_c_ptr( c_fckit_tensor_real32_from_array_rank1_new(rank, shape_vec, data_vec, layout_actual), &
                         fckit_c_deleter(c_fckit_tensor_real32_delete) )
  call this%return()  
end function


function ctor_from_float_array_rank2(tensor, layout) result(this)
  type(fckit_tensor_real32) :: this
  real(c_float), intent(in) :: tensor(:,:)
  integer(c_int), intent(in), optional :: layout
  integer(c_int) :: layout_actual = 1 ! left-layout (col-major)
  real(c_float), pointer :: data_vec(:)
  integer(c_size_t) :: shape_vec(2)
  integer(c_size_t) :: rank

  if (present(layout)) layout_actual = layout
  data_vec => array_view1d( tensor )
  shape_vec = shape(tensor)
  rank = size(shape_vec)

  call this%reset_c_ptr( c_fckit_tensor_real32_from_array_rank1_new(rank, shape_vec, data_vec, layout_actual), &
                         fckit_c_deleter(c_fckit_tensor_real32_delete) )
  call this%return()  
end function

function ctor_from_float_array_rank3(tensor, layout) result(this)
  type(fckit_tensor_real32) :: this
  real(c_float), intent(in) :: tensor(:,:,:)
  integer(c_int), intent(in), optional :: layout
  integer(c_int) :: layout_actual = 1 ! left-layout (col-major)
  real(c_float), pointer :: data_vec(:)
  integer(c_size_t) :: shape_vec(3)
  integer(c_size_t) :: rank

  if (present(layout)) layout_actual = layout
  data_vec => array_view1d( tensor )
  shape_vec = shape(tensor)
  rank = size(shape_vec)

  call this%reset_c_ptr( c_fckit_tensor_real32_from_array_rank1_new(rank, shape_vec, data_vec, layout_actual), &
                         fckit_c_deleter(c_fckit_tensor_real32_delete) )
  call this%return()  
end function


function ctor_from_float_array_rank4(tensor, layout) result(this)
  type(fckit_tensor_real32) :: this
  real(c_float), intent(in) :: tensor(:,:,:,:)
  integer(c_int), intent(in), optional :: layout
  integer(c_int) :: layout_actual = 1 ! left-layout (col-major)
  real(c_float), pointer :: data_vec(:)
  integer(c_size_t) :: shape_vec(4)
  integer(c_size_t) :: rank

  if (present(layout)) layout_actual = layout
  data_vec => array_view1d( tensor )
  shape_vec = shape(tensor)
  rank = size(shape_vec)

  call this%reset_c_ptr( c_fckit_tensor_real32_from_array_rank1_new(rank, shape_vec, data_vec, layout_actual), &
                         fckit_c_deleter(c_fckit_tensor_real32_delete) )
  call this%return()  
end function
  

! number of elements
function fckit_tensor_real32__size(this) result(val)
  class(fckit_tensor_real32), intent(in) :: this
  integer(c_size_t) :: val
  val = c_fckit_tensor_real32_size(this%c_ptr())
end function


! number of elements
function fckit_tensor_real32__rank(this) result(val)
  class(fckit_tensor_real32), intent(in) :: this
  integer(c_size_t) :: val
  val = c_fckit_tensor_real32_rank(this%c_ptr())
end function


! shape of the tensor
function fckit_tensor_real32__shape(this) result(shape_array)
  class(fckit_tensor_real32), intent(in)  :: this
  integer(c_size_t),   allocatable :: shape_array(:)
  integer(c_size_t),   pointer     :: shape_fptr(:)
  type(c_ptr)                      :: shape_cptr
  integer(c_size_t)                :: rank
  ! get shape and shape-size
  call c_fckit_tensor_real32_shape(this%c_ptr(), shape_cptr, rank)
  call c_f_pointer(shape_cptr, shape_fptr, (/rank/))
  allocate(shape_array(rank))
  shape_array(:) = shape_fptr(:)
  call c_ptr_free(shape_cptr)
end function

! fill it in with value
subroutine fckit_tensor_real32__fill(this, val)
  class(fckit_tensor_real32), intent(inout) :: this
  real(c_float), intent(in) :: val
  call c_fckit_tensor_real32_fill(this%c_ptr(), val)
end subroutine

! fill it in with "0"s
subroutine fckit_tensor_real32__zero(this)
  class(fckit_tensor_real32), intent(inout) :: this
  call c_fckit_tensor_real32_zero(this%c_ptr())
end subroutine

! tensor layout
function fckit_tensor_real32__layout(this) result(val)
  class(fckit_tensor_real32), intent(in) :: this
  integer(c_int) :: val
  val = c_fckit_tensor_real32_layout(this%c_ptr())
end function



#if FCKIT_FINAL_NOT_INHERITING
FCKIT_FINAL subroutine fckit_tensor_real32__final_auto(this)
  type(fckit_tensor_real32), intent(inout) :: this
#if FCKIT_FINAL_DEBUGGING
  write(0,*) "fckit_tensor_real32__final_auto"
#endif
#if FCKIT_FINAL_NOT_PROPAGATING
  call this%final()
#endif
  FCKIT_SUPPRESS_UNUSED( this )
end subroutine
#endif



! ======================================================
!                  fckit_tensor_real64
! ======================================================

! ctor's
function ctor_double_empty(layout) result(this)
  type(fckit_tensor_real64) :: this
  integer(c_int), intent(in), optional :: layout
  integer(c_int) :: layout_actual = 1 ! left-layout (col-major)
  type(c_ptr) :: obj

  if (present(layout)) layout_actual = layout
  obj = c_fckit_tensor_real64_empty_new(layout_actual)

  call this%reset_c_ptr( obj, fckit_c_deleter(c_fckit_tensor_real64_delete) )
  call this%return() 
end function


function ctor_double_from_cptr(cptr, own) result(this)
  type(c_ptr), value :: cptr
  type(fckit_tensor_real64) :: this
  logical, optional :: own
  logical :: opt_own
  opt_own = .false.
  if( present(own) ) opt_own = own
  if( opt_own ) then
    call this%reset_c_ptr( cptr, fckit_c_deleter(c_fckit_tensor_real64_delete) )
  else
    call this%reset_c_ptr( cptr, fckit_c_nodeleter() )
  endif
  call this%return()
end function


function ctor_double_from_shape(shape, layout) result(this)
  type(fckit_tensor_real64) :: this
  integer(c_size_t),  intent(in) :: shape(:)
  integer(c_int), intent(in), optional :: layout
  integer(c_int) :: layout_actual = 1 ! left-layout (col-major)
  integer(c_size_t)     :: rank

  if (present(layout)) layout_actual = layout
  rank = size(shape)
  call this%reset_c_ptr( c_fckit_tensor_real64_from_shape_new(rank, shape, layout_actual), &
                         fckit_c_deleter(c_fckit_tensor_real64_delete) )
  call this%return()
end function


function ctor_from_double_array_rank1(tensor, layout) result(this)
  type(fckit_tensor_real64) :: this
  real(c_double), intent(in) :: tensor(:)
  integer(c_int), intent(in), optional :: layout
  integer(c_int) :: layout_actual = 1 ! left-layout (col-major)
  real(c_double), pointer :: data_vec(:)
  integer(c_size_t) :: shape_vec(1)
  integer(c_size_t) :: rank

  if (present(layout)) layout_actual = layout
  data_vec => array_view1d( tensor )
  shape_vec = shape(tensor)
  rank = size(shape_vec)

  call this%reset_c_ptr( c_fckit_tensor_real64_from_array_rank1_new(rank, shape_vec, data_vec, layout_actual), &
                         fckit_c_deleter(c_fckit_tensor_real64_delete) )
  call this%return()  
end function


function ctor_from_double_array_rank2(tensor, layout) result(this)
  type(fckit_tensor_real64) :: this
  real(c_double), intent(in) :: tensor(:,:)
  integer(c_int), intent(in), optional :: layout
  integer(c_int) :: layout_actual = 1 ! left-layout (col-major)
  real(c_double), pointer :: data_vec(:)
  integer(c_size_t) :: shape_vec(2)
  integer(c_size_t) :: rank

  if (present(layout)) layout_actual = layout
  data_vec => array_view1d( tensor )
  shape_vec = shape(tensor)
  rank = size(shape_vec)

  call this%reset_c_ptr( c_fckit_tensor_real64_from_array_rank1_new(rank, shape_vec, data_vec, layout_actual), &
                         fckit_c_deleter(c_fckit_tensor_real64_delete) )
  call this%return()  
end function

function ctor_from_double_array_rank3(tensor, layout) result(this)
  type(fckit_tensor_real64) :: this
  real(c_double), intent(in) :: tensor(:,:,:)
  integer(c_int), intent(in), optional :: layout
  integer(c_int) :: layout_actual = 1 ! left-layout (col-major)
  real(c_double), pointer :: data_vec(:)
  integer(c_size_t) :: shape_vec(3)
  integer(c_size_t) :: rank

  if (present(layout)) layout_actual = layout
  data_vec => array_view1d( tensor )
  shape_vec = shape(tensor)
  rank = size(shape_vec)

  call this%reset_c_ptr( c_fckit_tensor_real64_from_array_rank1_new(rank, shape_vec, data_vec, layout_actual), &
                         fckit_c_deleter(c_fckit_tensor_real64_delete) )
  call this%return()  
end function


function ctor_from_double_array_rank4(tensor, layout) result(this)
  type(fckit_tensor_real64) :: this
  real(c_double), intent(in) :: tensor(:,:,:,:)
  integer(c_int), intent(in), optional :: layout
  integer(c_int) :: layout_actual = 1 ! left-layout (col-major)
  real(c_double), pointer :: data_vec(:)
  integer(c_size_t) :: shape_vec(4)
  integer(c_size_t) :: rank

  if (present(layout)) layout_actual = layout
  data_vec => array_view1d( tensor )
  shape_vec = shape(tensor)
  rank = size(shape_vec)

  call this%reset_c_ptr( c_fckit_tensor_real64_from_array_rank1_new(rank, shape_vec, data_vec, layout_actual), &
                         fckit_c_deleter(c_fckit_tensor_real64_delete) )
  call this%return()  
end function
  

! number of elements
function fckit_tensor_real64__size(this) result(val)
  class(fckit_tensor_real64), intent(in) :: this
  integer(c_size_t) :: val
  val = c_fckit_tensor_real64_size(this%c_ptr())
end function

! number of elements
function fckit_tensor_real64__rank(this) result(val)
  class(fckit_tensor_real64), intent(in) :: this
  integer(c_size_t) :: val
  val = c_fckit_tensor_real64_rank(this%c_ptr())
end function

! shape of the tensor
function fckit_tensor_real64__shape(this) result(shape_array)
  class(fckit_tensor_real64), intent(in)  :: this
  integer(c_size_t),   allocatable :: shape_array(:)
  integer(c_size_t),   pointer     :: shape_fptr(:)
  type(c_ptr)                      :: shape_cptr
  integer(c_size_t)                :: rank
  ! get shape and shape-size
  call c_fckit_tensor_real64_shape(this%c_ptr(), shape_cptr, rank)
  call c_f_pointer(shape_cptr, shape_fptr, (/rank/))
  allocate(shape_array(rank))
  shape_array(:) = shape_fptr(:)
  call c_ptr_free(shape_cptr)
end function

! fill it in with value
subroutine fckit_tensor_real64__fill(this, val)
  class(fckit_tensor_real64), intent(inout) :: this
  real(c_double), intent(in) :: val
  call c_fckit_tensor_real64_fill(this%c_ptr(), val)
end subroutine

! fill it in with "0"s
subroutine fckit_tensor_real64__zero(this)
  class(fckit_tensor_real64), intent(inout) :: this
  call c_fckit_tensor_real64_zero(this%c_ptr())
end subroutine

! is right-layout tensor? 
function fckit_tensor_real64__layout(this) result(val)
  class(fckit_tensor_real64), intent(in) :: this
  integer(c_int) :: val
  val = c_fckit_tensor_real64_layout(this%c_ptr())
end function



#if FCKIT_FINAL_NOT_INHERITING
FCKIT_FINAL subroutine fckit_tensor_real64__final_auto(this)
  type(fckit_tensor_real64), intent(inout) :: this
#if FCKIT_FINAL_DEBUGGING
  write(0,*) "fckit_tensor_real64__final_auto"
#endif
#if FCKIT_FINAL_NOT_PROPAGATING
  call this%final()
#endif
  FCKIT_SUPPRESS_UNUSED( this )
end subroutine
#endif

end module fckit_tensor_module
