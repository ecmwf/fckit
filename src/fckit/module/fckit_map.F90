#include "fckit/fckit.h"

module fckit_map_module

use, intrinsic :: iso_c_binding, only : c_ptr, &
                                        c_int, &
                                        c_int32_t, &
                                        c_int64_t, &
                                        c_float, &
                                        c_double, &
                                        c_null_char

use fckit_array_module, only: array_view1d
use fckit_shared_object_module, only : fckit_shared_object, fckit_c_deleter

implicit none

public :: fckit_map


private


#include "fckit_map.inc"

type, extends(fckit_shared_object) :: fckit_map

contains

  procedure :: has
  procedure :: size => map_size
  
  ! setters
  procedure :: insert_int32
  procedure :: insert_int64
  procedure :: insert_real32
  procedure :: insert_real64
  procedure :: insert_c_ptr

  generic   :: insert => insert_int32, &
                         insert_int64, & 
                         insert_real32, &
                         insert_real64, &
                         insert_c_ptr

  ! getters
  procedure :: get_int32
  procedure :: get_int64
  procedure :: get_real32
  procedure :: get_real64
  procedure :: get_c_ptr
  
  procedure, private :: get_int32_sub
  procedure, private :: get_int64_sub
  procedure, private :: get_real32_sub
  procedure, private :: get_real64_sub
  procedure, private :: get_c_ptr_sub
  procedure, private :: get_shared_sub

  generic, public :: get => get_int32_sub, &
                            get_int64_sub, &
                            get_real32_sub, &
                            get_real64_sub, &
                            get_c_ptr_sub, &
                            get_shared_sub

#if FCKIT_FINAL_NOT_INHERITING
    final :: fckit_map__final_auto
#endif

end type


! ------ interfaces

interface fckit_map
  module procedure ctor
end interface


interface insert
  module procedure insert_int32
  module procedure insert_int64
  module procedure insert_real32
  module procedure insert_real64
  module procedure insert_c_ptr
end interface

interface get
  module procedure get_int32_sub
  module procedure get_int64_sub
  module procedure get_real32_sub
  module procedure get_real64_sub
  module procedure get_c_ptr_sub
  module procedure get_shared_sub
end interface


! ------ end interfaces 

private :: c_ptr, c_int, c_int32_t, c_int64_t, c_float, c_double, c_null_char
private :: fckit_shared_object
private :: fckit_c_deleter

contains


! ctor's
function ctor() result(this)
    type(fckit_map) :: this
    call this%reset_c_ptr( c_fckit_map_new(), &
      & fckit_c_deleter(c_fckit_map_delete) )
    call this%return()
end function


#if FCKIT_FINAL_NOT_INHERITING
FCKIT_FINAL subroutine fckit_map__final_auto(this)
  type(fckit_map), intent(inout) :: this
#if FCKIT_FINAL_DEBUGGING
  write(0,*) "fckit_map__final_auto"
#endif
#if FCKIT_FINAL_NOT_PROPAGATING
  call this%final()
#endif
  FCKIT_SUPPRESS_UNUSED( this )
end subroutine
#endif


! ============ insert

! ---- insert int
subroutine insert_int32(this, name, value)
  class(fckit_map), intent(inout) :: this  
  character(len=*), intent(in) :: name
  integer(c_int) :: value
  call c_fckit_map_insert_int32(this%c_ptr(), name//c_null_char, value)
end subroutine


! ---- insert int
subroutine insert_int64(this, name, value)
  class(fckit_map), intent(inout) :: this  
  character(len=*), intent(in) :: name
  integer(c_int64_t) :: value
  call c_fckit_map_insert_int64(this%c_ptr(), name//c_null_char, value)
end subroutine


! ---- insert float
subroutine insert_real32(this, name, value)
  class(fckit_map), intent(inout) :: this  
  character(len=*), intent(in) :: name
  real(c_float) :: value
  call c_fckit_map_insert_real32(this%c_ptr(), name//c_null_char, value)
end subroutine


! ---- insert double
subroutine insert_real64(this, name, value)
  class(fckit_map), intent(inout) :: this  
  character(len=*), intent(in) :: name
  real(c_double) :: value
  call c_fckit_map_insert_real64(this%c_ptr(), name//c_null_char, value)
end subroutine


! ---- insert c_ptr
subroutine insert_c_ptr(this, name, value)
  class(fckit_map), intent(inout) :: this  
  character(len=*), intent(in) :: name
  type(c_ptr) :: value
  call c_fckit_map_insert_c_ptr(this%c_ptr(), name//c_null_char, value)
end subroutine


! ============ getters


function get_int32(this, name)
  class(fckit_map), intent(in) :: this
  character(len=*), intent(in) :: name
  integer(c_int) :: get_int32
  call c_fckit_map_get_int32(this%c_ptr(), name//c_null_char, get_int32)
end function


function get_int64(this, name)
  class(fckit_map), intent(in) :: this
  character(len=*), intent(in) :: name
  integer(c_int64_t) :: get_int64
  call c_fckit_map_get_int64(this%c_ptr(), name//c_null_char, get_int64)
end function


function get_real32(this, name)
  class(fckit_map), intent(in) :: this
  character(len=*), intent(in) :: name
  real(c_float) :: get_real32
  call c_fckit_map_get_real32(this%c_ptr(), name//c_null_char, get_real32)
end function


function get_real64(this, name)
  class(fckit_map), intent(in) :: this
  character(len=*), intent(in) :: name
  real(c_double) :: get_real64
  call c_fckit_map_get_real64(this%c_ptr(), name//c_null_char, get_real64)
end function


function get_c_ptr(this, name)
  class(fckit_map), intent(in) :: this
  character(len=*), intent(in) :: name
  type(c_ptr) :: get_c_ptr
  call c_fckit_map_get_c_ptr(this%c_ptr(), name//c_null_char, get_c_ptr)
end function

! ---

subroutine get_int32_sub(this, name, value)
  class(fckit_map), intent(in) :: this
  character(len=*), intent(in) :: name
  integer(c_int32_t), intent(inout) :: value
  call c_fckit_map_get_int32(this%c_ptr(), name//c_null_char, value)
end subroutine

subroutine get_int64_sub(this, name, value)
  class(fckit_map), intent(in) :: this
  character(len=*), intent(in) :: name
  integer(c_int64_t), intent(inout) :: value
  call c_fckit_map_get_int64(this%c_ptr(), name//c_null_char, value)
end subroutine

subroutine get_real32_sub(this, name, value)
  class(fckit_map), intent(in) :: this
  character(len=*), intent(in) :: name
  real(c_float), intent(inout) :: value
  call c_fckit_map_get_real32(this%c_ptr(), name//c_null_char, value)
end subroutine

subroutine get_real64_sub(this, name, value)
  class(fckit_map), intent(in) :: this
  character(len=*), intent(in) :: name
  real(c_double), intent(inout) :: value
  call c_fckit_map_get_real64(this%c_ptr(), name//c_null_char, value)
end subroutine

subroutine get_c_ptr_sub(this, name, value)
  class(fckit_map), intent(in) :: this
  character(len=*), intent(in) :: name
  type(c_ptr) :: value
  call c_fckit_map_get_c_ptr(this%c_ptr(), name//c_null_char, value)
end subroutine

subroutine get_shared_sub(this, name, value)
  class(fckit_map), intent(in) :: this
  character(len=*), intent(in) :: name
  class(fckit_shared_object), intent(inout) :: value
  type(c_ptr) :: shared_cptr
  call c_fckit_map_get_c_ptr(this%c_ptr(), name//c_null_char, shared_cptr)
  call value%reset_c_ptr(shared_cptr)
end subroutine

! =========== other

function has(this, name)
  class(fckit_map), intent(inout) :: this
  character(len=*), intent(in) :: name
  logical :: has
  has = c_fckit_map_has(this%c_ptr(), name//c_null_char)
end function


function map_size(this)
  class(fckit_map), intent(inout) :: this
  integer :: map_size
  map_size = c_fckit_map_size(this%c_ptr() )
end function


end module fckit_map_module