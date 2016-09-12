module fckit_object_module
use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr
implicit none
private

!========================================================================
! Public interface

public fckit_object

!========================================================================

type, abstract :: fckit_object
  type(c_ptr), private :: cpp_object_ptr = c_null_ptr
contains
  procedure, public :: is_null
  procedure, public :: c_ptr   => fckit_object__c_ptr
  procedure, public :: reset_c_ptr => reset_c_ptr

  procedure, private :: equal
  procedure, private :: not_equal
  generic, public :: operator(==) => equal
  generic, public :: operator(/=) => not_equal

  procedure, public :: final
  procedure(final), deferred, public :: delete

end type

!========================================================================

private :: c_ptr
private :: c_null_ptr

! =============================================================================
CONTAINS
! =============================================================================

function fckit_compare_equal(p1,p2) result(equal)
  use, intrinsic :: iso_c_binding, only: c_ptr, c_associated
  use fckit_C_interop, only : c_ptr_compare_equal
  logical :: equal
  type(c_ptr), intent(in) :: p1, p2
  if( c_ptr_compare_equal(p1,p2) == 1 ) then
    equal = .True.
  else
    equal = .False.
  endif
end function

function fckit_object__c_ptr(this) result(cptr)
  use, intrinsic :: iso_c_binding, only: c_ptr
  type(c_ptr) :: cptr
  class(fckit_object) :: this
  cptr = this%cpp_object_ptr
end function

subroutine reset_c_ptr(this,cptr)
  use, intrinsic :: iso_c_binding, only: c_ptr
  class(fckit_object) :: this
  type(c_ptr), optional :: cptr
  if( present(cptr) ) then
    this%cpp_object_ptr = cptr
  else
    this%cpp_object_ptr = c_null_ptr
  endif
end subroutine

function is_null(this)
  use, intrinsic :: iso_c_binding, only: c_associated
  logical :: is_null
  class(fckit_object) :: this
  if( c_associated( this%cpp_object_ptr ) ) then
    is_null = .False.
  else
    is_null = .True.
  endif
end function

logical function equal(obj1,obj2)
  use fckit_C_interop, only : c_ptr_compare_equal
  class(fckit_object), intent(in) :: obj1
  class(fckit_object), intent(in) :: obj2
  equal = c_ptr_compare_equal(obj1%c_ptr(),obj2%c_ptr())
end function

logical function not_equal(obj1,obj2)
  use fckit_C_interop, only : c_ptr_compare_equal
  class(fckit_object), intent(in) :: obj1
  class(fckit_object), intent(in) :: obj2
  if( c_ptr_compare_equal(obj1%c_ptr(),obj2%c_ptr()) ) then
    not_equal = .False.
  else
    not_equal = .True.
  endif
end function


subroutine final(this)
  class(fckit_object), intent(inout) :: this
end subroutine

end module
