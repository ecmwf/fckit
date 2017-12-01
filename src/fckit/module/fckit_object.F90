! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module fckit_object_module
  !! Provides abstract base class [[fckit_object_module:fckit_object(type)]]

use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr
implicit none
private

!========================================================================
! Public interface

public fckit_object

!========================================================================

type, abstract :: fckit_object
  !! Abstract base class for objects that wrap a C++ object

  type(c_ptr), private :: cpp_object_ptr = c_null_ptr
    !! Internal C pointer

  integer :: refcount = 0

contains

  procedure, public :: is_null
    !! Check if internal C pointer is set

  procedure, public :: c_ptr   => fckit_object__c_ptr
    !! Access to internal C pointer

  procedure, public :: reset_c_ptr
    !! Nullify internal C pointer

  procedure, private :: equal
    !! Compare two object's C pointers

  procedure, private :: not_equal
    !! Compare two object's C pointers

  generic, public :: operator(==) => equal
    !! Compare two objects internal C pointer

  generic, public :: operator(/=) => not_equal
    !! Compare two objects internal C pointer

  procedure(final), deferred, public :: final
    !! Finalise object

  ! Following line is to avoid PGI compiler bug
  procedure, private :: fckit_object__c_ptr

end type

!========================================================================

private :: c_ptr
private :: c_null_ptr

! =============================================================================
CONTAINS
! =============================================================================

function fckit_object__c_ptr(this)
  use, intrinsic :: iso_c_binding, only: c_ptr
  type(c_ptr) :: fckit_object__c_ptr
  class(fckit_object), intent(in) :: this
  fckit_object__c_ptr = this%cpp_object_ptr
end function

subroutine reset_c_ptr(this,cptr)
  use, intrinsic :: iso_c_binding, only: c_ptr
  use fckit_c_interop_module
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
  use fckit_c_interop_module, only : c_ptr_compare_equal
  class(fckit_object), intent(in) :: obj1
  class(fckit_object), intent(in) :: obj2
  equal = c_ptr_compare_equal(obj1%c_ptr(),obj2%c_ptr())
end function

logical function not_equal(obj1,obj2)
  use fckit_c_interop_module, only : c_ptr_compare_equal
  class(fckit_object), intent(in) :: obj1
  class(fckit_object), intent(in) :: obj2
  if( c_ptr_compare_equal(obj1%c_ptr(),obj2%c_ptr()) ) then
    not_equal = .False.
  else
    not_equal = .True.
  endif
end function

!========================================================================
! Interface

subroutine final(this)
  class(fckit_object), intent(inout) :: this
end subroutine

end module
