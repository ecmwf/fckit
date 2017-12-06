! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module fckit_object_module
  !! Provides abstract base class [[fckit_object_module:fckit_object(type)]]

use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr, c_null_ptr
use fckit_final_module, only: fckit_final
implicit none
private

!========================================================================
! Public interface

public fckit_object

!========================================================================

type, extends(fckit_final) :: fckit_object
  !! Abstract base class for objects that wrap a C++ object

  type(c_ptr), private :: cpp_object_ptr = c_null_ptr
  type(c_funptr), private :: deleter
    !! Internal C pointer

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

  ! Following line is to avoid PGI compiler bug
  procedure, private :: fckit_object__c_ptr
  
  procedure, public :: final

end type

interface fckit_object
  module procedure fckit_object_constructor
end interface

!========================================================================

private :: c_ptr
private :: c_null_ptr

! =============================================================================
CONTAINS
! =============================================================================

function fckit_object_constructor( cptr, deleter ) result(this)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_funptr
  type(fckit_object) :: this
  type(c_ptr) :: cptr
  type(c_funptr), optional :: deleter
  if( present(deleter) ) then
    call this%reset_c_ptr( cptr, deleter )
  else
    call this%reset_c_ptr( cptr )
  endif
end function

function fckit_object__c_ptr(this)
  use, intrinsic :: iso_c_binding, only: c_ptr
  type(c_ptr) :: fckit_object__c_ptr
  class(fckit_object), intent(in) :: this
  fckit_object__c_ptr = this%cpp_object_ptr
end function

subroutine reset_c_ptr(this,cptr,deleter)
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr, c_null_funptr
  use fckit_c_interop_module
  class(fckit_object) :: this
  type(c_ptr), optional :: cptr
  type(c_funptr), optional :: deleter
  if( present(cptr) ) then
    this%cpp_object_ptr = cptr
  else
    this%cpp_object_ptr = c_null_ptr
  endif
  if( present(deleter) ) then
    this%deleter = deleter
  else
    this%deleter = c_null_funptr
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

subroutine final( this )
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr, c_f_procpointer, c_associated
  use fckit_c_interop_module, only : fckit_c_deleter_interface
  class(fckit_object), intent(inout) :: this
  procedure(fckit_c_deleter_interface), pointer :: deleter
  if( c_associated( this%cpp_object_ptr ) ) then
    if( c_associated( this%deleter ) ) then
      call c_f_procpointer( this%deleter, deleter )
      call deleter( this%cpp_object_ptr )
    endif
  endif
end subroutine

end module
