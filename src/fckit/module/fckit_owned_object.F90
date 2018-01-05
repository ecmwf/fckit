! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/defines.h"

#ifdef FCKIT_FINAL_DEBUGGING
#define FCKIT_WRITE_DEBUG write(0,'(A,I0,A)',advance='NO') "fckit_owned_object.F90 @ ",__LINE__,'  : '; write(0,*) 
#else
#define FCKIT_WRITE_DEBUG !
#endif

module fckit_owned_object_module
use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr, c_null_ptr
implicit none
private

!========================================================================
! Public interface

public fckit_owned_object

!========================================================================

type :: fckit_owned_object
  !! Abstract base class for objects that wrap a C++ object

  type(c_ptr), private :: cpp_object_ptr = c_null_ptr
  type(c_funptr), private :: deleter
    !! Internal C pointer

#if FCKIT_FINAL_DEBUGGING
    logical :: return_value = .false.
#endif

contains

  procedure, public :: is_null
    !! Check if internal C pointer is set

  procedure, public :: c_ptr   => fckit_owned_object__c_ptr
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
  procedure, private :: fckit_owned_object__c_ptr

  procedure, public :: final => fckit_owned_object__final

#if FCKIT_HAVE_FINAL
  final :: fckit_object_final_auto
#endif

  procedure, private :: assignment_operator
  generic, public :: assignment(=) => assignment_operator
  procedure, public :: owners
  procedure, public :: attach
  procedure, public :: detach
  procedure, public :: return
  procedure, public :: assignment_operator_hook

  procedure, public :: consumed
end type

interface fckit_object
  module procedure fckit_owned_object_constructor
end interface

!========================================================================

private :: c_ptr
private :: c_null_ptr

!========================================================================

interface

  subroutine fckit__delete_Owned(this) bind(c,name="fckit__delete_Owned")
    use, intrinsic :: iso_c_binding, only: c_ptr
    type(c_ptr), value :: this
  end subroutine

  function fckit__Owned__owners(this) bind(c,name="fckit__Owned__owners")
    use, intrinsic :: iso_c_binding, only: c_int, c_ptr
    integer(c_int) :: fckit__Owned__owners
    type(c_ptr), value :: this
  end function

  subroutine fckit__Owned__attach(this) bind(c,name="fckit__Owned__attach")
    use, intrinsic :: iso_c_binding, only: c_ptr
    type(c_ptr), value :: this
  end subroutine

  subroutine fckit__Owned__detach(this) bind(c,name="fckit__Owned__detach")
    use, intrinsic :: iso_c_binding, only: c_ptr
    type(c_ptr), value :: this
  end subroutine

end interface

!========================================================================
CONTAINS
!========================================================================

subroutine fckit_owned_object__final_auto(this)
#ifdef _CRAYFTN
  use, intrinsic :: iso_c_binding, only : c_loc, c_null_ptr
#endif
  type(fckit_owned_object), intent(inout) :: this
  FCKIT_WRITE_DEBUG "fckit_owned_object__final_auto"
  ! Guard necessary for Cray compiler...
  ! ... when "this" has already been deallocated, and then
  ! fckit_owned_object__final_auto is called...
#ifdef _CRAYFTN
  if( c_loc(this) == c_null_ptr ) then
    return
  endif
#endif

  call this%final()
end subroutine

subroutine fckit_owned_object__delete( this )
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr, c_f_procpointer, c_associated, c_null_ptr
  use fckit_c_interop_module, only : fckit_c_deleter_interface
  class(fckit_owned_object), intent(inout) :: this
  procedure(fckit_c_deleter_interface), pointer :: deleter
  FCKIT_WRITE_DEBUG "fckit_owned_object__delete"
  if( c_associated( this%cpp_object_ptr ) ) then
    if( c_associated( this%deleter ) ) then
      call c_f_procpointer( this%deleter, deleter )
      call deleter( this%cpp_object_ptr )
      this%cpp_object_ptr = c_null_ptr
    endif
  endif
  this%cpp_object_ptr = c_null_ptr
end subroutine

subroutine fckit_owned_object__final(this)
  class(fckit_owned_object), intent(inout) :: this

  if( this%is_null() ) then
    FCKIT_WRITE_DEBUG "fckit_owned_object__final  (uninitialised --> no-op)"
    return
  endif

#if FCKIT_FINAL_DEBUGGING
  if( this%return_value ) then
    FCKIT_WRITE_DEBUG "fckit_owned_object__final on return value, owners = ", this%owners()
  endif
#endif

  if( this%owners() > 0 ) then
    FCKIT_WRITE_DEBUG "fckit_owned_object__final  , owners = ", this%owners()
    call this%detach()
    if( this%owners() == 0 ) then
      call fckit_owned_object__delete(this)
    endif
  endif

  call this%reset_c_ptr()
end subroutine



subroutine assignment_operator(this,other)
  class(fckit_owned_object), intent(inout) :: this
  class(fckit_owned_object), intent(in)    :: other
  if( other%is_null() ) then
    write(0,*) "ERROR! other was not initialised"
  endif
#if FCKIT_FINAL_DEBUGGING
  if( other%return_value ) then
    FCKIT_WRITE_DEBUG "other is a return value"
  endif
#endif
  if( this /= other ) then
#if FCKIT_FINAL_DEBUGGING
    if( this%is_null() ) then
      FCKIT_WRITE_DEBUG "assignment_operator of uninitialised"
    else
      FCKIT_WRITE_DEBUG "assignment_operator of initialised"
    endif
#endif
    call this%final()
    call this%reset_c_ptr( other%c_ptr(), other%deleter )
  else
    FCKIT_WRITE_DEBUG "assignment_operator ( obj_out = obj_in )"
  endif
  call this%assignment_operator_hook(other)
 end subroutine

subroutine attach(this)
  class(fckit_owned_object), intent(inout) :: this
  if( .not. this%is_null() ) then
    call fckit__Owned__attach(this%c_ptr())
  endif
end subroutine

subroutine detach(this)
  class(fckit_owned_object), intent(inout) :: this
  if( .not. this%is_null() ) then
    call fckit__Owned__detach(this%c_ptr())
  endif
end subroutine

function owners(this)
  integer :: owners
  class(fckit_owned_object), intent(in) :: this
  if( this%is_null() ) then
    owners = 0
  else
    owners = fckit__Owned__owners(this%c_ptr())
  endif
end function

subroutine return(this)
  !! Transfer ownership to left hand side of "assignment(=)"
  class(fckit_owned_object), intent(inout) :: this
#if FCKIT_FINAL_FUNCTION_RESULT
  ! Cray example
  ! final will be called, which will detach, so attach first
  if( this%owners() == 0 ) then
    FCKIT_WRITE_DEBUG "return --> attach"
    call this%attach()
  endif
#else
  ! final will not be called, so detach manually
  if( this%owners() > 0 ) then
    FCKIT_WRITE_DEBUG "return --> detach"
    call this%detach()
  endif
#endif
#if FCKIT_FINAL_DEBUGGING
  this%return_value = .true.
#endif
end subroutine


subroutine assignment_operator_hook(this, other)
  class(fckit_owned_object) :: this
  class(fckit_owned_object) :: other
  FCKIT_SUPPRESS_UNUSED( this )
  FCKIT_SUPPRESS_UNUSED( other )
end subroutine


subroutine bad_cast(message)
  character(len=*), optional :: message
  if( present(message) ) then
    write(0,'("ERROR: bad_cast -- ",A)') message
  else
    write(0,'("ERROR: bad cast")')
  endif
end subroutine

subroutine consumed(this)
  class(fckit_owned_object), intent(in) :: this
  type(fckit_owned_object) :: consumed_object
  consumed_object = this
  call consumed_object%final()
end subroutine




function fckit_owned_object_constructor( cptr, deleter ) result(this)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_funptr
  type(fckit_owned_object) :: this
  type(c_ptr) :: cptr
  type(c_funptr), optional :: deleter
  if( present(deleter) ) then
    call this%reset_c_ptr( cptr, deleter )
  else
    call this%reset_c_ptr( cptr )
  endif
end function

function fckit_owned_object__c_ptr(this)
  use, intrinsic :: iso_c_binding, only: c_ptr
  type(c_ptr) :: fckit_owned_object__c_ptr
  class(fckit_owned_object), intent(in) :: this
  fckit_owned_object__c_ptr = this%cpp_object_ptr
end function


function is_null(this)
  use, intrinsic :: iso_c_binding, only: c_associated
  logical :: is_null
  class(fckit_owned_object) :: this
  if( c_associated( this%cpp_object_ptr ) ) then
    is_null = .False.
  else
    is_null = .True.
  endif
end function

logical function equal(obj1,obj2)
  use fckit_c_interop_module, only : c_ptr_compare_equal
  class(fckit_owned_object), intent(in) :: obj1
  class(fckit_owned_object), intent(in) :: obj2
  equal = c_ptr_compare_equal(obj1%c_ptr(),obj2%c_ptr())
end function

logical function not_equal(obj1,obj2)
  use fckit_c_interop_module, only : c_ptr_compare_equal
  class(fckit_owned_object), intent(in) :: obj1
  class(fckit_owned_object), intent(in) :: obj2
  if( c_ptr_compare_equal(obj1%c_ptr(),obj2%c_ptr()) ) then
    not_equal = .False.
  else
    not_equal = .True.
  endif
end function

subroutine reset_c_ptr(this,cptr,deleter)
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr, c_null_funptr
  use fckit_c_interop_module
  class(fckit_owned_object) :: this
  type(c_ptr), optional :: cptr
  type(c_funptr), optional :: deleter
  if( present(cptr) ) then
    this%cpp_object_ptr = cptr
    call this%attach()
  else
    this%cpp_object_ptr = c_null_ptr
  endif
  if( present(deleter) ) then
    this%deleter = deleter
  else
    this%deleter = fckit_c_deleter(fckit__delete_Owned)
  endif
end subroutine

end module
