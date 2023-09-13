! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fckit.h"

#if FCKIT_FINAL_DEBUGGING
#define FCKIT_WRITE_LOC write(0,'(A,I0,A)',advance='NO') "fckit_owned_object.F90 @ ",__LINE__,'  : '
#define FCKIT_WRITE_DEBUG write(0,*)
#endif

module fckit_owned_object_module
use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr, c_null_ptr, c_null_funptr, c_int32_t
use fckit_c_interop_module, only : c_ptr_to_loc
implicit none
private

!========================================================================
! Public interface

public fckit_owned_object

!========================================================================

type :: fckit_owned_object
  !! Abstract base class for objects that wrap a C++ object

#if !PGIBUG_ATLAS_197
  type(c_ptr), private :: cpp_object_ptr = c_null_ptr
#else
  type(c_ptr), public :: cpp_object_ptr = c_null_ptr
#endif
  type(c_funptr), private :: deleter = c_null_funptr
    !! Internal C pointer

  logical, private :: return_value = .false.
    ! This variable should not be necessary,
    ! but seems to overcome compiler issues (gfortran 5.3, 6.3)

contains

  procedure, public :: is_null
    !! Check if internal C pointer is set

#if !PGIBUG_ATLAS_197_DEBUG
  procedure, public :: c_ptr   => fckit_owned_object__c_ptr
    !! Access to internal C pointer
#endif

  procedure, public :: reset_c_ptr
    !! Nullify internal C pointer

  procedure, private :: equal
    !! Compare two object C pointers

  procedure, private :: not_equal
    !! Compare two object C pointers

  generic, public :: operator(==) => equal
    !! Compare two objects internal C pointer

  generic, public :: operator(/=) => not_equal
    !! Compare two objects internal C pointer

  ! Following line is to avoid PGI compiler bug
  procedure, private :: fckit_owned_object__c_ptr

  procedure, public :: final => fckit_owned_object__final

#if FCKIT_HAVE_FINAL
  final :: fckit_owned_object__final_auto
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

!========================================================================

private :: c_ptr
private :: c_null_ptr
private :: c_funptr
private :: c_null_funptr
private :: c_int32_t

!========================================================================

interface

  subroutine fckit__delete_Owned(this) bind(c,name="fckit__delete_Owned")
    use, intrinsic :: iso_c_binding, only: c_ptr
    type(c_ptr), value :: this
  end subroutine

  function fckit__Owned__owners(this) bind(c,name="fckit__Owned__owners")
    use, intrinsic :: iso_c_binding, only: c_int32_t, c_ptr
    integer(c_int32_t) :: fckit__Owned__owners
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

FCKIT_FINAL subroutine fckit_owned_object__final_auto(this)
#ifdef _CRAYFTN
  use, intrinsic :: iso_c_binding, only : c_loc, c_null_ptr
#endif
  type(fckit_owned_object), intent(inout) :: this
#if FCKIT_FINAL_DEBUGGING
  FCKIT_WRITE_LOC
  FCKIT_WRITE_DEBUG "BEGIN fckit_owned_object__final_auto    address:", c_ptr_to_loc(this%cpp_object_ptr)
#endif
  ! Guard necessary for Cray compiler...
  ! ... when "this" has already been deallocated, and then
  ! fckit_owned_object__final_auto is called...
#ifdef _CRAYFTN
  if( c_loc(this) == c_null_ptr ) then
#if FCKIT_FINAL_DEBUGGING
    FCKIT_WRITE_LOC
    FCKIT_WRITE_DEBUG "_CRAYFTN not calling final()"
    FCKIT_WRITE_LOC
    FCKIT_WRITE_DEBUG "END fckit_owned_object__final_auto    address:", c_ptr_to_loc(this%cpp_object_ptr)
#endif
    return
  endif
#endif

  if( .not. this%is_null() ) then
#if FCKIT_FINAL_DEBUGGING
  FCKIT_WRITE_LOC
  FCKIT_WRITE_DEBUG "this%final() address:",c_ptr_to_loc(this%cpp_object_ptr)
#endif
  call type_final(this)
  endif
#if FCKIT_FINAL_DEBUGGING
  FCKIT_WRITE_LOC
  FCKIT_WRITE_DEBUG "END fckit_owned_object__final_auto    address:", c_ptr_to_loc(this%cpp_object_ptr)
#endif
end subroutine

subroutine fckit_owned_object__delete( this )
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr, c_f_procpointer, c_associated, c_null_ptr
  use fckit_c_interop_module, only : fckit_c_deleter_interface
  class(fckit_owned_object), intent(inout) :: this
  procedure(fckit_c_deleter_interface), pointer :: deleter
#if FCKIT_FINAL_DEBUGGING
  FCKIT_WRITE_LOC
  FCKIT_WRITE_DEBUG "fckit_owned_object__delete"
#endif
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

#if FCKIT_FINAL_DEBUGGING
  FCKIT_WRITE_LOC
  FCKIT_WRITE_DEBUG "fckit_owned_object__final address BEGIN: ", c_ptr_to_loc(this%cpp_object_ptr)
#endif
  if( this%is_null() ) then
#if FCKIT_FINAL_DEBUGGING
    FCKIT_WRITE_LOC
    FCKIT_WRITE_DEBUG "fckit_owned_object__final  (uninitialised --> no-op),  address: ", c_ptr_to_loc(this%cpp_object_ptr)
#endif
    return
  endif

#if FCKIT_FINAL_DEBUGGING
  if( this%return_value ) then
    FCKIT_WRITE_LOC
    FCKIT_WRITE_DEBUG "fckit_owned_object__final on return value, owners = ", type_owners(this), "    address: ", loc(this%cpp_object_ptr)
  endif
#endif

  if( this%owners() > 0 ) then
#if FCKIT_FINAL_DEBUGGING
    FCKIT_WRITE_LOC
    FCKIT_WRITE_DEBUG "fckit_owned_object__final  , owners = ", type_owners(this), "   address: ", c_ptr_to_loc(this%cpp_object_ptr)
#endif
    call this%detach()
    if( this%owners() == 0 ) then
      call fckit_owned_object__delete(this)
    endif
  endif
#if FCKIT_FINAL_DEBUGGING
  FCKIT_WRITE_LOC
  FCKIT_WRITE_DEBUG "fckit_owned_object__final  , owners = ", type_owners(this), "    address: ", c_ptr_to_loc(this%cpp_object_ptr)
#endif
  call this%reset_c_ptr()
end subroutine

subroutine type_final(this)
  type(fckit_owned_object), intent(inout) :: this

#if FCKIT_FINAL_DEBUGGING
  FCKIT_WRITE_LOC
  FCKIT_WRITE_DEBUG "fckit_owned_object__final address BEGIN: ", c_ptr_to_loc(this%cpp_object_ptr)
#endif
  if( type_is_null(this) ) then
#if FCKIT_FINAL_DEBUGGING
    FCKIT_WRITE_LOC
    FCKIT_WRITE_DEBUG "fckit_owned_object__final  (uninitialised --> no-op),  address: ", c_ptr_to_loc(this%cpp_object_ptr)
#endif
    return
  endif

#if FCKIT_FINAL_DEBUGGING
  if( this%return_value ) then
    FCKIT_WRITE_LOC
    FCKIT_WRITE_DEBUG "fckit_owned_object__final on return value, owners = ", type_owners(this), "    address: ", loc(this%cpp_object_ptr)
  endif
#endif

  if( type_owners(this) > 0 ) then
#if FCKIT_FINAL_DEBUGGING
    FCKIT_WRITE_LOC
    FCKIT_WRITE_DEBUG "fckit_owned_object__final  , owners = ", type_owners(this), "   address: ", c_ptr_to_loc(this%cpp_object_ptr)
#endif
    call type_detach(this)
    if( type_owners(this) == 0 ) then
      call fckit_owned_object__delete(this)
    endif
  endif
#if FCKIT_FINAL_DEBUGGING
  FCKIT_WRITE_LOC
  FCKIT_WRITE_DEBUG "fckit_owned_object__final  , owners = ", type_owners(this), "    address: ", c_ptr_to_loc(this%cpp_object_ptr)
#endif
  call type_reset_c_ptr(this)
end subroutine


subroutine assignment_operator(this,other)
  class(fckit_owned_object), intent(inout) :: this
  class(fckit_owned_object), intent(in)    :: other
  if( type_is_null(other) ) then
    write(0,*) "ERROR! other was not initialised"
  endif
#if FCKIT_FINAL_DEBUGGING
  if( other%return_value ) then
    FCKIT_WRITE_LOC
    FCKIT_WRITE_DEBUG "other is a return value"
  endif
#endif
  if( this /= other ) then
#if FCKIT_FINAL_DEBUGGING
    if( type_is_null(this) ) then
      FCKIT_WRITE_LOC
      FCKIT_WRITE_DEBUG "assignment_operator of uninitialised"
    else
      FCKIT_WRITE_LOC
      FCKIT_WRITE_DEBUG "assignment_operator of initialised"
    endif
#endif
    call this%final()
    call this%reset_c_ptr( other%cpp_object_ptr, other%deleter )
#if FCKIT_FINAL_DEBUGGING
    FCKIT_WRITE_LOC
    FCKIT_WRITE_DEBUG "  \-> owners ", this%owners()
#endif
  else
#if FCKIT_FINAL_DEBUGGING
    FCKIT_WRITE_LOC
    FCKIT_WRITE_DEBUG "assignment_operator ( obj_out = obj_in )"
#endif
  endif
  call this%assignment_operator_hook(other)
 end subroutine

subroutine attach(this)
  class(fckit_owned_object), intent(inout) :: this
  if( .not. this%is_null() ) then
    call fckit__Owned__attach(this%cpp_object_ptr)
#if FCKIT_FINAL_DEBUGGING
    FCKIT_WRITE_LOC
    FCKIT_WRITE_DEBUG "attach"
#endif
  endif
end subroutine

subroutine type_attach(this)
  type(fckit_owned_object), intent(inout) :: this
#if FCKIT_FINAL_DEBUGGING
  FCKIT_WRITE_LOC
  FCKIT_WRITE_DEBUG "attach  address:",c_ptr_to_loc(this%cpp_object_ptr)
#endif
  if( .not. type_is_null(this) ) then
    call fckit__Owned__attach(this%cpp_object_ptr)
#if FCKIT_FINAL_DEBUGGING
    FCKIT_WRITE_LOC
    FCKIT_WRITE_DEBUG "attach  address:",c_ptr_to_loc(this%cpp_object_ptr)
#endif
  endif
end subroutine

subroutine detach(this)
  class(fckit_owned_object), intent(inout) :: this
  if( .not. this%is_null() ) then
    call fckit__Owned__detach(this%cpp_object_ptr)
#if FCKIT_FINAL_DEBUGGING
    FCKIT_WRITE_LOC
    FCKIT_WRITE_DEBUG "detach"
#endif
  endif
end subroutine

subroutine type_detach(this)
  type(fckit_owned_object), intent(inout) :: this
  if( .not. type_is_null(this) ) then
    call fckit__Owned__detach(this%cpp_object_ptr)
#if FCKIT_FINAL_DEBUGGING
    FCKIT_WRITE_LOC
    FCKIT_WRITE_DEBUG "detach"
#endif
  endif
end subroutine

function owners(this)
  integer(c_int32_t) :: owners
  class(fckit_owned_object), intent(in) :: this
  if( this%is_null() ) then
    owners = 0
  else
    owners = fckit__Owned__owners(this%cpp_object_ptr)
  endif
end function

function type_owners(this)
  integer(c_int32_t) :: type_owners
  type(fckit_owned_object), intent(in) :: this
  if( type_is_null(this) ) then
    type_owners = 0
  else
    type_owners = fckit__Owned__owners(this%cpp_object_ptr)
  endif
end function

subroutine return(this)
  !! Transfer ownership to left hand side of "assignment(=)"
  class(fckit_owned_object), intent(inout) :: this
#if FCKIT_FINAL_FUNCTION_RESULT
  ! Cray example
  ! final will be called, which will detach, so attach first
  if( type_owners(this) == 0 ) then
#if FCKIT_FINAL_DEBUGGING
    FCKIT_WRITE_LOC
    FCKIT_WRITE_DEBUG "return --> attach"
#endif
    call type_attach(this)
  endif
#else
  ! final will not be called, so detach manually
  if( type_owners(this) > 0 ) then
#if FCKIT_FINAL_DEBUGGING
    FCKIT_WRITE_LOC
    FCKIT_WRITE_DEBUG "return --> detach"
#endif
    call type_detach(this)
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


subroutine consumed(this)
  class(fckit_owned_object), intent(in) :: this
  type(fckit_owned_object) :: consumed_object
  consumed_object = this
  call consumed_object%final()
end subroutine


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

function type_is_null(this)
  use, intrinsic :: iso_c_binding, only: c_associated
  logical :: type_is_null
  type(fckit_owned_object) :: this
  if( c_associated( this%cpp_object_ptr ) ) then
          type_is_null = .False.
  else
          type_is_null = .True.
  endif
end function


logical function equal(obj1,obj2)
  use fckit_c_interop_module, only : c_ptr_compare_equal
  class(fckit_owned_object), intent(in) :: obj1
  class(fckit_owned_object), intent(in) :: obj2
  equal = c_ptr_compare_equal(obj1%cpp_object_ptr,obj2%cpp_object_ptr)
end function

logical function not_equal(obj1,obj2)
  use fckit_c_interop_module, only : c_ptr_compare_equal
  class(fckit_owned_object), intent(in) :: obj1
  class(fckit_owned_object), intent(in) :: obj2
  if( c_ptr_compare_equal(obj1%cpp_object_ptr,obj2%cpp_object_ptr) ) then
    not_equal = .False.
  else
    not_equal = .True.
  endif
end function

logical function type_not_equal(obj1,obj2)
  use fckit_c_interop_module, only : c_ptr_compare_equal
  type(fckit_owned_object), intent(in) :: obj1
  type(fckit_owned_object), intent(in) :: obj2
  if( c_ptr_compare_equal(obj1%cpp_object_ptr,obj2%cpp_object_ptr) ) then
    type_not_equal = .False.
  else
    type_not_equal = .True.
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
    call type_attach(this)

    if( present(deleter) ) then
      this%deleter = deleter
    else
      this%deleter = fckit_c_deleter(fckit__delete_Owned)
    endif

  else
    this%cpp_object_ptr = c_null_ptr
    this%deleter = c_null_funptr
  endif

end subroutine

subroutine type_reset_c_ptr(this,cptr,deleter)
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr, c_null_funptr
  use fckit_c_interop_module
  type(fckit_owned_object) :: this
  type(c_ptr), optional :: cptr
  type(c_funptr), optional :: deleter
  if( present(cptr) ) then
    this%cpp_object_ptr = cptr
    call type_attach(this)

    if( present(deleter) ) then
      this%deleter = deleter
    else
      this%deleter = fckit_c_deleter(fckit__delete_Owned)
    endif

  else
    this%cpp_object_ptr = c_null_ptr
    this%deleter = c_null_funptr
  endif

end subroutine


end module
