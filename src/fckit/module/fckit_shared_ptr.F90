! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/defines.h"

module fckit_shared_ptr_module
implicit none
private

!========================================================================
! Public interface

public fckit_shared_ptr
public fckit_make_shared

!========================================================================

type :: fckit_shared_ptr
  class(*), pointer, private :: shared_ptr_ => null()
  integer,  pointer, private :: refcount_   => null()

#if FCKIT_FINAL_DEBUGGING
    logical :: return_value = .false.
#endif

contains
  procedure, public :: final => fckit_shared_ptr__final

#if FCKIT_HAVE_FINAL
  final :: fckit_shared_ptr__final_auto
#endif

  procedure, private :: clear_shared_ptr
  procedure, private :: reset_shared_ptr
  generic, private :: reset => clear_shared_ptr, reset_shared_ptr
  generic, public :: assignment(=) => reset_shared_ptr
  procedure, public :: owners
  procedure, public :: attach
  procedure, public :: detach
  procedure, public :: return
  procedure, public :: shared_ptr => get_shared_ptr

  procedure, public :: shared_ptr_cast
  procedure, public :: clear
  procedure, public :: share

  procedure, public :: consumed

end type

!========================================================================
CONTAINS
! =============================================================================

subroutine fckit_finalise( shared_ptr )
  use fckit_final_module, only: fckit_final
  class(*), pointer :: shared_ptr
  select type(shared_ptr)
    class is(fckit_final)
#if FCKIT_FINAL_DEBUGGING
      write(0,*) "fckit_final%final()"
#endif
      call shared_ptr%final()
  end select
end subroutine

subroutine fckit_shared_ptr__final_auto(this)
  type(fckit_shared_ptr), intent(inout) :: this
#if FCKIT_FINAL_DEBUGGING
  write(0,*) "fckit_shared_ptr__final_auto"
#endif
  ! Guard necessary for Cray compiler...
  ! ... when "this" has already been deallocated, and then
  ! fckit_shared_ptr__final_auto is called...
#ifdef _CRAYFTN
  if( c_loc(this) == c_null_ptr ) then
    return
  endif
#endif

  call this%final()
end subroutine

subroutine fckit_shared_ptr__final(this)
  use, intrinsic :: iso_c_binding, only : c_loc, c_associated, c_null_ptr
  class(fckit_shared_ptr), intent(inout) :: this

  if( .not. associated(this%shared_ptr_) ) then
#if FCKIT_FINAL_DEBUGGING
    write(0,*) "fckit_shared_ptr__final  (uninitialised --> no-op)"
#endif
    call this%clear()
    return
  endif

#if FCKIT_FINAL_DEBUGGING
  if( this%return_value ) then
    write(0,*) "fckit_shared_ptr__final on return value, owners = ", this%owners()
  endif
#endif

  if( this%owners() > 0 ) then
#if FCKIT_FINAL_DEBUGGING
    write(0,*) "fckit_shared_ptr__final  , owners = ", this%owners()
#endif
    call this%detach()
    if( this%owners() == 0 ) then
      call fckit_finalise(this%shared_ptr_)
      deallocate(this%shared_ptr_)
      deallocate(this%refcount_)
    endif
  endif

  call this%clear()
end subroutine

subroutine clear_shared_ptr(obj_out)
  use, intrinsic :: iso_c_binding, only : c_loc, c_associated
  class(fckit_shared_ptr), intent(inout) :: obj_out
  if( associated( obj_out%shared_ptr_ ) ) then
    nullify(obj_out%shared_ptr_)
    nullify(obj_out%refcount_)
  endif
end subroutine

subroutine clear(obj_out)
  use, intrinsic :: iso_c_binding, only : c_loc, c_associated
  class(fckit_shared_ptr), intent(inout) :: obj_out
  call obj_out%clear_shared_ptr()
end subroutine


subroutine reset_shared_ptr(obj_out,obj_in)
  use, intrinsic :: iso_c_binding, only : c_loc, c_associated
  class(fckit_shared_ptr), intent(inout) :: obj_out
  class(fckit_shared_ptr), intent(in)    :: obj_in
  if( .not. associated( obj_in%shared_ptr_) ) then
    write(0,*) "ERROR! obj_in was not initialised"
  endif
#if FCKIT_FINAL_DEBUGGING
  if( obj_in%return_value ) then
    write(0,*) "obj_in is a return value"
  endif
#endif
!  if( .not. associated( obj_out%shared_ptr_, obj_in%shared_ptr_ ) ) then
#if FCKIT_FINAL_DEBUGGING
    if( .not. associated( obj_out%shared_ptr_ ) ) then
      write(0,*) "reset_shared_ptr of uninitialised"
    else
      write(0,*) "reset_shared_ptr of initialised"
    endif
#endif
    call obj_out%final()
    obj_out%shared_ptr_ => obj_in%shared_ptr_
    obj_out%refcount_   => obj_in%refcount_
    if( obj_out%shared_ptr_cast() ) then
      call obj_out%attach()
    else
      call obj_out%clear()
      call bad_cast()
    endif
!  else
!#if FCKIT_FINAL_DEBUGGING
!    write(0,*) "reset_shared_ptr ( identity )"
!#endif
!    if( obj_out%shared_ptr_cast() ) then ; endif
!  endif
end subroutine

subroutine attach(this)
  class(fckit_shared_ptr), intent(inout) :: this
  if( associated(this%shared_ptr_) ) then
    this%refcount_ = this%refcount_ + 1
  endif
end subroutine

subroutine detach(this)
  class(fckit_shared_ptr), intent(inout) :: this
  if( associated(this%shared_ptr_) ) then
    this%refcount_ = max(0, this%refcount_ - 1)
  endif
end subroutine

function owners(this)
  integer :: owners
  class(fckit_shared_ptr), intent(in) :: this
  if( associated( this%shared_ptr_) ) then
    owners = this%refcount_
  else
    owners = 0
  endif
end function

subroutine return(this)
  !! Transfer ownership to left hand side of "assignment(=)"
  class(fckit_shared_ptr), intent(inout) :: this
#if FCKIT_FINAL_FUNCTION_RESULT
  ! Cray example
  ! final will be called, which will detach, so attach first
  if( this%owners() == 0 ) then
#if FCKIT_FINAL_DEBUGGING
        write(0,*) "return --> detach"
#endif
    call this%attach()
  endif
#else
  ! final will not be called, so detach manually
  if( this%owners() > 0 ) then
#if FCKIT_FINAL_DEBUGGING
    write(0,*) "return --> detach"
#endif
    call this%detach()
  endif
#endif
#if FCKIT_FINAL_DEBUGGING
  this%return_value = .true.
#endif
end subroutine

function get_shared_ptr(this) result(shared_ptr)
  class(*), pointer :: shared_ptr
  class(fckit_shared_ptr), intent(in) :: this
  shared_ptr => this%shared_ptr_
end function

function shared_ptr_cast(this) result(success)
  class(fckit_shared_ptr) :: this
  logical :: success
  success = .true.
  FCKIT_SUPPRESS_UNUSED( this )
end function

function fckit_make_shared( ptr ) result(this)
  type(fckit_shared_ptr) :: this
  class(*), target :: ptr
#if FCKIT_FINAL_DEBUGGING
  write(0,*) "begin fckit_make_shared"
#endif
  call this%share( ptr )
  call this%return()
#if FCKIT_FINAL_DEBUGGING
  write(0,*) " this%owners() = ", this%owners()
  write(0,*) "end   fckit_make_shared"
#endif
end function

subroutine share( this, ptr )
  class(fckit_shared_ptr) :: this
  class(*), target :: ptr
  this%shared_ptr_ => ptr
  allocate(this%refcount_)
  this%refcount_ = 0
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
  class(fckit_shared_ptr), intent(in) :: this
  type(fckit_shared_ptr) :: consumed_object
  consumed_object = this
  call consumed_object%final()
end subroutine



end module
