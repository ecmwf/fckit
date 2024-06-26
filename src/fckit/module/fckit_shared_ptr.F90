! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fckit.h"

#if FCKIT_FINAL_DEBUGGING
#define FCKIT_WRITE_LOC write(0,'(A,I0,A)',advance='NO') "fckit_shared_ptr.F90 @ ",__LINE__,'  : '
#define FCKIT_WRITE(unit,format) write(unit,format)
#endif


module fckit_shared_ptr_module

#if FCKIT_HAVE_ECKIT
use fckit_refcount_module, only : &
  & fckit_refcount, &
  & fckit_refcount_interface, &
  & fckit_external, &
  & fckit_owned
#else
use fckit_refcount_module, only : &
  & fckit_refcount, &
  & fckit_refcount_interface, &
  & fckit_external
#endif

implicit none
private

!========================================================================
! Public interface

public fckit_shared_ptr
public fckit_make_shared
public fckit_refcount
public fckit_refcount_interface
public fckit_external

#if FCKIT_HAVE_ECKIT
public fckit_owned
#endif

!========================================================================

type :: fckit_shared_ptr
  class(*), pointer, private :: shared_ptr_ ! => null()
  class(fckit_refcount), pointer , private :: refcount_ => null()

  logical, private :: is_null_ = .true.
    ! Compiler bug for gcc > 7 : we may not default assign shared_ptr_ to null()
    ! Instead we use this `is_null` variable (See JIRA issue FCKIT-21)

  logical, private :: return_value = .false.
    ! This variable should not be necessary,
    ! but seems to overcome compiler issues ( gfortran 5.3, 6.3 )

contains
  procedure, public :: final => fckit_shared_ptr__final
  procedure, public :: fckit_shared_ptr__final

#if FCKIT_HAVE_FINAL
  final :: fckit_shared_ptr__final_auto
#endif

  procedure, private :: clear_shared_ptr
  procedure, public :: reset_shared_ptr
  generic, public :: reset => clear_shared_ptr, reset_shared_ptr
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
!========================================================================

subroutine deallocate_shared_ptr( shared_ptr )
  use fckit_final_module, only: fckit_final
  class(*), pointer :: shared_ptr
#ifndef __ibmxl__
  deallocate(shared_ptr)
#else
  ! FCKIT-17: Runtime error when deallocating unlimited polymorphic pointer
  !  MEMORY LEAK!!!
  ! IBM XL COMPILER AT RUNTIME GIVES ERROR:
  !     1525-109 Error encountered while attempting to deallocate a data object.
  !     The program will stop.
#endif
  nullify( shared_ptr )
end subroutine

subroutine fckit_finalise( shared_ptr )
  use fckit_final_module, only: fckit_final
  use fckit_object_module, only: fckit_object
  class(*), pointer :: shared_ptr
  select type( shared_ptr)
#ifdef _CRAYFTN
  ! Cray compiler cce/14 has problem with typebound procedure if it is using 'class'
  type is(fckit_object)
#if FCKIT_FINAL_DEBUGGING
      write(0,*) "fckit_object%final()"
#endif
      call shared_ptr%final()
#endif
    class is(fckit_final)
#if FCKIT_FINAL_DEBUGGING
      write(0,*) "fckit_final%final()"
#endif
      call shared_ptr%final()
  end select
end subroutine

FCKIT_FINAL subroutine fckit_shared_ptr__final_auto(this)
#ifdef _CRAYFTN
  use, intrinsic :: iso_c_binding, only : c_loc, c_null_ptr
#endif
  type(fckit_shared_ptr), intent(inout) :: this
#if FCKIT_FINAL_DEBUGGING
  FCKIT_WRITE_LOC
  FCKIT_WRITE(0,*) "fckit_shared_ptr__final_auto"
#endif

#ifdef _CRAYFTN
  ! Guard necessary for Cray compiler...
  ! ... when "this" has already been deallocated, and then
  ! fckit_shared_ptr__final_auto is called...
  if( c_loc(this) == c_null_ptr ) then
    return
  endif
#endif

  if (.not. this%return_value) then
    if( .not. this%is_null_ ) then
      if( this%owners() > 0 ) then
        call this%final()
      endif
    endif
  else
#if FCKIT_FINAL_DEBUGGING
    FCKIT_WRITE_LOC
    FCKIT_WRITE(0,'(A)') "Applying return-value-optimisation during assignment_operator, this%final not called"
#endif
  endif
end subroutine

subroutine fckit_shared_ptr__final(this)
  class(fckit_shared_ptr), intent(inout) :: this

#if FCKIT_FINAL_DEBUGGING
    write(0,*) "fckit_shared_ptr__final"
#endif

  if( this%is_null_ ) then
#if FCKIT_FINAL_DEBUGGING
    FCKIT_WRITE(0,*) "fckit_shared_ptr__final  (uninitialised --> no-op)"
#endif
    call this%clear()
    return
  endif

#if FCKIT_FINAL_DEBUGGING
  if( this%return_value ) then
    FCKIT_WRITE(0,'(A,I0)') " fckit_shared_ptr__final on return value, owners = ", this%owners()
  endif
#endif

  if( this%owners() >= 0 ) then
#if FCKIT_FINAL_DEBUGGING
    FCKIT_WRITE(0,'(A,I0)') " fckit_shared_ptr__final  , owners = ", this%owners()
#endif
    call this%detach()
    if( this%owners() == 0 ) then
#if FCKIT_FINAL_DEBUGGING
      FCKIT_WRITE(0,*) " + call fckit_finalise(this%shared_ptr_)"
#endif
      call fckit_finalise(this%shared_ptr_)
      call deallocate_shared_ptr(this%shared_ptr_)
      deallocate(this%refcount_)
    endif
  endif

  call this%clear()
end subroutine

subroutine clear_shared_ptr(obj_out)
  class(fckit_shared_ptr), intent(inout) :: obj_out
  if( .not. obj_out%is_null_ ) then
    nullify(obj_out%shared_ptr_)
    nullify(obj_out%refcount_)
    obj_out%is_null_ = .true.
  endif
end subroutine

subroutine clear(obj_out)
  class(fckit_shared_ptr), intent(inout) :: obj_out
  call obj_out%clear_shared_ptr()
end subroutine


subroutine reset_shared_ptr(obj_out,obj_in)
  class(fckit_shared_ptr), intent(inout) :: obj_out
  class(fckit_shared_ptr), intent(in)    :: obj_in
#if FCKIT_FINAL_DEBUGGING
  FCKIT_WRITE(0,*) "fckit_shared_ptr::reset_shared_ptr(out,in)"
#endif

  if( obj_in%is_null_ ) then
    write(0,*) "ERROR! obj_in was not initialised"
  endif

  if( obj_out%is_null_ ) then
    nullify( obj_out%shared_ptr_ ) ! so that we can check association
  endif

  if( .not. associated( obj_out%shared_ptr_, obj_in%shared_ptr_ ) ) then
#if FCKIT_FINAL_DEBUGGING
    if( obj_out%is_null_ ) then
      FCKIT_WRITE(0,'(A)') "reset_shared_ptr of uninitialised"
    else
      FCKIT_WRITE(0,'(A)') "reset_shared_ptr of initialised"
    endif
#endif
    call obj_out%final()
    obj_out%shared_ptr_ => obj_in%shared_ptr_
    obj_out%refcount_   => obj_in%refcount_
    obj_out%is_null_ = .not. associated( obj_out%shared_ptr_ )
    if( obj_out%shared_ptr_cast() ) then
      call obj_out%attach()
    else
      call obj_out%clear()
      call bad_cast()
    endif
  else
#if FCKIT_FINAL_DEBUGGING
    FCKIT_WRITE(0,*) "reset_shared_ptr ( obj_out = obj_in )"
#endif
    if( obj_out%shared_ptr_cast() ) then ; endif
  endif
end subroutine

subroutine attach(this)
  class(fckit_shared_ptr), intent(inout) :: this
  if( .not. this%is_null_ ) then
    call this%refcount_%attach()
  endif
end subroutine

subroutine detach(this)
  class(fckit_shared_ptr), intent(inout) :: this
  if( .not. this%is_null_ ) then
    call this%refcount_%detach()
  endif
end subroutine

function owners(this)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: owners
  class(fckit_shared_ptr), intent(in) :: this
  if( .not. this%is_null_ ) then
    owners = this%refcount_%owners()
  else
    owners = 0
  endif
end function

subroutine return(this)
  !! Transfer ownership to left hand side of "assignment(=)"
  class(fckit_shared_ptr), intent(inout) :: this
  this%return_value = .true.
  call this%detach()
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
  FCKIT_WRITE(0,*) "begin fckit_make_shared"
#endif
  call this%share( ptr )
  call this%return()
#if FCKIT_FINAL_DEBUGGING
  write(0,*) " this%owners() = ", this%owners()
  write(0,*) "end   fckit_make_shared"
#endif
end function

subroutine share( this, ptr, refcount )
  use, intrinsic :: iso_c_binding, only : c_funptr, c_f_procpointer
  class(fckit_shared_ptr) :: this
  class(*), target :: ptr
  type(c_funptr), optional :: refcount
  procedure(fckit_refcount_interface), pointer :: opt_refcount
  if( present(refcount) ) then
    call c_f_procpointer( refcount, opt_refcount )
  else
    call fckit_external(opt_refcount)
  endif
  this%shared_ptr_ => ptr
  this%is_null_ = .not. associated( this%shared_ptr_ )
  call opt_refcount(this%refcount_, this%shared_ptr_)
  call this%refcount_%attach()
#if FCKIT_FINAL_DEBUGGING
  FCKIT_WRITE(0,*) "share --> attach"
#endif
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
