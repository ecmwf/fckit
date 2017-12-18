! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/defines.h"

module fckit_shared_ptr_module
use, intrinsic :: iso_c_binding, only : c_ptr, c_null_ptr
implicit none
private

!========================================================================
! Public interface

public fckit_shared_ptr
public fckit_make_shared
public fckit_refcount
public fckit_refcount_interface, fckit_external, fckit_owned

!========================================================================

type, abstract :: fckit_refcount
contains
procedure(fckit_refcount_owners), public, deferred :: owners
procedure(fckit_refcount_attach), public, deferred :: attach
procedure(fckit_refcount_detach), public, deferred :: detach
end type

interface
  function fckit_refcount_owners(this)
    import fckit_refcount
    integer :: fckit_refcount_owners
    class(fckit_refcount), intent(in) :: this
  end function
  subroutine fckit_refcount_attach(this)
    import fckit_refcount
    class(fckit_refcount), intent(inout) :: this
  end subroutine
  subroutine fckit_refcount_detach(this)
    import fckit_refcount
    class(fckit_refcount), intent(inout) :: this
  end subroutine
end interface

abstract interface
  subroutine fckit_refcount_interface(refcount,shared_ptr)
    import fckit_refcount
    class(fckit_refcount), pointer, intent(inout) :: refcount
    class(*), target, intent(in) :: shared_ptr
  end subroutine
end interface



type, extends(fckit_refcount) :: fckit_refcount_external
  integer, private :: refcount_ = 0
  contains
  procedure, public :: owners => fckit_refcount_external_owners
  procedure, public :: attach => fckit_refcount_external_attach
  procedure, public :: detach => fckit_refcount_external_detach
end type

interface

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

  function fckit__new_Owned() bind(c,name="fckit__new_Owned")
    use, intrinsic :: iso_c_binding, only: c_ptr
    type(c_ptr) :: fckit__new_Owned
  end function

  subroutine fckit__delete_Owned(this) bind(c,name="fckit__delete_Owned")
    use, intrinsic :: iso_c_binding, only: c_ptr
    type(c_ptr), value :: this
  end subroutine

end interface

type, extends(fckit_refcount) :: fckit_eckit_Owned
  type(c_ptr), private :: eckit_Owned_ = c_null_ptr
  contains
  procedure, public :: owners => fckit_eckit_Owned_owners
  procedure, public :: attach => fckit_eckit_Owned_attach
  procedure, public :: detach => fckit_eckit_Owned_detach
end type



type :: fckit_shared_ptr
  class(*), pointer, private :: shared_ptr_ => null()
  class(fckit_refcount), pointer , private :: refcount_ => null()

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

function new_fckit_refcount_external() result(ptr)
  class(fckit_refcount), pointer :: ptr
  allocate( fckit_refcount_external::ptr )
end function

function new_fckit_refcount_Owned(cptr) result(ptr)
  class(fckit_eckit_Owned), pointer :: ptr
  type(c_ptr) :: cptr
  allocate( ptr )
  ptr%eckit_Owned_ = cptr
end function

subroutine allocate_fckit_external(refcount,shared_ptr)
  class(fckit_refcount), pointer, intent(inout) :: refcount
  class(*), target, intent(in) :: shared_ptr
  allocate( fckit_refcount_external::refcount )
end subroutine

function fckit_external() result(funptr)
  procedure(fckit_refcount_interface), pointer :: funptr
  funptr => allocate_fckit_external
end function

subroutine allocate_fckit_refcount_owned(refcount,shared_ptr)
  use fckit_object_module, only : fckit_object
  use, intrinsic :: iso_c_binding, only : c_ptr
  class(fckit_refcount), pointer, intent(inout) :: refcount
  class(*), target, intent(in) :: shared_ptr
  type(c_ptr) :: cptr
  allocate( fckit_eckit_Owned::refcount )
  select type( shared_ptr )
    class is( fckit_object )
      cptr = shared_ptr%c_ptr()
  end select
  select type( refcount )
    class is( fckit_eckit_Owned )
      refcount%eckit_Owned_ = cptr
  end select
end subroutine

function fckit_owned() result(funptr)
  procedure(fckit_refcount_interface), pointer :: funptr
  funptr => allocate_fckit_refcount_owned
end function

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
    call this%refcount_%attach()
  endif
end subroutine

subroutine detach(this)
  class(fckit_shared_ptr), intent(inout) :: this
  if( associated(this%shared_ptr_) ) then
    call this%refcount_%detach()
  endif
end subroutine

function owners(this)
  integer :: owners
  class(fckit_shared_ptr), intent(in) :: this
  if( associated( this%shared_ptr_) ) then
    owners = this%refcount_%owners()
  else
    owners = 0
  endif
end function


subroutine fckit_refcount_external_attach(this)
  class(fckit_refcount_external), intent(inout) :: this
  this%refcount_ = this%refcount_ + 1
end subroutine

subroutine fckit_refcount_external_detach(this)
  class(fckit_refcount_external), intent(inout) :: this
  this%refcount_ = max(0, this%refcount_ - 1)
end subroutine

function fckit_refcount_external_owners(this) result(owners)
  integer :: owners
  class(fckit_refcount_external), intent(in) :: this
  owners = this%refcount_
end function


subroutine fckit_eckit_Owned_attach(this)
  class(fckit_eckit_Owned), intent(inout) :: this
  call fckit__Owned__attach(this%eckit_Owned_)
end subroutine

subroutine fckit_eckit_Owned_detach(this)
  class(fckit_eckit_Owned), intent(inout) :: this
  call fckit__Owned__detach(this%eckit_Owned_)
end subroutine

function fckit_eckit_Owned_owners(this) result(owners)
  integer :: owners
  class(fckit_eckit_Owned), intent(in) :: this
  owners = fckit__Owned__owners(this%eckit_Owned_)
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

subroutine share( this, ptr, refcount )
  use, intrinsic :: iso_c_binding, only : c_funptr
  class(fckit_shared_ptr) :: this
  class(*), target :: ptr
  procedure(fckit_refcount_interface), optional :: refcount
  this%shared_ptr_ => ptr
  if( present(refcount) ) then
    call refcount(this%refcount_, this%shared_ptr_)
  else
    call allocate_fckit_external( this%refcount_, this%shared_ptr_ )
  endif
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
