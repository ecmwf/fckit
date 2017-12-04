! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fckit_defines.h"

module fckit_shared_object_module
use fckit_object_module, only : fckit_object, fckit_c_deleter
implicit none
private

!========================================================================
! Public interface

public fckit_shared_object
public fckit_c_deleter

!========================================================================

type :: fckit_shared_object
  class(fckit_object), pointer, public  :: shared_ptr => null()
  integer,             pointer, private :: refcount   => null()
contains

  procedure, public :: final => fckit_shared_object__final

#ifdef EC_HAVE_Fortran_FINALIZATION
  final :: fckit_shared_object__final_auto
#endif

  procedure, private :: reset
  generic, public :: assignment(=) => reset
  procedure, public :: owners
  procedure, public :: attach
  procedure, public :: detach
  procedure, public :: return
  procedure, public :: shared_ptr_cast

  procedure, public :: make_shared

  procedure, public  :: c_ptr => fckit_shared_object_c_ptr
  procedure, private :: fckit_shared_object_c_ptr

end type

private fckit_object

!========================================================================
CONTAINS
!========================================================================

subroutine fckit_shared_object__final_auto(this)
  type(fckit_shared_object), intent(inout) :: this
  call this%final()
end subroutine

subroutine fckit_shared_object__final(this)
  class(fckit_shared_object), intent(inout) :: this
  if( associated(this%shared_ptr) ) then
    if( this%owners() > 0 ) then
      call this%detach()
      if( this%owners() == 0 ) then
        call this%shared_ptr%final()
        deallocate(this%shared_ptr)
        deallocate(this%refcount)
      endif
    endif
  endif
  nullify(this%shared_ptr)
  nullify(this%refcount)
end subroutine

subroutine reset(obj_out,obj_in)
  use, intrinsic :: iso_c_binding, only : c_loc, c_associated
  class(fckit_shared_object), intent(inout) :: obj_out
  class(fckit_shared_object), intent(in)    :: obj_in
  if( .not. associated( obj_out%shared_ptr, obj_in%shared_ptr ) ) then
    call obj_out%final()
    obj_out%shared_ptr => obj_in%shared_ptr
    obj_out%refcount   => obj_in%refcount
    if( obj_out%shared_ptr_cast() ) then
      call obj_out%attach()
    else
      nullify(obj_out%shared_ptr)
      nullify(obj_out%refcount)
      call fckit_shared_object_bad_cast
    endif
  else
    if( obj_out%shared_ptr_cast() ) then ; endif
  endif
end subroutine

subroutine attach(this)
  class(fckit_shared_object), intent(in) :: this
  if( associated(this%shared_ptr) ) then
    this%refcount = this%refcount + 1
  endif
end subroutine

subroutine detach(this)
  class(fckit_shared_object), intent(in) :: this
  if( associated(this%shared_ptr) ) then
    this%refcount = max(0, this%refcount - 1)
  endif
end subroutine

function owners(this)
  integer :: owners
  class(fckit_shared_object), intent(in) :: this
  if( associated( this%shared_ptr) ) then
    owners = this%refcount
  else
    owners = 0
  endif
end function

subroutine return(this)
  !! Transfer ownership to left hand side of "assignment(=)"
  class(fckit_shared_object), intent(inout) :: this
#ifdef Fortran_FINAL_FUNCTION_RESULT
  ! (auto) final will be called, which will detach, so attach first
  call this%attach()
#else
  ! final will not be called, so detach manually
  if( this%owners() > 0 ) call this%detach()
#endif
end subroutine

function shared_ptr_cast(this) result(success)
  class(fckit_shared_object) :: this
  logical :: success
  success = .true.
end function

subroutine fckit_shared_object_bad_cast(message)
  character(len=*), optional :: message
  if( present(message) ) then
    write(0,'("ERROR: bad_cast -- ",A)') message
  else
    write(0,'("ERROR: bad cast")')
  endif
end subroutine

subroutine make_shared(this, cptr, deleter)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_funptr
  class(fckit_shared_object) :: this
  type(c_ptr), optional :: cptr
  type(c_funptr), optional :: deleter
  allocate( fckit_object::this%shared_ptr )
  if( present( cptr ) ) then
    if( present( deleter) ) then 
      call this%shared_ptr%reset_c_ptr( cptr, deleter )
    else
      call this%shared_ptr%reset_c_ptr( cptr )
    endif
  else
    call this%shared_ptr%reset_c_ptr()
  endif
  allocate(this%refcount)
  call this%attach()
end subroutine

function fckit_shared_object_c_ptr(this) result(cptr)
  use, intrinsic :: iso_c_binding, only : c_ptr
  type(c_ptr) :: cptr
  class(fckit_shared_object) :: this
  cptr = this%shared_ptr%c_ptr()
end function

end module
