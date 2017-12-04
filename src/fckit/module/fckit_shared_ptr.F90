! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fckit_defines.h"

module fckit_shared_ptr_module
implicit none
private

!========================================================================
! Public interface

public fckit_shared_ptr
public fckit_make_shared

!========================================================================

type :: fckit_shared_ptr
  class(*), pointer :: shared_ptr => null()
  integer,  pointer :: refcount   => null()
contains
  procedure, public :: final => fckit_shared_ptr__final

#ifdef EC_HAVE_Fortran_FINALIZATION
  final :: fckit_shared_ptr__final_auto
#endif

  procedure, private :: reset
  generic, public :: assignment(=) => reset
  procedure, public :: owners
  procedure, public :: attach
  procedure, public :: detach
  procedure, public :: return
  procedure, public :: shared_ptr_cast
  procedure, public :: make_shared

end type

!========================================================================
CONTAINS
! =============================================================================

subroutine fckit_finalise( shared_ptr )
  use fckit_final_module, only: fckit_final
  class(*), pointer :: shared_ptr
  select type(shared_ptr)
    class is(fckit_final)
      write(0,*) "fckit_final%final()"
      call shared_ptr%final()
  end select
end subroutine

subroutine fckit_shared_ptr__final_auto(this)
  type(fckit_shared_ptr), intent(inout) :: this
  write(0,*) "fckit_shared_ptr__final_auto"
  call this%final()
end subroutine

subroutine fckit_shared_ptr__final(this)
  class(fckit_shared_ptr), intent(inout) :: this
  if( associated(this%shared_ptr) ) then
    if( this%owners() > 0 ) then
      write(0,*) "fckit_shared_ptr__final  , owners = ", this%owners()
      call this%detach()
      if( this%owners() == 0 ) then
        call fckit_finalise(this%shared_ptr)
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
  class(fckit_shared_ptr), intent(inout) :: obj_out
  class(fckit_shared_ptr), intent(in)    :: obj_in
  if( .not. associated( obj_out%shared_ptr, obj_in%shared_ptr ) ) then
    write(0,*) "fckit_shared_ptr::reset"
    call obj_out%final()
    obj_out%shared_ptr => obj_in%shared_ptr
    obj_out%refcount   => obj_in%refcount
    if( obj_out%shared_ptr_cast() ) then
      call obj_out%attach()
    else
      nullify(obj_out%shared_ptr)
      nullify(obj_out%refcount)
      call bad_cast
    endif
    write(0,*) "fckit_shared_ptr::reset... done"
  else
    if( obj_out%shared_ptr_cast() ) then ; endif
  endif
end subroutine

subroutine attach(this)
  class(fckit_shared_ptr), intent(inout) :: this
  if( associated(this%shared_ptr) ) then
    this%refcount = this%refcount + 1
  endif
end subroutine

subroutine detach(this)
  class(fckit_shared_ptr), intent(inout) :: this
  if( associated(this%shared_ptr) ) then
    this%refcount = max(0, this%refcount - 1)
  endif
end subroutine

function owners(this)
  integer :: owners
  class(fckit_shared_ptr), intent(in) :: this
  if( associated( this%shared_ptr) ) then
    owners = this%refcount
  else
    owners = 0
  endif
end function

subroutine return(this)
  !! Transfer ownership to left hand side of "assignment(=)"
  class(fckit_shared_ptr), intent(inout) :: this
#ifdef Fortran_FINAL_FUNCTION_RESULT
  ! final will be called, which will detach, so attach first
  call this%attach()
#else
  ! final will not be called, so detach manually
  if( this%owners() > 0 ) call this%detach()
#endif
end subroutine

function shared_ptr_cast(this) result(success)
  class(fckit_shared_ptr) :: this
  logical :: success
  success = .true.
  write(0,*) "wrong cast"
end function

function fckit_make_shared( ptr ) result(this)
  type(fckit_shared_ptr) :: this
  class(*), target :: ptr
  call this%make_shared(ptr)
  call this%return()
end function

subroutine make_shared(this,ptr)
  class(fckit_shared_ptr) :: this
  class(*), target :: ptr
  this%shared_ptr => ptr
  allocate(this%refcount)
  call this%attach()
end subroutine


subroutine bad_cast(message)
  character(len=*), optional :: message
  if( present(message) ) then
    write(0,'("ERROR: bad_cast -- ",A)') message
  else
    write(0,'("ERROR: bad cast")')
  endif
end subroutine

end module
