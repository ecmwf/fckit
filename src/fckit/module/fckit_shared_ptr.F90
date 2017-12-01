! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fckit_defines.h"

module fckit_shared_ptr_module
use fckit_object_module, only: fckit_object
implicit none
private

!========================================================================
! Public interface

public fckit_shared_ptr

!========================================================================


type :: fckit_shared_ptr
  class(fckit_object), pointer :: shared_ptr => null()
contains
  procedure, public :: final => fckit_shared_ptr__final
  final :: fckit_shared_ptr__final_auto

  procedure, private :: reset
  generic, public :: assignment(=) => reset
  procedure, public :: owners
  procedure, public :: attach
  procedure, public :: detach
  procedure, public :: return
  procedure, public :: cast
  procedure, public :: share

end type

interface fckit_shared_ptr
  module procedure fckit_shared_ptr
end interface

!========================================================================

private :: fckit_object

! =============================================================================
CONTAINS
! =============================================================================

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
        call this%shared_ptr%final()
        deallocate(this%shared_ptr)
      endif
    endif
  endif
  nullify(this%shared_ptr)
end subroutine

subroutine reset(obj_out,obj_in)
  use, intrinsic :: iso_c_binding, only : c_loc, c_associated
  class(fckit_shared_ptr), intent(inout) :: obj_out
  class(fckit_shared_ptr), intent(in)    :: obj_in
  if( .not. associated( obj_out%shared_ptr, obj_in%shared_ptr ) ) then
    call obj_out%final()
    obj_out%shared_ptr => obj_in%shared_ptr
    if( obj_out%cast() ) then
      call obj_out%attach()
    else
      nullify(obj_out%shared_ptr)
      call bad_cast
    endif
  else
    if( obj_out%cast() ) then ; endif
  endif
end subroutine

subroutine attach(this)
  class(fckit_shared_ptr), intent(inout) :: this
  this%shared_ptr%refcount = this%shared_ptr%refcount + 1
end subroutine

subroutine detach(this)
  class(fckit_shared_ptr), intent(inout) :: this
  this%shared_ptr%refcount = max(0, this%shared_ptr%refcount - 1)
end subroutine

function owners(this)
  integer :: owners
  class(fckit_shared_ptr), intent(in) :: this
  if( associated( this%shared_ptr) ) then
    owners = this%shared_ptr%refcount
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

function cast(this) result(success)
  class(fckit_shared_ptr) :: this
  logical :: success
  success = .true.
end function

subroutine share(this)
  class(fckit_shared_ptr) :: this
  logical :: success
  success = this%cast()
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
