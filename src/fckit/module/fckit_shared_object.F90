! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fckit_defines.h"

module fckit_shared_object_module
use fckit_object_module, only : fckit_object
use fckit_c_interop_module, only : fckit_c_deleter, fckit_c_nodeleter
use fckit_shared_ptr_module, only : fckit_shared_ptr
implicit none
private

!========================================================================
! Public interface

public :: fckit_shared_object
public :: fckit_c_deleter
public :: fckit_c_nodeleter

!========================================================================

type, extends(fckit_shared_ptr) :: fckit_shared_object
  class(fckit_object), pointer, private  :: shared_object_ => null()
contains

  procedure, public :: shared_ptr_cast

  procedure, public :: share_c_ptr

  procedure, public  :: c_ptr => fckit_shared_object_c_ptr
  procedure, private :: fckit_shared_object_c_ptr

! WARNING: Not strictly necessary, as base class (fckit_shared_ptr) has the
!          destructor defined.
!     - PGI-17.7 needs this, as it does not call the base class destructor (COMPILER BUG!)
!                from derived types
!     - Cray-8.5.6 needs this as well as it otherwise does not call constructor from
!                  from function returns
#ifdef Fortran_FINAL_NOT_INHERITING
  final :: fckit_shared_object__final_auto
#endif

end type

!========================================================================

private :: fckit_object
private :: fckit_shared_ptr

!========================================================================
CONTAINS
!========================================================================

subroutine fckit_shared_object__final_auto(this)
  type(fckit_shared_object), intent(inout) :: this
#ifdef Fortran_FINAL_DEBUGGING
  write(0,*) "fckit_shared_object__final_auto"
#endif
#ifdef Fortran_FINAL_NOT_PROPAGATING
  call this%final()
#endif
  FCKIT_SUPPRESS_UNUSED( this )
end subroutine

function shared_ptr_cast(this) result(success)
  class(fckit_shared_object) :: this
  logical :: success
  success = .false.
  nullify( this%shared_object_ )
  associate( shared_object => this%shared_ptr() )
  select type( shared_object )
    class is( fckit_object )
      this%shared_object_ => shared_object
      success = .true.
      return
  end select
  end associate
end function

subroutine share_c_ptr(this, cptr, deleter)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_funptr
  class(fckit_shared_object) :: this
  type(c_ptr), optional :: cptr
  type(c_funptr), optional :: deleter
  allocate( fckit_object::this%shared_object_ )
  if( present( cptr ) ) then
    if( present( deleter) ) then 
      call this%shared_object_%reset_c_ptr( cptr, deleter )
    else
      call this%shared_object_%reset_c_ptr( cptr )
    endif
  else
    call this%shared_object_%reset_c_ptr()
  endif
  call this%share( this%shared_object_ )
end subroutine

function fckit_shared_object_c_ptr(this) result(cptr)
  use, intrinsic :: iso_c_binding, only : c_ptr
  type(c_ptr) :: cptr
  class(fckit_shared_object) :: this
  cptr = this%shared_object_%c_ptr()
end function

end module
