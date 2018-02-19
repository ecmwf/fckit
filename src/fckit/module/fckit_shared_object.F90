! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fckit.h"

module fckit_shared_object_module
use fckit_object_module, only : fckit_object
use fckit_c_interop_module, only : fckit_c_deleter, fckit_c_nodeleter
use fckit_shared_ptr_module, only : fckit_shared_ptr, fckit_refcount_interface, &
 & fckit_owned
implicit none
private

!========================================================================
! Public interface

public :: fckit_shared_object
public :: fckit_c_deleter
public :: fckit_c_nodeleter
public :: fckit_owned

!========================================================================

type, extends(fckit_shared_ptr) :: fckit_shared_object
  class(fckit_object), pointer, private  :: shared_object_ => null()
contains

  procedure, public :: shared_ptr_cast

  procedure, public :: reset_c_ptr

  procedure, public  :: c_ptr => fckit_shared_object_c_ptr
  procedure, private :: fckit_shared_object_c_ptr
  
  procedure, public :: is_null

! WARNING: Not strictly necessary, as base class (fckit_shared_ptr) has the
!          destructor defined.
!     - PGI-17.7 needs this, as it does not call the base class destructor (COMPILER BUG!)
!                from derived types
!     - Cray-8.5.6 needs this as well as it otherwise does not call constructor from
!                  from function returns
#if FCKIT_FINAL_NOT_INHERITING
  final :: fckit_shared_object__final_auto
#endif

  procedure, public :: fckit_shared_object__reset_c_ptr => reset_c_ptr
  procedure, public :: fckit_shared_object__shared_ptr_cast => shared_ptr_cast

end type

!========================================================================

private :: fckit_object
private :: fckit_shared_ptr
private :: fckit_refcount_interface

!========================================================================
CONTAINS
!========================================================================

subroutine fckit_shared_object__final_auto(this)
  type(fckit_shared_object), intent(inout) :: this
#if FCKIT_FINAL_DEBUGGING
  write(0,*) "fckit_shared_object__final_auto"
#endif
#if FCKIT_FINAL_NOT_PROPAGATING
  call this%final()
#endif
  FCKIT_SUPPRESS_UNUSED( this )
end subroutine

function shared_ptr_cast(this) result(success)
  class(fckit_shared_object) :: this
  logical :: success
  class(*), pointer :: shared_object
  success = .false.
  nullify( this%shared_object_ )
  shared_object => this%shared_ptr()
  select type( shared_object )
    class is( fckit_object )
      this%shared_object_ => shared_object
      success = .true.
      return
  end select
end function

subroutine reset_c_ptr(this, cptr, deleter, refcount )
  use, intrinsic :: iso_c_binding, only : c_ptr, c_funptr
  implicit none
  class(fckit_shared_object) :: this
  type(c_ptr), optional :: cptr
  type(c_funptr), optional :: deleter
  type(c_funptr), optional :: refcount
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
  if( present(refcount) ) then
    call this%share( this%shared_object_, refcount )
  else
    call this%share( this%shared_object_ )
  endif
end subroutine

function is_null(this)
  logical :: is_null
  class(fckit_shared_object) :: this
  if( .not. associated(this%shared_object_) ) then
    is_null = .true.
  else
    is_null = this%shared_object_%is_null()
  endif
end function

function fckit_shared_object_c_ptr(this) result(cptr)
  use, intrinsic :: iso_c_binding, only : c_ptr
  type(c_ptr) :: cptr
  class(fckit_shared_object) :: this
  cptr = this%shared_object_%c_ptr()
end function

end module
