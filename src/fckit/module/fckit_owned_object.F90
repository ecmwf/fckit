! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/defines.h"

module fckit_owned_object_module
use fckit_refcount_module, only : fckit_refcount_interface, fckit_owned
use fckit_shared_object_module, only : fckit_shared_object, fckit_c_deleter
implicit none
private

!========================================================================
! Public interface

public :: fckit_owned_object

!========================================================================

type, extends(fckit_shared_object) :: fckit_owned_object
contains
  procedure, public :: reset_c_ptr
! WARNING: Not strictly necessary, as base class (fckit_shared_ptr) has the
!          destructor defined.
!     - PGI-17.7 needs this, as it does not call the base class destructor (COMPILER BUG!)
!                from derived types
!     - Cray-8.5.6 needs this as well as it otherwise does not call constructor from
!                  from function returns
#if FCKIT_FINAL_NOT_INHERITING
  final :: fckit_owned__final_auto
#endif

end type

!========================================================================

private :: fckit_shared_object
private :: fckit_owned
private :: fckit_c_deleter

interface

  subroutine fckit__delete_Owned(this) bind(c,name="fckit__delete_Owned")
    use, intrinsic :: iso_c_binding, only: c_ptr
    type(c_ptr), value :: this
  end subroutine

end interface

!========================================================================
CONTAINS
!========================================================================

subroutine fckit_owned__final_auto(this)
  type(fckit_owned_object), intent(inout) :: this
#if FCKIT_FINAL_DEBUGGING
  write(0,*) "fckit_owned__final_auto"
#endif
#if FCKIT_FINAL_NOT_PROPAGATING
  call this%final()
#endif
  FCKIT_SUPPRESS_UNUSED( this )
end subroutine

subroutine reset_c_ptr(this, cptr, deleter, refcount )
  use, intrinsic :: iso_c_binding, only : c_ptr, c_funptr
  implicit none
  class(fckit_owned_object) :: this
  type(c_ptr), optional :: cptr
  type(c_funptr), optional :: deleter
  procedure(fckit_refcount_interface), optional :: refcount ! dummy

  if( present( cptr ) ) then
    if( present( deleter) ) then
      call this%fckit_shared_object__reset_c_ptr(cptr, deleter, fckit_owned() )
    else
      call this%fckit_shared_object__reset_c_ptr(cptr, fckit_c_deleter(fckit__delete_Owned), fckit_owned() )
    endif
  else
    call this%fckit_shared_object__reset_c_ptr()
  endif
end subroutine


end module
