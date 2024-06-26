! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fckit.h"

module fckit_refcount_module
use, intrinsic :: iso_c_binding, only : c_ptr, c_null_ptr
implicit none
private

!========================================================================
! Public interface

public :: fckit_refcount
public :: fckit_refcount_interface
public :: fckit_external

#if FCKIT_HAVE_ECKIT
public :: fckit_owned
#endif

!========================================================================

type, abstract :: fckit_refcount
contains
procedure(fckit_refcount_owners), public, deferred :: owners
procedure(fckit_refcount_attach), public, deferred :: attach
procedure(fckit_refcount_detach), public, deferred :: detach
end type

interface
  function fckit_refcount_owners(this)
    use, intrinsic :: iso_c_binding, only: c_int32_t
    import fckit_refcount
    integer(c_int32_t) :: fckit_refcount_owners
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

!========================================================================

type, extends(fckit_refcount) :: fckit_refcount_external
  integer, private :: refcount_ = 0
  contains
  procedure, public :: owners => fckit_refcount_external_owners
  procedure, public :: attach => fckit_refcount_external_attach
  procedure, public :: detach => fckit_refcount_external_detach
end type

!========================================================================

interface

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

#if FCKIT_HAVE_ECKIT
type, extends(fckit_refcount) :: fckit_refcount_owned
  type(c_ptr), private :: cptr_ = c_null_ptr
  contains
  procedure, public :: owners => fckit_refcount_owned_owners
  procedure, public :: attach => fckit_refcount_owned_attach
  procedure, public :: detach => fckit_refcount_owned_detach
end type
#endif

private :: c_ptr, c_null_ptr

!========================================================================
CONTAINS
!========================================================================

subroutine allocate_fckit_external(refcount,shared_ptr)
  class(fckit_refcount), pointer, intent(inout) :: refcount
  class(*), target, intent(in) :: shared_ptr
  allocate( fckit_refcount_external::refcount )
  FCKIT_SUPPRESS_UNUSED( shared_ptr )
end subroutine

subroutine fckit_external (funptr)
  procedure(fckit_refcount_interface), pointer :: funptr
  funptr => allocate_fckit_external
end subroutine fckit_external

#if FCKIT_HAVE_ECKIT
subroutine allocate_fckit_refcount_owned(refcount,shared_ptr)
  use fckit_object_module, only : fckit_object
  use, intrinsic :: iso_c_binding, only : c_ptr
  class(fckit_refcount), pointer, intent(inout) :: refcount
  class(*), target, intent(in) :: shared_ptr
  type(c_ptr) :: cptr
  allocate( fckit_refcount_owned::refcount )
  select type( shared_ptr )
    class is( fckit_object )
      cptr = shared_ptr%CPTR_PGIBUG_A
  end select
  select type( refcount )
    class is( fckit_refcount_owned )
      refcount%cptr_ = cptr
  end select
end subroutine

subroutine fckit_owned(funptr)
  procedure(fckit_refcount_interface), pointer :: funptr
  funptr => allocate_fckit_refcount_owned
end subroutine fckit_owned

#endif


subroutine fckit_refcount_external_attach(this)
  class(fckit_refcount_external), intent(inout) :: this
  this%refcount_ = this%refcount_ + 1
end subroutine

subroutine fckit_refcount_external_detach(this)
  class(fckit_refcount_external), intent(inout) :: this
  this%refcount_ = max(0, this%refcount_ - 1)
end subroutine

function fckit_refcount_external_owners(this) result(owners)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: owners
  class(fckit_refcount_external), intent(in) :: this
  owners = this%refcount_
end function

#if FCKIT_HAVE_ECKIT
subroutine fckit_refcount_owned_attach(this)
  class(fckit_refcount_owned), intent(inout) :: this
  call fckit__Owned__attach(this%cptr_)
end subroutine

subroutine fckit_refcount_owned_detach(this)
  class(fckit_refcount_owned), intent(inout) :: this
  call fckit__Owned__detach(this%cptr_)
end subroutine

function fckit_refcount_owned_owners(this) result(owners)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: owners
  class(fckit_refcount_owned), intent(in) :: this
  owners = fckit__Owned__owners(this%cptr_)
end function
#endif

end module
