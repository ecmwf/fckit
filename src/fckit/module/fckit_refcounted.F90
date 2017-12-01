! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fckit_defines.h"

module fckit_refcounted_module
  !! Provides [[fckit_refcounted_module:fckit_refcounted(type)]],
  !! a reference counted implementation of [[fckit_object_module:fckit_object(type)]]

use fckit_object_module, only: fckit_object
implicit none
private

private :: fckit_object

!========================================================================
! Public interface

public fckit_refcounted
public fckit__new_Owned
public fckit__delete_Owned

!========================================================================

type, extends(fckit_object) :: fckit_refcounted
  !! Implements a reference counted [[fckit_object_module:fckit_object(type)]]
  !!
  !! Assigning one such object to another copies the internal C pointer
  !! and increases the reference count.
  !! Finalising one such object decreases the reference count. When the
  !! last object is destroyed, the reference count becomes zero, and the
  !! internal C pointer is deleted.

contains
  procedure, public :: final => fckit_refcounted__final
  procedure, private :: reset
  generic, public :: assignment(=) => reset
  procedure, public :: owners
  procedure, public :: attach
  procedure, public :: detach
  procedure, public :: return
  procedure, public :: copy
  procedure, public :: delete

#ifdef EC_HAVE_Fortran_FINALIZATION
 final :: fckit_refcounted__final_auto
#endif

endtype

!========================================================================

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

!========================================================================
contains

subroutine delete(this)
  use fckit_c_interop_module
  class(fckit_refcounted), intent(inout) :: this
  write(0,*) "fckit_refcounted::delete"
  call fckit__delete_Owned(this%c_ptr())
  call this%reset_c_ptr()
end subroutine

subroutine copy(this,obj_in)
  class(fckit_refcounted), intent(inout) :: this
  class(fckit_refcounted), target, intent(in) :: obj_in
  write(0,*) "fckit_refcounted::copy"
end subroutine

subroutine fckit_refcounted__final(this)
  class(fckit_refcounted), intent(inout) :: this
  if( .not. this%is_null() ) then
    write(0,*) "fckit_refcounted__final owners ",this%owners()
    if( this%owners() >  0 ) then
      write(0,*) __LINE__
      call this%detach()
      if( this%owners() == 0 ) then
        write(0,*) __LINE__

        call this%delete()
      endif
    endif
  else
    write(0,*) "fckit_refcounted__final (uninitialized)"
  endif


end subroutine

subroutine reset(obj_out,obj_in)
  use fckit_c_interop_module
  class(fckit_refcounted), intent(inout) :: obj_out
  class(fckit_refcounted), intent(in) :: obj_in
  if( obj_out /= obj_in ) then
    if( .not. obj_out%is_null() ) then
      write(0,*) "final LHS"
      call obj_out%final()
    endif
    call obj_out%reset_c_ptr( obj_in%c_ptr() )
    write(0,*) ">>> copy"
    call obj_out%copy(obj_in)
    write(0,*) "<<< copy"
    call obj_out%attach()
  endif
  if( .not. obj_out%is_null() ) then
    write(0,*) "reset --> owners = ",obj_out%owners()
  endif
end subroutine

subroutine attach(this)
  class(fckit_refcounted), intent(in) :: this
  call fckit__Owned__attach(this%c_ptr())
end subroutine

subroutine detach(this)
  class(fckit_refcounted), intent(in) :: this
  call fckit__Owned__detach(this%c_ptr())
end subroutine

function owners(this)
  integer :: owners
  class(fckit_refcounted) :: this
  owners = fckit__Owned__owners(this%c_ptr())
end function

subroutine return(this)
  !! Transfer ownership to left hand side of "assignment(=)"
  class(fckit_refcounted), intent(in) :: this
#ifdef Fortran_FINAL_FUNCTION_RESULT
  ! final will be called, which will detach, so attach. 
  call this%attach()
#else
  ! final will not be called, so detach manually
  if( this%owners() > 0 ) call this%detach()
#endif
end subroutine

subroutine fckit_refcounted__final_auto(this)
  type(fckit_refcounted), intent(inout) :: this
  write(0,*) "fckit_refcounted__final_auto"
  call this%final()
end subroutine

!========================================================================

end module
