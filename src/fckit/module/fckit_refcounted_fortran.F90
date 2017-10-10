! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


module fckit_refcounted_fortran_module
  !! Provides [[fckit_refcounted_fortran_module:fckit_refcounted_fortran(type)]],
  !! a reference counted implementation of [[fckit_object_module:fckit_object(type)]]

use fckit_object_module, only: fckit_object
implicit none
private

private :: fckit_object

!========================================================================
! Public interface

public fckit_refcounted_fortran

!========================================================================

type, extends(fckit_object) :: fckit_refcounted_fortran
  !! Implements a reference counted [[fckit_object_module:fckit_object(type)]]
  !!
  !! Very similar to [[fckit_refcounted_module:fckit_refcounted(type)]], with the
  !! distinction that the internal C pointer is not keeping track of refcounting.
  !! Instead, the refcount is stored internally

  integer, pointer, private :: refcount => null()
contains
  procedure, public :: final
  procedure, private :: reset
  generic, public :: assignment(=) => reset
  procedure, public :: owners
  procedure, public :: attach
  procedure, public :: detach
  procedure, public :: return
  procedure, public :: copy
  procedure, public :: delete

#ifdef EC_HAVE_Fortran_FINALIZATION
  final :: final_auto_f
#endif

endtype

!========================================================================

contains

subroutine assert_refcount(this)
  type(fckit_refcounted_fortran) :: this
  if( .not. associated(this%refcount) ) then
    allocate( this%refcount )
    this%refcount = 0
  endif
end subroutine

subroutine delete(this)
  use fckit_c_interop_module
  class(fckit_refcounted_fortran), intent(inout) :: this
  call c_ptr_free(this%c_ptr())
end subroutine


subroutine copy(this,obj_in)
  class(fckit_refcounted_fortran), intent(inout) :: this
  class(fckit_refcounted_fortran), target, intent(in) :: obj_in
end subroutine

subroutine final_auto(this)
  type(fckit_refcounted_fortran), intent(inout) :: this
  call this%final()
end subroutine

subroutine final(this)
  class(fckit_refcounted_Fortran), intent(inout) :: this
  if( .not. this%is_null() ) then
    if( this%owners() >  0 ) then
      call this%detach()
    endif
    if( this%owners() == 0 ) then
      call this%delete()
      if( associated(this%refcount) ) deallocate(this%refcount)
    endif
    call this%reset_c_ptr()
  endif
end subroutine

subroutine reset(obj_out,obj_in)
  use fckit_c_interop_module
  class(fckit_refcounted_fortran), intent(inout) :: obj_out
  class(fckit_refcounted_fortran), intent(in) :: obj_in
  if( obj_out /= obj_in ) then
    if( .not. obj_out%is_null() ) call obj_out%final()
    call obj_out%reset_c_ptr( obj_in%c_ptr() )
    obj_out%refcount => obj_in%refcount
    call obj_out%copy(obj_in)
    call obj_out%attach()
  endif
end subroutine

subroutine attach(this)
  class(fckit_refcounted_fortran), intent(inout) :: this
  call assert_refcount(this)
  this%refcount = this%refcount + 1
end subroutine

subroutine detach(this)
  class(fckit_refcounted_fortran), intent(inout) :: this
  call assert_refcount(this)
  this%refcount = max(0, this%refcount - 1)
end subroutine

function owners(this)
  integer :: owners
  class(fckit_refcounted_fortran) :: this
  call assert_refcount(this)
  owners = this%refcount
end function

subroutine return(this)
  class(fckit_refcounted_fortran), intent(inout) :: this
#ifdef Fortran_FINAL_FUNCTION_RESULT
  if( this%owners() == 0 ) call this%attach()
#else
  call this%detach()
#endif
end subroutine

subroutine consumed(this)
  use fckit_c_interop_module
  class(fckit_refcounted_fortran), intent(in) :: this
  type(fckit_refcounted_fortran) :: consumed_obj
  consumed_obj = this        ! increase refcount
  call consumed_obj%final()  ! decrease refcount, and possibly delete
end subroutine

end module
