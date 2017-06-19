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
  !! distinction that the internal C pointer is only used to track reference counts.
  !!
  !! Assigning one such object to another copies the data
  !! and increases the reference count.
  !! Finalising one such object decreases the reference count. When the
  !! last object is destroyed, the reference count becomes zero, and the
  !! object should be deallocated
  !! 
  !! note: The internal data besides the C pointer should also be a pointer so that
  !! a copy only copies pointers!

  integer, pointer, private :: refcount => null()
contains
  procedure, public :: final => final_f
  procedure, private :: reset => reset_f
  generic, public :: assignment(=) => reset
  procedure, public :: owners => owners_f
  procedure, public :: attach => attach_f
  procedure, public :: detach => detach_f
  procedure, public :: return => return_f
  procedure, public :: copy => copy_f
  procedure, public :: delete => delete_f

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

subroutine delete_f(this)
  use fckit_c_interop_module
  class(fckit_refcounted_fortran), intent(inout) :: this
  call c_ptr_free(this%c_ptr())
end subroutine


subroutine copy_f(this,obj_in)
  class(fckit_refcounted_fortran), intent(inout) :: this
  class(fckit_refcounted_fortran), target, intent(in) :: obj_in
end subroutine

subroutine final_auto_f(this)
  type(fckit_refcounted_fortran), intent(inout) :: this
  call this%final()
end subroutine

subroutine final_f(this)
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

subroutine reset_f(obj_out,obj_in)
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

subroutine attach_f(this)
  class(fckit_refcounted_fortran), intent(inout) :: this
  call assert_refcount(this)
  this%refcount = this%refcount + 1
end subroutine

subroutine detach_f(this)
  class(fckit_refcounted_fortran), intent(inout) :: this
  call assert_refcount(this)
  this%refcount = this%refcount - 1
end subroutine

function owners_f(this)
  integer :: owners_f
  class(fckit_refcounted_fortran) :: this
  call assert_refcount(this)
  owners_f = this%refcount
end function

subroutine return_f(this)
  class(fckit_refcounted_fortran), intent(inout) :: this
#ifdef Fortran_FINAL_FUNCTION_RESULT
  if( this%owners() == 0 ) call this%attach()
#else
  if( this%owners() > 0 ) call this%detach()
#endif
end subroutine

end module
