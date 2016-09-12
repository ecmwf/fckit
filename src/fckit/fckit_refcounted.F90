module fckit_refcounted_module
use fckit_object_module, only: fckit_object
implicit none
private

private :: fckit_object
!#define FORTRAN_SUPPORTS_FINAL
!========================================================================
! Public interface

public fckit_refcounted
public fckit__new_Owned
public fckit__delete_Owned

!========================================================================

type, extends(fckit_object) :: fckit_refcounted
contains
  procedure, public :: final => final_c
  procedure, private :: reset => reset_c
  generic, public :: assignment(=) => reset
  procedure, public :: owners => owners_c
  procedure, public :: attach => attach_c
  procedure, public :: detach => detach_c
  procedure, public :: return => return_c
  procedure, public :: copy
  procedure, public :: delete

#ifdef FORTRAN_SUPPORTS_FINAL
 final :: final_auto
#endif

  !procedure(intf_copy), deferred, public :: copy
  !procedure(intf_delete), deferred, public :: delete

endtype

!interface
!  subroutine intf_delete(this)
!     import fckit_refcounted
!     class(fckit_refcounted), intent(inout):: this
!  end subroutine
!  subroutine intf_copy(this,obj_in)
!    import fckit_refcounted
!    class(fckit_refcounted), intent(inout) :: this
!    class(fckit_refcounted), target, intent(in) :: obj_in
!  end subroutine
!end interface


!========================================================================

#if 0
type, abstract, extends(fckit_object) :: fckit_refcounted_fortran
contains
  procedure, public :: final => final_f
  procedure, private :: reset => reset_f
  generic, public :: assignment(=) => reset
  procedure(intf_copy_f), deferred, public :: copy
  procedure(intf_delete_f), deferred, public :: delete
  procedure, public :: owners => owners_f
  procedure, public :: attach => attach_f
  procedure, public :: detach => detach_f
  procedure, public :: return => return_f
endtype

interface
  subroutine intf_delete_f(this)
     import fckit_refcounted_fortran
     class(fckit_refcounted_fortran), intent(inout):: this
  end subroutine
  subroutine intf_copy_f(this,obj_in)
    import fckit_refcounted_fortran
    class(fckit_refcounted_fortran), intent(inout) :: this
    class(fckit_refcounted_fortran), target, intent(in) :: obj_in
  end subroutine
end interface
#endif

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
  use fckit_c_interop
  class(fckit_refcounted), intent(inout) :: this
  call c_ptr_free(this%ptr())
end subroutine


subroutine copy(this,obj_in)
  class(fckit_refcounted), intent(inout) :: this
  class(fckit_refcounted), target, intent(in) :: obj_in
end subroutine

subroutine final_auto(this)
  type(fckit_refcounted), intent(inout) :: this
  call this%final()
end subroutine

#if 0
subroutine fckit_refcounted_nocopy(this,obj_in)
  class(fckit_refcounted), intent(inout) :: this
  class(fckit_refcounted), intent(in) :: obj_in
end subroutine
#endif

subroutine final_c(this)
  use fckit_c_interop
  use iso_c_binding
  class(fckit_refcounted), intent(inout) :: this
  if( .not. this%is_null() ) then
    if( this%owners() >  0 ) then
      call this%detach()
    endif
    if( this%owners() == 0 ) then
      call this%delete()
    endif
    call this%reset_ptr()
  endif
end subroutine



subroutine reset_c(obj_out,obj_in)
  use fckit_c_interop
  class(fckit_refcounted), intent(inout) :: obj_out
  class(fckit_refcounted), intent(in) :: obj_in
  if( obj_out /= obj_in ) then
    if( .not. obj_out%is_null() ) call obj_out%final()
    call obj_out%final()
    call obj_out%reset_ptr( obj_in%ptr() )
    call obj_out%copy(obj_in)
    call obj_out%attach()
  endif
end subroutine

subroutine attach_c(this)
  class(fckit_refcounted), intent(inout) :: this
  call fckit__Owned__attach(this%ptr())
end subroutine

subroutine detach_c(this)
  class(fckit_refcounted), intent(inout) :: this
  call fckit__Owned__detach(this%ptr())
end subroutine

function owners_c(this)
  integer :: owners_c
  class(fckit_refcounted) :: this
  owners_c = fckit__Owned__owners(this%ptr())
end function

subroutine return_c(this)
  class(fckit_refcounted), intent(inout) :: this
  if( this%owners() > 0 ) call this%detach()
end subroutine

!========================================================================

#if 0
subroutine final_f(this)
  class(fckit_refcounted_Fortran), intent(inout) :: this
  write(0,*) 'final , owners = ',this%owners()
  if( .not. this%is_null() ) then
    if( this%owners() >  0 ) then
      write(0,*) '---detach'
      call this%detach()
    elseif( this%owners() == 0 ) then
      write(0,*) '---delete'
      call this%delete()
      call fckit__delete_Owned(this%ptr())
    else
      write(0,*)'error'
    endif
    call this%reset_ptr()
  endif
end subroutine

subroutine reset_f(obj_out,obj_in)
  use fckit_c_interop
  class(fckit_refcounted_fortran), intent(inout) :: obj_out
  class(fckit_refcounted_fortran), intent(in) :: obj_in
  if( obj_out /= obj_in ) then
    if( .not. obj_out%is_null() ) call obj_out%final()
    call obj_out%reset_ptr( obj_in%ptr() )
    call obj_out%copy(obj_in)
    if( .not. obj_out%is_null() ) call obj_out%attach()
  endif
end subroutine

subroutine attach_f(this)
  class(fckit_refcounted_fortran), intent(inout) :: this
  call fckit__Owned__attach(this%ptr())
end subroutine

subroutine detach_f(this)
  class(fckit_refcounted_fortran), intent(inout) :: this
  call fckit__Owned__detach(this%ptr())
end subroutine

function owners_f(this)
  integer :: owners_f
  class(fckit_refcounted_fortran) :: this
  owners_f = fckit__Owned__owners(this%ptr())
end function

subroutine return_f(this)
  class(fckit_refcounted_fortran), intent(inout) :: this
  if( this%is_null() ) then
    call this%reset_ptr( fckit__new_Owned() )
    call this%attach()
  endif
#ifndef FORTRAN_SUPPORTS_FINAL
  call this%detach()
#endif
end subroutine
#endif

end module
