! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fckit_defines.h"

module fckit_buffer_module
  !! Wrap eckit Buffer capabilities.

use fckit_object_module, only: fckit_object
implicit none

private :: fckit_object

!========================================================================
! Public interface

public :: fckit_buffer

private

interface
!-------------------------------------------------------------------------------
! int c_fckit_buffer_str( const eckit::Buffer* This, const
!   char* &str, size_t &size)
!-------------------------------------------------------------------------------
function c_fckit_buffer_str( This, str, size ) bind(C,name="c_fckit_buffer_str")
    use iso_c_binding, only: c_char, c_ptr, c_int, c_size_t
    integer(c_int) :: c_fckit_buffer_str
    type(c_ptr), value :: This
    type(c_ptr) :: str
    integer(c_size_t) :: size
end function
!-------------------------------------------------------------------------------

subroutine c_fckit_buffer_delete( This ) bind(C, name="c_fckit_buffer_delete")
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: This
end subroutine

end interface


!========================================================================

type, extends(fckit_object) :: fckit_buffer
  !! Buffer
  !!
  !! Can contain any data
  !! A typical use case is with the function [[fckit_mpi_module:fckit_mpi_comm(type):broadcast_file(function)]]
  !! where a file is read on one MPI task, and broadcast to all MPI tasks, storing it in a buffer.
  !! This buffer can then be used to construct a configuration
  !! (see e.g. [[fckit_configuration_module:fckit_YAMLConfiguration(type)]] )
  integer, pointer, private :: refcount => null()

contains
  procedure, public :: str

! Following functions are copies of what's in fckit_refcounted_fortran
! Ideally we inherit from fckit_refcounted_fortran, but a gfortran bug
! prevents it. The only difference is that the reset_f function uses
! "type" instead of "class" for the second "obj_in" argument.

  procedure, public :: final => fckit_buffer__final
  procedure, private :: reset
  generic, public :: assignment(=) => reset
  procedure, public :: owners
  procedure, public :: attach
  procedure, public :: detach
  procedure, public :: return
  procedure, public :: copy
  procedure, public :: delete
  procedure, public :: consumed

#ifdef EC_HAVE_Fortran_FINALIZATION
  final :: fckit_buffer__final_auto
#endif

endtype

interface fckit_buffer
  module procedure ctor_from_cptr
end interface

!========================================================================
contains
!---------------------------------------------------------------------------------------

subroutine assert_refcount(this)
  type(fckit_buffer) :: this
  if( .not. associated(this%refcount) ) then
    allocate( this%refcount )
    this%refcount = 0
  endif
end subroutine


function ctor_from_cptr(cptr) result(buffer)
  use, intrinsic :: iso_c_binding, only : c_ptr
  type(c_ptr), value :: cptr
  type(fckit_buffer) :: buffer
  call buffer%reset_c_ptr( cptr )
  call assert_refcount(buffer)
end function


!---------------------------------------------------------------------------------------

function str(this)
  use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_size_t
  use fckit_c_interop_module
  character(len=:), allocatable :: str
  class(fckit_buffer), intent(in) :: this
  integer(c_int) :: errcode
  integer(c_size_t) :: str_size
  type(c_ptr) :: str_cptr
  errcode = c_fckit_buffer_str(this%c_ptr(),str_cptr,str_size)
  allocate(character(len=str_size) :: str )
  str = c_ptr_to_string(str_cptr)
  call c_ptr_free(str_cptr)
end function

!========================================================================

subroutine delete(this)
  use fckit_c_interop_module
  class(fckit_buffer), intent(inout) :: this
  call c_fckit_buffer_delete(this%c_ptr())
end subroutine


subroutine copy(this,obj_in)
  class(fckit_buffer), intent(inout) :: this
  class(fckit_buffer), target, intent(in) :: obj_in
end subroutine


subroutine fckit_buffer__final_auto(this)
  type(fckit_buffer), intent(inout) :: this
  call this%final()
end subroutine

subroutine fckit_buffer__final(this)
  class(fckit_buffer), intent(inout) :: this
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
  class(fckit_buffer), intent(inout) :: obj_out
  type(fckit_buffer), intent(in) :: obj_in
  if( obj_out /= obj_in ) then
    if( .not. obj_out%is_null() ) call obj_out%final()
    call obj_out%reset_c_ptr( obj_in%c_ptr() )
    obj_out%refcount => obj_in%refcount
    call obj_out%copy(obj_in)
    call obj_out%attach()
  endif
end subroutine


subroutine attach(this)
  class(fckit_buffer), intent(inout) :: this
  call assert_refcount(this)
  this%refcount = this%refcount + 1
end subroutine

subroutine detach(this)
  class(fckit_buffer), intent(inout) :: this
  call assert_refcount(this)
  this%refcount = max( 0, this%refcount - 1 )
end subroutine

function owners(this)
  integer :: owners
  class(fckit_buffer) :: this
  call assert_refcount(this)
  owners = this%refcount
end function

subroutine return(this)
  class(fckit_buffer), intent(inout) :: this
#ifdef Fortran_FINAL_FUNCTION_RESULT
  if( this%owners() == 0 ) call this%attach()
#else
  call this%detach()
#endif
end subroutine

subroutine consumed(this)
  use fckit_c_interop_module
  class(fckit_buffer), intent(in) :: this
  type(fckit_buffer) :: consumedbuffer
  consumedbuffer = this
  call consumedbuffer%final()
end subroutine


end module
