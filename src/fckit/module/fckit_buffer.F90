! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fckit.h"

module fckit_buffer_module
  !! Wrap eckit Buffer capabilities.

use fckit_shared_object_module, only: fckit_shared_object, fckit_c_deleter, fckit_c_nodeleter
implicit none

private :: fckit_shared_object
private :: fckit_c_deleter
private :: fckit_c_nodeleter

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

subroutine c_fckit_buffer_delete( this ) bind(C, name="c_fckit_buffer_delete")
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: this
end subroutine

end interface


!========================================================================

type, extends(fckit_shared_object) :: fckit_buffer
  !! Buffer
  !!
  !! Can contain any data
  !! A typical use case is with the function [[fckit_mpi_module:fckit_mpi_comm(type):broadcast_file(function)]]
  !! where a file is read on one MPI task, and broadcast to all MPI tasks, storing it in a buffer.
  !! This buffer can then be used to construct a configuration
  !! (see e.g. [[fckit_configuration_module:fckit_YAMLConfiguration(type)]] )
contains
  procedure, public :: str

#if FCKIT_FINAL_NOT_INHERITING
  final :: fckit_buffer__final_auto
#endif

endtype

interface fckit_buffer
  module procedure ctor_from_cptr
end interface

!========================================================================
contains
!---------------------------------------------------------------------------------------

function ctor_from_cptr(cptr, share) result(this)
  use, intrinsic :: iso_c_binding, only : c_ptr
  type(c_ptr), value :: cptr
  type(fckit_buffer) :: this
  logical, optional  :: share
  logical :: opt_share
  opt_share = .false.
  if( present(share) ) opt_share = share
  if( opt_share ) then
    call this%reset_c_ptr( cptr , fckit_c_deleter(c_fckit_buffer_delete) )
  else
    call this%reset_c_ptr( cptr , fckit_c_nodeleter() )
  endif
  call this%return()
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

subroutine fckit_buffer__final_auto(this)
  type(fckit_buffer), intent(inout) :: this
#if FCKIT_FINAL_DEBUGGING
  write(0,*) "fckit_buffer__final_auto"
#endif
#if FCKIT_FINAL_NOT_PROPAGATING
  call this%final()
#endif
  FCKIT_SUPPRESS_UNUSED( this )
end subroutine

end module
