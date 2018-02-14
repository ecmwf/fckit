! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module fckit_exception_module
  !! author: Willem Deconinck
  !!
  !! Module providing [[fckit_exception_module:fckit_exception(variable)]] global variable

implicit none
private

#include "fckit_exception.inc"

public :: fckit_exception
public :: fckit_exception_handler

type, FORD_PRIVATE :: fckit_exception_location
  !! Type that gives API to read the location of the thrown exception

contains

  procedure, public, nopass :: is_set    => location_is_set
    !! Function that returns if a location where abort happens is present
    !!
    !! This function can be used in an abort handler

  procedure, public, nopass :: file      => location_file
    !! Function that returns the file where abort is called
    !!
    !! This function can be used in an abort handler

  procedure, public, nopass :: line      => location_line
    !! Function that returns the line where abort is called
    !!
    !! This function can be used in an abort handler

  procedure, public, nopass :: function  => location_function
    !! Function that returns the function where abort is called
    !!
    !! This function can be used in an abort handler

end type fckit_exception_location

type, FORD_PRIVATE :: fckit_exception_type
  !! Type of the global [[fckit_exception_module:fckit_exception(variable)]] variable

  type(fckit_exception_location) :: location
    !! Variable of the type [[fckit_exception_module:fckit_exception_location(type)]]
    !! exposing the location where the exception is thrown

contains

  procedure, public, nopass :: throw
    !! Throw exception
    !!
    !! If the exception is not caught, the program will try to terminate, and
    !! fckit terminate handler will be called, giving nice backtrace etc.
    !!
    !!####Example usage
    !!
    !!```fortran
    !! call fckit_exception%throw("I have my reasons",___FILE___,___LINE___)
    !!```

  procedure, public, nopass :: abort
    !! Throw the ```eckit::Abort``` exception
    !!
    !!####Example usage
    !!
    !!```fortran
    !! call fckit_exception%abort("I have my reasons",___FILE___,___LINE___)
    !!```
    !!
    !! You could create a macro to help:
    !!
    !!```fortran
    !! #define ABORT_HERE( what ) fckit_exception%abort(what,___FILE___,___LINE___)
    !!
    !! call ABORT_HERE("I have my reasons")
    !!```

  procedure, public, nopass :: what
    !! Function that returns the reason for the thrown exception
    !!
    !! This function can be used in an abort handler

  procedure, public, nopass :: callstack
    !! Function that returns the callstack where exception is thrown
    !!
    !! This function can be used in an abort handler

  procedure, public, nopass :: set_handler
    !! Subroutine to set custom abort handler
    !!
    !!####Example usage
    !!
    !!```fortran
    !! subroutine custom_exception_handler()
    !!   ! handle exception
    !! end subroutine
    !!
    !! subroutine set_custom_exception_handler()
    !!   external :: custom_exception_handler
    !!   procedure(fckit_exception_handler), pointer :: exception_handler => custom_exception_handler
    !!   call fckit_exception%set_handler( exception_handler )
    !! end subroutine
    !!```
end type fckit_exception_type

type(fckit_exception_type) :: fckit_exception
  !! Instance of the [[fckit_exception_module:fckit_exception_type(type)]] type

contains

!------------------------------------------------------------------------------

subroutine set_handler( exception_handler )


  use, intrinsic :: iso_c_binding, only : c_funloc
  procedure(fckit_exception_handler) :: exception_handler
  call fckit__set_abort_handler( c_funloc(exception_handler) )
end subroutine

!------------------------------------------------------------------------------

subroutine abort( what, file, line, function )
  use fckit_c_interop_module, only : c_str

  character(len=*), optional :: what
    !! what for abort

  character(len=*), optional :: file
    !! File path where aborted (hint: use ```___FILE___``` fortran-line-length permitting)

  integer, optional :: line
    !! Line in file where aborted (hint: use ```___LINE___```)

  character(len=*), optional :: function
    !!  Function where aborted

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  character(len=:), allocatable :: opt_what
  character(len=:), allocatable :: opt_file
  integer :: opt_line
  character(len=:), allocatable :: opt_function

  if( present(what) ) then
    opt_what = what
  else
    opt_what = ""
  endif
  if( present(file) ) then
    opt_file = file
  else
    opt_file = ""
  endif
  if( present(line) ) then
    opt_line = line
  else
    opt_line = 0
  endif
  if( present(function) ) then
    opt_function = function
  else
    opt_function = ""
  endif

  call fckit__abort( c_str(opt_what), c_str(opt_file), opt_line, c_str(opt_function) )
end subroutine

!------------------------------------------------------------------------------

function what()
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module

  character(len=:), allocatable :: what
    !! what for aborting

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  type(c_ptr)    :: what_c_ptr
  integer(c_int) :: what_size
  integer(c_int) :: error_code

  error_code = fckit__exception_what(what_c_ptr,what_size)
  allocate(character(len=what_size) :: what )
  what = c_ptr_to_string(what_c_ptr)
  call c_ptr_free(what_c_ptr)
end function

!------------------------------------------------------------------------------

function location_is_set() result(location)
  logical :: location
    !! True if location is present

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  if( fckit__exception_location() == 0 ) then
    location = .false.
  else
    location = .true.
  endif
end function

!------------------------------------------------------------------------------

function location_file() result(file)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module

  character(len=:), allocatable :: file
    !! File where abort is called

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  type(c_ptr)    :: file_c_ptr
  integer(c_int) :: file_size
  integer(c_int) :: error_code

  error_code = fckit__exception_file(file_c_ptr,file_size)
  allocate(character(len=file_size) :: file )
  file = c_ptr_to_string(file_c_ptr)
  call c_ptr_free(file_c_ptr)
end function

!------------------------------------------------------------------------------

function location_line() result(line)
  integer :: line
    !! Line where abort is called

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  line = fckit__exception_line()
end function

!------------------------------------------------------------------------------

function location_function() result(function)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module

  character(len=:), allocatable :: function
    !! Function where abort is called

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  type(c_ptr)    :: function_c_ptr
  integer(c_int) :: function_size
  integer(c_int) :: error_code

  error_code = fckit__exception_function(function_c_ptr,function_size)
  allocate(character(len=function_size) :: function )
  function = c_ptr_to_string(function_c_ptr)
  call c_ptr_free(function_c_ptr)
end function

!------------------------------------------------------------------------------

function callstack()
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module

  character(len=:), allocatable :: callstack
    !! Callstack is called

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  type(c_ptr)    :: callstack_c_ptr
  integer(c_int) :: callstack_size
  integer(c_int) :: error_code

  error_code = fckit__exception_callstack(callstack_c_ptr,callstack_size)
  allocate(character(len=callstack_size) :: callstack )
  callstack = c_ptr_to_string(callstack_c_ptr)
  call c_ptr_free(callstack_c_ptr)
end function

!------------------------------------------------------------------------------

subroutine throw( what, file, line, function )
  use fckit_c_interop_module, only : c_str

  character(len=*) :: what
  character(len=*), optional :: file
  integer, optional :: line
  character(len=*), optional :: function

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  character(len=:), allocatable :: opt_file
  integer :: opt_line
  character(len=:), allocatable :: opt_function

  if( present(file) ) then
    opt_file = file
  else
    opt_file = ""
  endif
  if( present(line) ) then
    opt_line = line
  else
    opt_line = 0
  endif
  if( present(function) ) then
    opt_function = function
  else
    opt_function = ""
  endif

  call fckit__exception_throw( c_str(what), c_str(opt_file), opt_line, c_str(opt_function) )
end subroutine

!------------------------------------------------------------------------------

end module fckit_exception_module

