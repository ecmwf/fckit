! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fckit.h"

module fckit_main_module
  !! author: Willem Deconinck
  !!
  !! Module providing [[fckit_main_module:fckit_main(variable)]] global variable
  !! to set up the ```eckit::Main``` object required by all libraries relying on eckit
  !!
  !!@Note
  !! ```fckit_main%initialize()``` will set up default log channels to be used by ```eckit::Log``` and ```fckit_log```.
  !! Refer to [[fckit_log_module:fckit_log(variable)]] to adapt the log channels to suit your needs
  !!@Endnote
  !!

implicit none
private

#include "fckit_main.inc"

public :: fckit_main

type, FORD_PRIVATE :: fckit_main_type
  !! Private type of [[fckit_main_module:fckit_main(variable)]] module variable
  !!
  !! It wraps ```eckit::Main```, allowing C++ compatibility
  !! with understanding the command-line arguments

contains

  procedure, nopass, public :: ready
    !! Check if Main was initialised.
    !!
    !!####Example usage:
    !!
    !!```fortran
    !! if( fckit_main%ready() ) then
    !!   ! fckit/eckit Main is ready (initialised)
    !! endif
    !!```

  procedure, nopass, public :: initialise
    !! display: none
    !! Initialise Main if not initialised
    !!
    !! Internally calls eckit::Main::initialise(argc,argv)
    !! with the command-line arguments acquired from the Fortran runtime.
    !!
    !! When using fckit or libraries based on eckit/fckit, this
    !! should be called as early as possible in the main program.
    !!
    !!####Example usage:
    !!
    !!```fortran
    !! call fckit_main%initialise()
    !!```

  procedure, nopass, public :: finalise
    !! Finalise Main if not finalised
    !!
    !! This function is to be called at the end of the main program.
    !! Calling this function is optional as the program should be capable to clean up
    !! itself. It may however provide a hook to clean up and print in future
    !! eckit/fckit releases.
    !!
    !!####Example usage:
    !!
    !!```fortran
    !! call fckit_main%finalise()
    !!```

  procedure, nopass, public :: taskID
    !! Get the taskID; typically this is the ```MPI_RANK``` in ```MPI_COMM_WORLD```
    !!
    !! This value can be changed using [[fckit_main_type:set_taskID(bound)]]
    !!
    !!####Example usage:
    !!
    !!```fortran
    !! integer :: taskID
    !! taskID = fckit_main%taskID()
    !!```

  procedure, nopass, public :: set_taskID
    !! Set the taskID; typically this is the MPI rank
    !!
    !! Upon ```call fckit_main%initialise()```, the taskID is
    !! set to the ```MPI_RANK``` in ```MPI_COMM_WORLD```.
    !!
    !! Use this function to change this afterwards.
    !!
    !!####Example usage:
    !!
    !!```fortran
    !! call fckit_main%set_taskID( int(taskID) )
    !!```

  procedure, nopass, public :: debug
    !! Check if debug flag is set (environment or command-line)

  procedure, nopass, public :: name => main_name
    !! Get the name of the program (command-line name)
    !!
    !!####Example usage:
    !!
    !!```fortran
    !! character(len=:), allocatable :: name
    !! call fckit_main%name( name )
    !!```

  procedure, nopass, public :: displayname
    !! Get the displayname of the program.
    !!
    !! This is by default the command-line name but can be overwritten
    !! using --displayname=\<NAME\> command-line argument
    !!
    !!####Example usage:
    !!
    !!```fortran
    !!    character(len=:), allocatable :: displayname
    !!    call fckit_main%displayname( displayname )
    !!```

  procedure, nopass, public :: init => initialise
    !! deprecated: true
    !! Deprecated. Use [[fckit_main_type:initialise(bound)]] instead

  procedure, nopass, public :: final => finalise
    !! deprecated: true
    !! Finalise Main if not finalised
    !! Deprecated. Use [[fckit_main_type:finalise(bound)]] instead

end type fckit_main_type

type(fckit_main_type), save :: fckit_main
  !! Instance of [[fckit_main_module:fckit_main_type(type)]]

contains

!------------------------------------------------------------------------------

subroutine initialise()
  use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t
  use fckit_c_interop_module
  integer, save :: argc
  type(c_ptr), save :: argv(15)
  integer(c_int32_t):: error_code
  call get_c_commandline_arguments(argc,argv)
  error_code = fckit__main_init(argc,argv)
end subroutine

subroutine finalise()
  call fckit__main_finalise()
end subroutine

function ready()
  use, intrinsic :: iso_c_binding, only : c_int32_t
  use fckit_c_interop_module
  logical :: ready
  integer(c_int32_t) :: ready_int
  integer(c_int32_t) :: error_code
  error_code = fckit__main_ready(ready_int)
  if( ready_int == 0 ) then
    ready = .false.
  else
    ready = .true.
  endif
end function

function taskID()
  use, intrinsic :: iso_c_binding, only : c_int32_t
  use fckit_c_interop_module
  integer(c_int32_t) :: taskID
  integer(c_int32_t) :: error_code
  error_code = fckit__main_taskID(taskID)
end function

subroutine set_taskID(taskID)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  use fckit_c_interop_module
  integer(c_int32_t), intent(in) :: taskID
  integer(c_int32_t) :: error_code
  error_code = fckit__main_setTaskID(taskID)
end subroutine

function debug()
  use, intrinsic :: iso_c_binding, only : c_int32_t
  logical :: debug
  integer(c_int32_t) :: debug_int
  debug_int = fckit__main_debug()
  debug = .false.
  if( debug_int==1 ) debug = .true.
end function

function verbose()
  use, intrinsic :: iso_c_binding, only : c_int32_t
  logical :: verbose
  integer(c_int32_t) :: verbose_int
  verbose_int = fckit__main_verbose()
  verbose = .false.
  if( verbose_int==1 .OR. debug() ) verbose = .true.
end function

subroutine main_name(name)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module
  character(kind=c_char,len=:), allocatable, intent(inout) :: name
    !! Name will be allocated.
  type(c_ptr) :: name_c_ptr
  integer(c_size_t) :: name_size
  integer(c_int32_t) :: error_code
  error_code = fckit__main_name(name_c_ptr,name_size)
  FCKIT_ALLOCATE_CHARACTER(name,name_size)
  name = c_ptr_to_string(name_c_ptr)
  call c_ptr_free(name_c_ptr)
end subroutine

subroutine displayname(name)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module
  character(kind=c_char,len=:), allocatable, intent(inout) :: name
    !! Name will be allocated.
  type(c_ptr) :: name_c_ptr
  integer(c_size_t) :: name_size
  integer(c_int32_t) :: error_code
  error_code = fckit__main_displayname(name_c_ptr,name_size)
  FCKIT_ALLOCATE_CHARACTER(name,name_size)
  name = c_ptr_to_string(name_c_ptr)
  call c_ptr_free(name_c_ptr)
end subroutine

end module fckit_main_module

