! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module fckit_signal_module
  !! author: Willem Deconinck
  !!
  !! Module providing [[fckit_signal_module:fckit_signal(variable)]] global variable

implicit none
private

#include "fckit_signal.inc"

public :: fckit_signal
public :: fckit_signal_handler


type, FORD_PRIVATE :: fckit_signal_type
  !! Private type of [[fckit_signal_module:fckit_signal(variable)]] module variable
  !! exposing signal codes

contains
  procedure, nopass, public :: raise
    !! Raise a signal with given signal code
    !!
    !!####Example usage:
    !!
    !!```fortran
    !! call fckit_signal%raise(fckit_signal%SIGABRT())
    !!```

  procedure, nopass, public :: set_handler
    !! Set custom signal handler
    !!
    !!####Example usage
    !!
    !!```fortran
    !! subroutine custom_signal_handler( signum ) bind(c)
    !!   integer :: signum
    !!   ! handle abort
    !! end subroutine
    !!
    !! subroutine set_custom_signal_handler()
    !!   external :: custom_signal_handler
    !!   procedure(fckit_signal_handler), pointer :: signal_handler => custom_signal_handler
    !!   call fckit_signal%set_handler( fckit_signal%SIGABRT(), signal_handler )
    !! end subroutine
    !!```

  procedure, nopass, public :: set_handlers
    !! Set signal handlers to a fckit-internal signal handler
    !!
    !! This fckit-internal signal handler will print a message containing the
    !! backtrace, and will then call the abort-handler to terminate. The
    !! abort-handler can be modified with the subroutine
    !! [[fckit_exception_module:fckit_exception(type):set_handler(subroutine)]]
    !!
    !!####Example usage
    !!```fortran
    !! call fckit_signal%set_handlers()
    !!```

  procedure, nopass, public :: restore_handler
    !! Restore the signal handler for the given signal code to the default.
    !!
    !! Available signal codes are shown in [[fckit_signal_module:fckit_signal_type(type)]]
    !!
    !!####Example usage:
    !!
    !!```fortran
    !! call fckit_signal%restore_handler( fckit_signal%SIGABRT() )
    !!```

  procedure, nopass, public :: restore_handlers
    !! Restore all the signal handlers to the default.

  procedure, nopass, public :: SIGABRT !! Return code for Abort
  procedure, nopass, public :: SIGINT  !! Return code for Interrupt (e.g. CTRL+C)
  procedure, nopass, public :: SIGKILL !! Return code for Kill
  procedure, nopass, public :: SIGALRM !! Return code for Alarm
  procedure, nopass, public :: SIGTERM !! Return code for Terminate
  procedure, nopass, public :: SIGILL  !! Return code for Illegal
  procedure, nopass, public :: SIGSEGV !! Return code for Segmentation violation
  procedure, nopass, public :: SIGFPE  !! Return code for Floating Point Exception
end type

type(fckit_signal_type), save :: fckit_signal
  !! Instance of [[fckit_signal_module:fckit_signal_type(type)]]


contains

!------------------------------------------------------------------------------

subroutine set_handler( signum, signal_handler )
  use, intrinsic :: iso_c_binding, only : c_funloc, c_int32_t

  integer(c_int32_t) :: signum
    !! signal code

  procedure(fckit_signal_handler), optional :: signal_handler
    !! procedure with signature

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  if( present( signal_handler) ) then
    call fckit__set_signal_handler( signum, c_funloc(signal_handler) )
  else
    call fckit__set_fckit_signal_handler( signum )
  endif
end subroutine

!------------------------------------------------------------------------------

subroutine set_handlers()
  call fckit__set_fckit_signal_handlers()
end subroutine

!------------------------------------------------------------------------------

subroutine raise( signum )
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: signum
  !! Signal code
  call fckit__raise_signal(signum)
end subroutine

!------------------------------------------------------------------------------

subroutine restore_handler( signum )
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: signum
    !! Signal code
  call fckit__restore_signal_handler(signum)
end subroutine

!------------------------------------------------------------------------------

subroutine restore_handlers()
  call fckit__restore_all_signal_handlers()
end subroutine

!------------------------------------------------------------------------------

function SIGABRT() result(signum)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: signum
  signum = fckit__SIGABRT()
end function

function SIGINT() result(signum)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: signum
  signum = fckit__SIGINT()
end function

function SIGKILL() result(signum)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: signum
  signum = fckit__SIGKILL()
end function

function SIGALRM() result(signum)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: signum
  signum = fckit__SIGALRM()
end function

function SIGILL() result(signum)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: signum
  signum = fckit__SIGILL()
end function

function SIGSEGV() result(signum)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: signum
  signum = fckit__SIGSEGV()
end function

function SIGTERM() result(signum)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: signum
  signum = fckit__SIGTERM()
end function

function SIGFPE() result(signum)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: signum
  signum = fckit__SIGFPE()
end function

!------------------------------------------------------------------------------

end module fckit_signal_module

