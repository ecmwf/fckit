! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fckit.h"

module fckit_log_module
  !! Provides [[fckit_log_module:fckit_log(variable)]] for logging and to configure logging

use fckit_object_module, only: fckit_object
implicit none
private
public :: log ! DEPRECATED, USE fckit_log INSTEAD!
public :: fckit_log
public :: fckit_logchannel
private :: fckit_object

#include "fckit_log.inc"

type, FORD_PRIVATE :: fckit_log_type
  !! Private type of [[fckit_log_module:fckit_log(variable)]] module variable
  !!
  !! It wraps ```eckit::Log```, allowing Fortran and C++ code to log to the
  !! same output channels

  integer :: SIMPLE = 0
    !! Style for logging without any prefix
  integer :: PREFIX = 1
    !! Style for logging with prefix
    !!
    !!    (I) --> info
    !!    (W) --> warning
    !!    (E) --> error
    !!    (D) --> debug

  integer :: TIMESTAMP = 2
    !! Style for logging with prefix that contains time stamp and taskID
    !!
    !!    <taskID> <TIME> (I) --> info
    !!    <taskID> <TIME> (W) --> warning
    !!    <taskID> <TIME> (E) --> error
    !!    <taskID> <TIME> (D) --> debug

contains

  procedure, nopass, public :: debug
    !! Log to debug channel

  procedure, nopass, public :: info
    !! Log to info channel

  procedure, nopass, public :: warning
    !! Log to warning channel

  procedure, nopass, public :: error
    !! Log to error channel

  procedure, nopass, public :: panic
    !! Log to panic channel (fortran_unit 0, always)

  procedure, nopass, public :: reset
    !! Reset all log channels (No more logging)

  procedure, nopass, public :: flush
    !! Flush all log channels (empty buffers)

  procedure, nopass, public :: add_stdout
    !! Add the C++ ```std::cout``` output stream to each logchannel

  procedure, nopass, public :: set_stdout
    !! Replace each logchannels streams with the C++ ```std::cout``` output stream

  procedure, nopass, public :: add_fortran_unit
    !! Add a given fortran unit as output stream to each logchannel

  procedure, nopass, public :: set_fortran_unit
    !! Replace each logchannel streams with a given fortran unit

  procedure, nopass, public :: add_file
    !! Add a given file as output stream to each logchannel.
    !!
    !! The file will be (re)created.

  procedure, nopass, public :: set_file
    !! Replace each logchannels streams with a given file
    !!
    !! The file will be (re)created.

  procedure, nopass, public :: info_channel
    !! Return the info [[fckit_logchannel(type)]]

  procedure, nopass, public :: warning_channel
    !! Return the warning [[fckit_logchannel(type)]]

  procedure, nopass, public :: error_channel
    !! Return the error [[fckit_logchannel(type)]]

  procedure, nopass, public :: debug_channel
    !! Return the debug [[fckit_logchannel(type)]]

end type fckit_log_type

type(fckit_log_type) :: fckit_log
  !! Instance of [[fckit_log_module:fckit_log_type(type)]]

type(fckit_log_type) :: log
  !! deprecated: true
  !! Deprecated instance of [[fckit_log_module:fckit_log_type(type)]].
  !! Use [[fckit_log_module:fckit_log(variable)]] instead!

!------------------------------------------------------------------------------

type, extends(fckit_object) :: fckit_logchannel
  !! Log channel (any of info, warning, error, debug)
  !!
  !! Wraps ```eckit::Channel```. This channel can be passed as
  !! arguments to functions that expect a ```std::ostream``` to log to.
contains
  procedure, public :: delete
    !! Delete internal C++ pointer
end type fckit_logchannel

!========================================================
contains
!========================================================

subroutine debug(msg,newl,flush)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module, only : c_str_right_trim
  character(kind=c_char,len=*), intent(in) :: msg
    !! Message to be logged
  logical, intent(in), optional :: newl
    !! Add newline character (```\n```) after message.
    !! Default ```.true.```
  logical, intent(in), optional :: flush
    !! Flush channel after message.
    !! Default ```.true.```
  integer :: opt_newl, opt_flush
  opt_newl  = 1 ; if( present(newl) ) then; if( .not.  newl ) opt_newl  = 0; endif
  opt_flush = 1 ; if( present(flush)) then; if( .not. flush ) opt_flush = 0; endif
  call fckit__log_debug(c_str_right_trim(msg),opt_newl,opt_flush)
end subroutine


subroutine info(msg,newl,flush)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module, only : c_str_right_trim
  character(kind=c_char,len=*), intent(in) :: msg
    !! Message to be logged
  logical, intent(in), optional :: newl
    !! Add newline character (```\n```) after message.
    !! Default ```.true.```
  logical, intent(in), optional :: flush
    !! Flush channel after message.
    !! Default ```.true.```
  integer :: opt_newl, opt_flush
  opt_newl  = 1 ; if( present(newl) ) then; if( .not.  newl ) opt_newl  = 0; endif
  opt_flush = 1 ; if( present(flush)) then; if( .not. flush ) opt_flush = 0; endif
  call fckit__log_info(c_str_right_trim(msg),opt_newl,opt_flush)
end subroutine


subroutine warning(msg,newl,flush)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module, only : c_str_right_trim
  character(kind=c_char,len=*), intent(in) :: msg
    !! Message to be logged
  logical, intent(in), optional :: newl
    !! Add newline character (```\n```) after message.
    !! Default ```.true.```
  logical, intent(in), optional :: flush
    !! Flush channel after message.
    !! Default ```.true.```
  integer :: opt_newl, opt_flush
  opt_newl  = 1 ; if( present(newl) ) then; if( .not.  newl ) opt_newl  = 0; endif
  opt_flush = 1 ; if( present(flush)) then; if( .not. flush ) opt_flush = 0; endif
  call fckit__log_warning(c_str_right_trim(msg),opt_newl,opt_flush)
end subroutine


subroutine error(msg,newl,flush)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module, only : c_str_right_trim
  character(kind=c_char,len=*), intent(in) :: msg
    !! Message to be logged
  logical, intent(in), optional :: newl
    !! Add newline character (```\n```) after message.
    !! Default ```.true.```
  logical, intent(in), optional :: flush
    !! Flush channel after message.
    !! Default ```.true.```
  integer :: opt_newl, opt_flush
  opt_newl  = 1 ; if( present(newl) ) then; if( .not.  newl ) opt_newl  = 0; endif
  opt_flush = 1 ; if( present(flush)) then; if( .not. flush ) opt_flush = 0; endif
  call fckit__log_error(c_str_right_trim(msg),opt_newl,opt_flush)
end subroutine


subroutine panic(msg)
  use, intrinsic :: iso_c_binding
  character(kind=c_char,len=*), intent(in) :: msg
    !! Message to be logged
  write(0,'(A)') msg
end subroutine


subroutine add_fortran_unit(unit,style)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module
  integer(c_int), intent(in) :: unit
    !! Fortran unit
  integer(c_int), intent(in), optional :: style
    !! Style to prefix the stream with. Options are:
    !!
    !! - [[fckit_log_module:fckit_log(variable)]]%[[fckit_log_type(type):SIMPLE(variable)]]
    !! - [[fckit_log_module:fckit_log(variable)]]%[[fckit_log_type(type):PREFIX(variable)]]
    !! - [[fckit_log_module:fckit_log(variable)]]%[[fckit_log_type(type):TIMESTAMP(variable)]]
  integer(c_int) :: opt_style
  opt_style = fckit_log%PREFIX
  if( present( style ) ) opt_style = style
  call fckit__log_add_fortran_unit(unit,opt_style)
end subroutine


subroutine set_fortran_unit(unit,style)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module
  integer(c_int), intent(in) :: unit
    !! Fortran unit
  integer(c_int), intent(in), optional :: style
    !! Style to prefix the stream with. Options are:
    !!
    !! - [[fckit_log_module:fckit_log(variable)]]%[[fckit_log_type(type):SIMPLE(variable)]]
    !! - [[fckit_log_module:fckit_log(variable)]]%[[fckit_log_type(type):PREFIX(variable)]]
    !! - [[fckit_log_module:fckit_log(variable)]]%[[fckit_log_type(type):TIMESTAMP(variable)]]
  integer(c_int) :: opt_style
  opt_style = fckit_log%PREFIX
  if( present( style ) ) opt_style = style
  call fckit__log_set_fortran_unit(unit,opt_style)
end subroutine

subroutine add_file(path,style)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module
  character(len=*), intent(in) :: path
    !! Path to file. File will be (re)created
  integer(c_int), intent(in), optional :: style
    !! Style to prefix the stream with. Options are:
    !!
    !! - [[fckit_log_module:fckit_log(variable)]]%[[fckit_log_type(type):SIMPLE(variable)]]
    !! - [[fckit_log_module:fckit_log(variable)]]%[[fckit_log_type(type):PREFIX(variable)]]
    !! - [[fckit_log_module:fckit_log(variable)]]%[[fckit_log_type(type):TIMESTAMP(variable)]]
  integer(c_int) :: opt_style
  opt_style = fckit_log%PREFIX
  if( present( style ) ) opt_style = style
  call fckit__log_add_file(c_str(path),opt_style)
end subroutine


subroutine set_file(path,style)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module
  character(len=*), intent(in) :: path
    !! Path to file. File will be (re)created
  integer(c_int), intent(in), optional :: style
    !! Style to prefix the stream with. Options are:
    !!
    !! - [[fckit_log_module:fckit_log(variable)]]%[[fckit_log_type(type):SIMPLE(variable)]]
    !! - [[fckit_log_module:fckit_log(variable)]]%[[fckit_log_type(type):PREFIX(variable)]]
    !! - [[fckit_log_module:fckit_log(variable)]]%[[fckit_log_type(type):TIMESTAMP(variable)]]
  integer(c_int) :: opt_style
  opt_style = fckit_log%PREFIX
  if( present( style ) ) opt_style = style
  call fckit__log_set_file(c_str(path),opt_style)
end subroutine

subroutine add_stdout(style)
  use, intrinsic :: iso_c_binding
  integer(c_int), intent(in), optional :: style
    !! Style to prefix the stream with. Options are:
    !!
    !! - [[fckit_log_module:fckit_log(variable)]]%[[fckit_log_type(type):SIMPLE(variable)]]
    !! - [[fckit_log_module:fckit_log(variable)]]%[[fckit_log_type(type):PREFIX(variable)]]
    !! - [[fckit_log_module:fckit_log(variable)]]%[[fckit_log_type(type):TIMESTAMP(variable)]]
  integer(c_int) :: opt_style
  opt_style = fckit_log%PREFIX
  if( present( style ) ) opt_style = style
  call fckit__log_add_stdout(opt_style)
end subroutine


subroutine set_stdout(style)
  use, intrinsic :: iso_c_binding
  integer(c_int), intent(in), optional :: style
    !! Style to prefix the stream with. Options are:
    !!
    !! - [[fckit_log_module:fckit_log(variable)]]%[[fckit_log_type(type):SIMPLE(variable)]]
    !! - [[fckit_log_module:fckit_log(variable)]]%[[fckit_log_type(type):PREFIX(variable)]]
    !! - [[fckit_log_module:fckit_log(variable)]]%[[fckit_log_type(type):TIMESTAMP(variable)]]
  integer(c_int) :: opt_style
  opt_style = fckit_log%PREFIX
  if( present( style ) ) opt_style = style
  call fckit__log_set_stdout(opt_style)
end subroutine


subroutine reset()
  call fckit__log_reset()
end subroutine

subroutine flush()
  call fckit__log_flush()
end subroutine

function info_channel() result(channel)
  type(fckit_logchannel) :: channel
  call channel%reset_c_ptr( fckit__log_info_channel() )
end function


function warning_channel() result(channel)
  type(fckit_logchannel) :: channel
  call channel%reset_c_ptr( fckit__log_warning_channel() )
end function


function error_channel() result(channel)
  type(fckit_logchannel) :: channel
  call channel%reset_c_ptr( fckit__log_error_channel() )
end function


function debug_channel() result(channel)
  type(fckit_logchannel) :: channel
  call channel%reset_c_ptr( fckit__log_debug_channel() )
end function


subroutine delete(this)
  class(fckit_logchannel), intent(inout) :: this
  ! do nothing
  FCKIT_SUPPRESS_UNUSED(this)
end subroutine

end module fckit_log_module
