module fckit_log_module
use fckit_object_module, only: fckit_object
implicit none
private

public :: fckit_log
public :: log
public :: logchannel

private :: fckit_object

type, extends(fckit_object) :: logchannel
contains
  procedure, public :: delete
end type


!------------------------------------------------------------------------------
! Logger singleton
type :: fckit_log_type
  integer :: SIMPLE = 0
  integer :: PREFIX = 1
  integer :: TIMESTAMP = 2
contains

  ! subroutines
  procedure, nopass, public :: debug
  procedure, nopass, public :: info
  procedure, nopass, public :: warning
  procedure, nopass, public :: error
  procedure, nopass, public :: panic
  procedure, nopass, public :: reset
  procedure, nopass, public :: flush
  procedure, nopass, public :: add_stdout
  procedure, nopass, public :: set_stdout
  procedure, nopass, public :: add_fortran_unit
  procedure, nopass, public :: set_fortran_unit
  procedure, nopass, public :: add_file
  procedure, nopass, public :: set_file

  ! functions
  procedure, nopass, public :: info_channel
  procedure, nopass, public :: warning_channel
  procedure, nopass, public :: error_channel
  procedure, nopass, public :: debug_channel

end type
type(fckit_log_type) :: fckit_log
type(fckit_log_type) :: log
!------------------------------------------------------------------------------

interface
  ! void fckit__log_debug(char *msg, int newl, int flush)
  subroutine fckit__log_debug(msg,newl,flush) bind(c)
    use, intrinsic :: iso_c_binding , only : c_char, c_int
    character(kind=c_char), dimension(*) :: msg
    integer(c_int), value :: newl
    integer(c_int), value :: flush
  end subroutine
  ! void fckit__log_info(char *msg, int newl, int flush)
  subroutine fckit__log_info(msg,newl,flush) bind(c)
    use, intrinsic :: iso_c_binding , only : c_char, c_int
    character(kind=c_char), dimension(*) :: msg
    integer(c_int), value :: newl
    integer(c_int), value :: flush
  end subroutine
  ! void fckit__log_warning(char *msg, int newl, int flush)
  subroutine fckit__log_warning(msg,newl,flush) bind(c)
    use, intrinsic :: iso_c_binding , only : c_char, c_int
    character(kind=c_char), dimension(*) :: msg
    integer(c_int), value :: newl
    integer(c_int), value :: flush
  end subroutine
  ! void fckit__log_error(char *msg, int newl, int flush)
  subroutine fckit__log_error(msg,newl,flush) bind(c)
    use, intrinsic :: iso_c_binding , only : c_char, c_int
    character(kind=c_char), dimension(*) :: msg
    integer(c_int), value :: newl
    integer(c_int), value :: flush
  end subroutine
  subroutine fckit__log_add_fortran_unit(unit,style) bind(c)
    use, intrinsic :: iso_c_binding, only : c_int
    integer(c_int), value :: unit
    integer(c_int), value :: style
  end subroutine
  subroutine fckit__log_set_fortran_unit(unit,style) bind(c)
    use, intrinsic :: iso_c_binding, only : c_int
    integer(c_int), value :: unit
    integer(c_int), value :: style
  end subroutine
  subroutine fckit__log_add_file(path,style) bind(c)
    use, intrinsic :: iso_c_binding , only : c_char, c_int
    character(kind=c_char), dimension(*) :: path
    integer(c_int), value :: style
  end subroutine
  subroutine fckit__log_set_file(path,style) bind(c)
    use, intrinsic :: iso_c_binding , only : c_char, c_int
    character(kind=c_char), dimension(*) :: path
    integer(c_int), value :: style
  end subroutine
  subroutine fckit__log_add_stdout(style) bind(c)
    use, intrinsic :: iso_c_binding , only : c_int
    integer(c_int), value :: style
  end subroutine
  subroutine fckit__log_set_stdout(style) bind(c)
    use, intrinsic :: iso_c_binding , only : c_int
    integer(c_int), value :: style
  end subroutine
  subroutine fckit__log_reset() bind(c)
  end subroutine
  subroutine fckit__log_flush() bind(c)
  end subroutine
  function fckit__log_info_channel() bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr
    type(c_ptr) :: fckit__log_info_channel
  end function
  function fckit__log_warning_channel() bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr
    type(c_ptr) :: fckit__log_warning_channel
  end function
  function fckit__log_error_channel() bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr
    type(c_ptr) :: fckit__log_error_channel
  end function
  function fckit__log_debug_channel() bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr
    type(c_ptr) :: fckit__log_debug_channel
  end function

end interface

!========================================================
contains
!========================================================

subroutine debug(msg,newl,flush)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module, only : c_str_right_trim
  character(kind=c_char,len=*), intent(in) :: msg
  logical, intent(in), optional :: newl, flush
  integer :: opt_newl, opt_flush
  opt_newl  = 1 ; if( present(newl) ) then; if( .not. newl ) opt_newl  = 0; endif
  opt_flush = 0 ; if( present(flush)) then; if(      flush ) opt_flush = 1; endif
  call fckit__log_debug(c_str_right_trim(msg),opt_newl,opt_flush)
end subroutine


subroutine info(msg,newl,flush)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module, only : c_str_right_trim
  character(kind=c_char,len=*), intent(in) :: msg
  logical, intent(in), optional :: newl, flush
  integer :: opt_newl, opt_flush
  opt_newl  = 1 ; if( present(newl) ) then; if( .not. newl ) opt_newl  = 0; endif
  opt_flush = 0 ; if( present(flush)) then; if(      flush ) opt_flush = 1; endif
  call fckit__log_info(c_str_right_trim(msg),opt_newl,opt_flush)
end subroutine


subroutine warning(msg,newl,flush)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module, only : c_str_right_trim
  character(kind=c_char,len=*), intent(in) :: msg
  logical, intent(in), optional :: newl, flush
  integer :: opt_newl, opt_flush
  opt_newl  = 1 ; if( present(newl) ) then; if( .not. newl ) opt_newl  = 0; endif
  opt_flush = 0 ; if( present(flush)) then; if(      flush ) opt_flush = 1; endif
  call fckit__log_warning(c_str_right_trim(msg),opt_newl,opt_flush)
end subroutine


subroutine error(msg,newl,flush)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module, only : c_str_right_trim
  character(kind=c_char,len=*), intent(in) :: msg
  logical, intent(in), optional :: newl, flush
  integer :: opt_newl, opt_flush
  opt_newl  = 1 ; if( present(newl) ) then; if( .not. newl ) opt_newl  = 0; endif
  opt_flush = 0 ; if( present(flush)) then; if(      flush ) opt_flush = 1; endif
  call fckit__log_error(c_str_right_trim(msg),opt_newl,opt_flush)
end subroutine


subroutine panic(msg)
  use, intrinsic :: iso_c_binding
  character(kind=c_char,len=*), intent(in) :: msg
  write(0,'(A)') msg
end subroutine


subroutine add_fortran_unit(unit,style)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module
  integer(c_int), intent(in) :: unit
  integer(c_int), intent(in), optional :: style
  integer(c_int) :: opt_style
  opt_style = fckit_log%PREFIX
  if( present( style ) ) opt_style = style
  call fckit__log_add_fortran_unit(unit,opt_style)
end subroutine


subroutine set_fortran_unit(unit,style)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module
  integer(c_int), intent(in) :: unit
  integer(c_int), intent(in), optional :: style
  integer(c_int) :: opt_style
  opt_style = fckit_log%PREFIX
  if( present( style ) ) opt_style = style
  call fckit__log_set_fortran_unit(unit,opt_style)
end subroutine

subroutine add_file(path,style)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module
  character(len=*), intent(in) :: path
  integer(c_int), intent(in), optional :: style
  integer(c_int) :: opt_style
  opt_style = fckit_log%PREFIX
  if( present( style ) ) opt_style = style
  call fckit__log_add_file(c_str(path),opt_style)
end subroutine


subroutine set_file(path,style)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module
  character(len=*), intent(in) :: path
  integer(c_int), intent(in), optional :: style
  integer(c_int) :: opt_style
  opt_style = fckit_log%PREFIX
  if( present( style ) ) opt_style = style
  call fckit__log_set_file(c_str(path),opt_style)
end subroutine

subroutine add_stdout(style)
  use, intrinsic :: iso_c_binding
  integer(c_int), intent(in), optional :: style
  integer(c_int) :: opt_style
  opt_style = fckit_log%PREFIX
  if( present( style ) ) opt_style = style
  call fckit__log_add_stdout(opt_style)
end subroutine


subroutine set_stdout(style)
  use, intrinsic :: iso_c_binding
  integer(c_int), intent(in), optional :: style
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
  type(logchannel) :: channel
  call channel%reset_c_ptr( fckit__log_info_channel() )
end function


function warning_channel() result(channel)
  type(logchannel) :: channel
  call channel%reset_c_ptr( fckit__log_warning_channel() )
end function


function error_channel() result(channel)
  type(logchannel) :: channel
  call channel%reset_c_ptr( fckit__log_error_channel() )
end function


function debug_channel() result(channel)
  type(logchannel) :: channel
  call channel%reset_c_ptr( fckit__log_debug_channel() )
end function


subroutine delete(this)
  class(logchannel), intent(inout) :: this
  ! do nothing
end subroutine

end module fckit_log_module
