module fckit_log_module
  implicit none
private

public :: log
! public :: simplelogtarget
! public :: prefixlogtarget
! public :: timestamplogtarget

type :: fckit_logtarget
  character(len=20) :: name
contains
end type

!------------------------------------------------------------------------------
! Logger singleton
type :: fckit_log_type
  integer :: SIMPLE = 0
  integer :: PREFIX = 1
  integer :: TIMESTAMP = 2
contains
  procedure, nopass, public :: debug
  procedure, nopass, public :: info
  procedure, nopass, public :: warning
  procedure, nopass, public :: error
  procedure, nopass, public :: panic
  procedure, nopass, public :: add_fortran_unit
  procedure, nopass, public :: set_fortran_unit
  procedure, nopass, public :: reset
end type
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
  subroutine fckit__log_reset() bind(c)
  end subroutine
  
end interface

!========================================================
contains
!========================================================


function simplelogtarget() result(this)
  type(fckit_logtarget) :: this
  this%name = "simple"
end function

function prefixlogtarget() result(this)
  type(fckit_logtarget) :: this
  this%name = "prefix"
end function

function timestamplogtarget() result(this)
  type(fckit_logtarget) :: this
  this%name = "timestamp"
end function

subroutine debug(msg,newl,flush)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module, only : c_str
  character(kind=c_char,len=*), intent(in) :: msg
  logical, intent(in), optional :: newl, flush
  integer :: opt_newl, opt_flush
  opt_newl  = 1 ; if( present(newl) ) then; if( .not. newl )  opt_newl  = 0; endif
  opt_flush = 1 ; if( present(flush)) then; if( .not. flush ) opt_flush = 0; endif
  call fckit__log_debug(c_str(msg),opt_newl,opt_flush)
end subroutine

subroutine info(msg,newl,flush)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module, only : c_str
  character(kind=c_char,len=*), intent(in) :: msg
  logical, intent(in), optional :: newl, flush
  integer :: opt_newl, opt_flush
  opt_newl  = 1 ; if( present(newl) ) then; if( .not. newl )  opt_newl  = 0; endif
  opt_flush = 1 ; if( present(flush)) then; if( .not. flush ) opt_flush = 0; endif
  call fckit__log_info(c_str(msg),opt_newl,opt_flush)
end subroutine

subroutine warning(msg,newl,flush)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module, only : c_str
  character(kind=c_char,len=*), intent(in) :: msg
  logical, intent(in), optional :: newl, flush
  integer :: opt_newl, opt_flush
  opt_newl  = 1 ; if( present(newl) ) then; if( .not. newl )  opt_newl  = 0; endif
  opt_flush = 1 ; if( present(flush)) then; if( .not. flush ) opt_flush = 0; endif
  call fckit__log_warning(c_str(msg),opt_newl,opt_flush)
end subroutine

subroutine error(msg,newl,flush)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module, only : c_str
  character(kind=c_char,len=*), intent(in) :: msg
  logical, intent(in), optional :: newl, flush
  integer :: opt_newl, opt_flush
  opt_newl  = 1 ; if( present(newl) ) then; if( .not. newl )  opt_newl  = 0; endif
  opt_flush = 1 ; if( present(flush)) then; if( .not. flush ) opt_flush = 0; endif
  call fckit__log_error(c_str(msg),opt_newl,opt_flush)
end subroutine

subroutine panic(msg)
  use, intrinsic :: iso_c_binding
  character(kind=c_char,len=*), intent(in) :: msg
  write(0,'(A)') msg
end subroutine

subroutine add_fortran_unit(unit,style)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module
  use fckit_main_module
  integer(c_int), intent(in) :: unit
  integer(c_int), intent(in), optional :: style
  integer(c_int) :: opt_style
  opt_style = log%PREFIX
  if( present( style ) ) opt_style = style
  call fckit__log_set_fortran_unit(unit,style)
end subroutine


subroutine set_fortran_unit(unit,style)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module
  use fckit_main_module
  integer(c_int), intent(in) :: unit
  integer(c_int), intent(in), optional :: style
  integer(c_int) :: opt_style
  opt_style = log%PREFIX
  if( present( style ) ) opt_style = style
  call fckit__log_set_fortran_unit(unit,style)
end subroutine


subroutine reset()
  use, intrinsic :: iso_c_binding
  use fckit_main_module
  call fckit__log_reset()
end subroutine

end module fckit_log_module
