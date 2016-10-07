module fckit_log_module
  implicit none
private

public :: fckit_log

!------------------------------------------------------------------------------
! Logger singleton
type :: fckit_log_type
contains
  procedure, nopass, public :: debug
  procedure, nopass, public :: info
  procedure, nopass, public :: warning
  procedure, nopass, public :: error
  procedure, nopass, public :: panic
  procedure, nopass, public :: add_fortran_unit
  procedure, nopass, public :: set_fortran_unit
end type
type(fckit_log_type) :: fckit_log
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
  subroutine fckit__log_add_fortran_unit(unit) bind(c)
    use, intrinsic :: iso_c_binding, only : c_int
    integer(c_int), value :: unit
  end subroutine
  subroutine fckit__log_set_fortran_unit(unit) bind(c)
    use, intrinsic :: iso_c_binding, only : c_int
    integer(c_int), value :: unit
  end subroutine
end interface

!========================================================
contains
!========================================================

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

subroutine add_fortran_unit(unit)
  use, intrinsic :: iso_c_binding
  integer(c_int) :: unit
  call fckit__log_add_fortran_unit(unit)
end subroutine

subroutine set_fortran_unit(unit)
  use, intrinsic :: iso_c_binding
  integer(c_int) :: unit
  call fckit__log_set_fortran_unit(unit)
end subroutine

end module fckit_log_module

!------------------------------------------------------------------------------

! Callback function, used from C++ side
subroutine fckit_write_to_fortran_unit(unit,msg_cptr) bind(C)
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr
  use fckit_c_interop_module, only : c_ptr_to_string
  integer(c_int), value, intent(in) :: unit
  type(c_ptr), value, intent(in) :: msg_cptr
  character(len=:), allocatable :: msg
  msg = c_ptr_to_string(msg_cptr)
  write(unit,'(A)') msg
end subroutine
