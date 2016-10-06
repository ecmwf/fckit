
module fckit_runtime_module
implicit none

public :: main

private

type :: fckit_main
contains
  procedure, nopass, public :: ready
  procedure, nopass, public :: init
end type

type(fckit_main), save :: main

!------------------------------------------------------------------------------
interface
  !int fckit__runtime_main_init (int argc, char* argv[])
  function fckit__runtime_main_init(argc,argv) result(error_code) bind(c,name="fckit__runtime_main_init")
    use iso_c_binding, only: c_int, c_ptr
    integer(c_int) :: error_code
    integer(c_int), value :: argc
    type(c_ptr), dimension(*) :: argv
  end function

  !int fckit__runtime_main_ready (int& ready)
  function fckit__runtime_main_ready(ready) result(error_code) bind(c,name="fckit__runtime_main_ready")
    use iso_c_binding, only: c_int
    integer(c_int) :: error_code
    integer(c_int) :: ready
  end function

end interface
!------------------------------------------------------------------------------

! =============================================================================
CONTAINS
! =============================================================================

subroutine init()
  use, intrinsic :: iso_c_binding, only : c_ptr
  use fckit_c_interop_module
  integer, save :: argc
  type(c_ptr), save :: argv(15)
  integer:: error_code

  call get_c_commandline_arguments(argc,argv)
  error_code = fckit__runtime_main_init(argc,argv)
end subroutine

function ready()
  use, intrinsic :: iso_c_binding, only : c_int
  use fckit_c_interop_module
  logical :: ready
  integer(c_int) :: ready_int
  integer:: error_code
  error_code = fckit__runtime_main_ready(ready_int)
  if( ready_int == 0 ) then
    ready = .false.
  else
    ready = .true.
  endif
end function


end module fckit_runtime_module

