
module fckit_main_module
implicit none
private

public :: fckit_main
public :: main

type :: fckit_main_type
contains
  procedure, nopass, public :: ready
  procedure, nopass, public :: init
  procedure, nopass, public :: final
  procedure, nopass, public :: taskID
  procedure, nopass, public :: set_taskID
  procedure, nopass, public :: debug
  procedure, nopass, public :: name => main_name
  procedure, nopass, public :: displayname => displayname
end type

type(fckit_main_type), save :: fckit_main
type(fckit_main_type), save :: main

!------------------------------------------------------------------------------
interface
  !int fckit__main_init (int argc, char* argv[])
  function fckit__main_init(argc,argv) &
    & result(error_code) bind(c,name="fckit__main_init")
    use iso_c_binding, only: c_int, c_ptr, c_char
    integer(c_int) :: error_code
    integer(c_int), value :: argc
    type(c_ptr), dimension(*) :: argv
  end function

  ! void fckit__main_finalise()
  subroutine fckit__main_finalise() bind(c)
  end subroutine


  !int fckit__main_ready (int& ready)
  function fckit__main_ready(ready) result(error_code) bind(c,name="fckit__main_ready")
    use iso_c_binding, only: c_int
    integer(c_int) :: error_code
    integer(c_int) :: ready
  end function
  !int fckit__main_taskID (int& taskID)
  function fckit__main_taskID(taskID) result(error_code) bind(c,name="fckit__main_taskID")
    use iso_c_binding, only: c_int
    integer(c_int) :: error_code
    integer(c_int) :: taskID
  end function

  function fckit__main_setTaskID(taskID) result(error_code) bind(c,name="fckit__main_setTaskID")
    use iso_c_binding, only: c_int
    integer(c_int) :: error_code
    integer(c_int), value :: taskID
  end function

  function fckit__main_debug() result(debug) bind(c)
    use iso_c_binding, only : c_int
    integer(c_int) :: debug
  end function

  !int fckit__main_name (char* &name, int &name_size)
  function fckit__main_name(name,name_size) result(error_code) bind(c)
    use iso_c_binding, only: c_int, c_ptr, c_char
    integer(c_int) :: error_code
    type(c_ptr) :: name
    integer(c_int) :: name_size
  end function

  !int fckit__main_displayname (char* &name, int &name_size)
  function fckit__main_displayname(name,name_size) result(error_code) bind(c)
    use iso_c_binding, only: c_int, c_ptr, c_char
    integer(c_int) :: error_code
    type(c_ptr) :: name
    integer(c_int) :: name_size
  end function
end interface
!------------------------------------------------------------------------------

! =============================================================================
CONTAINS
! =============================================================================

subroutine init()
  use, intrinsic :: iso_c_binding, only : c_ptr, c_int
  use fckit_c_interop_module
  integer, save :: argc
  type(c_ptr), save :: argv(15)
  integer(c_int):: error_code
  call get_c_commandline_arguments(argc,argv)
  error_code = fckit__main_init(argc,argv)
end subroutine

subroutine final()
  call fckit__main_finalise()
end subroutine

function ready()
  use, intrinsic :: iso_c_binding, only : c_int
  use fckit_c_interop_module
  logical :: ready
  integer(c_int) :: ready_int
  integer:: error_code
  error_code = fckit__main_ready(ready_int)
  if( ready_int == 0 ) then
    ready = .false.
  else
    ready = .true.
  endif
end function

function taskID()
  use, intrinsic :: iso_c_binding, only : c_int
  use fckit_c_interop_module
  logical :: ready
  integer(c_int) :: taskID
  integer:: error_code
  error_code = fckit__main_taskID(taskID)
end function

subroutine set_taskID(taskID)
  use, intrinsic :: iso_c_binding, only : c_int
  use fckit_c_interop_module
  integer(c_int), intent(in) :: taskID
  integer:: error_code
  error_code = fckit__main_setTaskID(taskID)
end subroutine

function debug()
  use, intrinsic :: iso_c_binding, only : c_int
  logical :: debug
  integer(c_int) :: debug_int
  debug_int = fckit__main_debug()
  debug = .false.
  if( debug_int==1 ) debug = .true.
end function

subroutine main_name(name)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module
  character(len=:), allocatable, intent(inout) :: name
  type(c_ptr) :: name_c_ptr
  integer(c_int) :: name_size
  integer(c_int) :: error_code
  error_code = fckit__main_name(name_c_ptr,name_size)
  allocate(character(len=name_size) :: name )
  name = c_ptr_to_string(name_c_ptr)
  call c_ptr_free(name_c_ptr)
end subroutine

subroutine displayname(name)
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module
  character(len=:), allocatable, intent(inout) :: name
  type(c_ptr) :: name_c_ptr
  integer(c_int) :: name_size
  integer(c_int) :: error_code
  error_code = fckit__main_displayname(name_c_ptr,name_size)
  allocate(character(len=name_size) :: name )
  name = c_ptr_to_string(name_c_ptr)
  call c_ptr_free(name_c_ptr)
end subroutine

end module fckit_main_module

