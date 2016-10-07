
module fckit_runtime_module
implicit none
private

public :: main

type :: fckit_main
contains
  procedure, nopass, public :: ready
  procedure, nopass, public :: init
  procedure, nopass, public :: output_task
end type

type(fckit_main), save :: main

!------------------------------------------------------------------------------
interface
  !int fckit__runtime_main_init (int argc, char* argv[])
  function fckit__runtime_main_init(argc,argv,task,output_task,output_unit,error_unit, output_simple) &
    & result(error_code) bind(c,name="fckit__runtime_main_init")
    use iso_c_binding, only: c_int, c_ptr
    integer(c_int) :: error_code
    integer(c_int), value :: argc
    type(c_ptr), dimension(*) :: argv
    integer(c_int), value :: task
    integer(c_int), value :: output_task
    integer(c_int), value :: output_unit
    integer(c_int), value :: error_unit
    integer(c_int), value :: output_simple
  end function

  !int fckit__runtime_main_ready (int& ready)
  function fckit__runtime_main_ready(ready) result(error_code) bind(c,name="fckit__runtime_main_ready")
    use iso_c_binding, only: c_int
    integer(c_int) :: error_code
    integer(c_int) :: ready
  end function
  !int fckit__runtime_main_output_task (int& output_task)
  function fckit__runtime_main_output_task(output_task) result(error_code) bind(c,name="fckit__runtime_main_output_task")
    use iso_c_binding, only: c_int
    integer(c_int) :: error_code
    integer(c_int) :: output_task
  end function

end interface
!------------------------------------------------------------------------------

! =============================================================================
CONTAINS
! =============================================================================

subroutine init(task,output_task,output_unit,error_unit,output_simple)
  use, intrinsic :: iso_c_binding, only : c_ptr, c_int
  use fckit_c_interop_module
  integer, save :: argc
  type(c_ptr), save :: argv(15)
  integer(c_int):: error_code
  integer(c_int), optional, intent(in) :: task
  integer(c_int), optional, intent(in) :: output_task
  integer(c_int), optional, intent(in) :: output_unit
  integer(c_int), optional, intent(in) :: error_unit
  logical, optional, intent(in) :: output_simple

  integer(c_int) :: opt_task, opt_output_task, opt_output_unit, opt_error_unit, opt_output_simple
  integer(c_int), parameter :: MPI_COMM_WORLD_RANK = -1
  integer(c_int), parameter :: COUT = -1
  integer(c_int), parameter :: CERR = -1
  integer(c_int), parameter :: ALL_TASKS = -1
  opt_task = MPI_COMM_WORLD_RANK
  opt_output_task = ALL_TASKS
  opt_output_unit = COUT
  opt_error_unit  = CERR
  opt_output_simple  = 0
  call get_c_commandline_arguments(argc,argv)
  if( present( task        ) ) opt_task        = task
  if( present( output_task ) ) opt_output_task = output_task
  if( present( output_unit ) ) opt_output_unit = output_unit
  if( present( error_unit  ) ) opt_error_unit  = error_unit
  if( present( output_simple ) ) then
    if( output_simple ) opt_output_simple = 1
  endif

  error_code = fckit__runtime_main_init(argc,argv,opt_task,opt_output_task,opt_output_unit,opt_error_unit,opt_output_simple)
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


function output_task()
  use, intrinsic :: iso_c_binding, only : c_int
  use fckit_c_interop_module
  logical :: ready
  integer(c_int) :: output_task
  integer:: error_code
  error_code = fckit__runtime_main_output_task(output_task)
end function


end module fckit_runtime_module

