#include "fckit/fctest.h"
#define TEST_ABORT 0

subroutine abort_func_0()
  use fckit_module
  type(fckit_mpi_comm) :: mpi
  call fckit_log%info("custom_abort 0",flush=.true.)
  mpi = fckit_mpi_comm("world")
  call mpi%abort()
end subroutine

subroutine abort_func_1(msg)
  use fckit_module
  type(fckit_mpi_comm) :: mpi

  character(len=*), intent(in) :: msg
  character(len=1024) :: string
  write(string,*) "custom abort 1,  msg = ", msg
  call fckit_log%info(string,flush=.true.)
  
  mpi = fckit_mpi_comm("world")
  call mpi%abort()
end subroutine

subroutine abort_func_2(msg,file,line)
  use fckit_module
  type(fckit_mpi_comm) :: mpi

  character(len=*), intent(in) :: msg
  character(len=*), intent(in) :: file
  integer, intent(in) :: line
  
  character(len=1024) :: string
  write(string,*) "custom abort 2,  msg = ", msg, ", file = ",file, ", line = ",line
  call fckit_log%info(string,flush=.true.)
  
  mpi = fckit_mpi_comm("world")
  call mpi%abort()
end subroutine

module fckit_test_abort_fx
public
contains

subroutine abort_wrapper()
  use fckit_module
  if( fckit_exception%location%is_set() ) then 
    call abort_func_2( fckit_exception%what(), fckit_exception%location%file(), fckit_exception%location%line() )
  else
    call abort_func_1( fckit_exception%what() )
  endif
end subroutine

subroutine sig_handler(signum)
  use, intrinsic :: iso_c_binding, only : c_int
  use fckit_module
  integer(c_int), value, intent(in) :: signum
  character(len=1024) :: string
  write(string,*) "signal handler intercepted signal ",signum
  call fckit_log%info(string,flush=.true.)
end subroutine

end module

TESTSUITE( fckit_test_abort )

TESTSUITE_INIT
  use fckit_module
  call fckit_main%init()
END_TESTSUITE_INIT

TESTSUITE_FINALIZE
  use fckit_module
  call fckit_main%final()
END_TESTSUITE_FINALIZE

TEST( test_signal )
  use fckit_module
  use fckit_test_abort_fx
  implicit none
  procedure(fckit_signal_handler), pointer:: signal_handler => sig_handler
#if TEST_ABORT
!   call fckit_signal%restore_handlers()
!   call fckit_signal%raise(fckit_signal%SIGABRT())
#endif
  call fckit_signal%set_handler(fckit_signal%SIGABRT(),signal_handler)
  call fckit_signal%raise(fckit_signal%SIGABRT())
END_TEST

TEST( test_abort_1 )
  use fckit_module
  implicit none
  external :: abort_func_0
  procedure(fckit_exception_handler), pointer:: exception_handler => abort_func_0
  call fckit_exception%set_handler( exception_handler )
#if TEST_ABORT  
!   call fckit_exception%abort()
#endif
END_TEST

TEST( test_abort_2 )
  use fckit_module
  use fckit_test_abort_fx
  implicit none
  procedure(fckit_exception_handler), pointer:: exception_handler => abort_wrapper
  call fckit_exception%set_handler( exception_handler )
# if TEST_ABORT  
!   call fckit_exception%abort("test_abort_2")
#endif
END_TEST

TEST( test_abort_3 )
  use fckit_module
  use fckit_test_abort_fx
  implicit none
  procedure(fckit_exception_handler), pointer:: exception_handler => abort_wrapper
  call fckit_exception%set_handler( exception_handler )
#if TEST_ABORT  
!   call fckit_exception%abort("test_abort_3","test_abort.F90",__LINE__)
#endif
END_TEST

TEST( test_throw )
  use fckit_module
  implicit none
#if TEST_ABORT  
!   call fckit_exception%throw("Exception: test throw","test_abort.F90",__LINE__)
#endif
END_TEST

TEST( test_interrupt )
  use fckit_module
  implicit none
#if TEST_ABORT
  call fckit_log%info("Please write CTRL+C to interrupt",flush=.true.)
  call sleep(5)
#endif
END_TEST

TEST( test_summary )
write(0,*) "Change inside this file (test_abort.F90) the definition TEST_ABORT to 1, and expect tests to fail"
END_TEST

END_TESTSUITE
