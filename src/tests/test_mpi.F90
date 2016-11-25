#include "fckit/fctest.h"

TESTSUITE( test_mpi )

TESTSUITE_FINALIZE
  use fckit_main_module
  call main%final()
END_TESTSUITE_FINALIZE

TEST( test_comm )
  use fckit_mpi_module
  use, intrinsic :: iso_c_binding

  type(fckit_mpi_comm) :: comm

  write(0,*) "test_comm"

  comm = fckit_mpi_comm()
  write(0,*) "default size:", comm%size()
  write(0,*) "default rank:", comm%rank()

  comm = fckit_mpi_comm("world")
  write(0,*) "world size:", comm%size()
  write(0,*) "default world:", comm%rank()

  comm = fckit_mpi_comm("self")
  FCTEST_CHECK_EQUAL( comm%size(), 1 )
  FCTEST_CHECK_EQUAL( comm%rank(), 0 )

END_TEST


TEST( test_add_comm )
  use fckit_mpi_module
  use, intrinsic :: iso_c_binding

  integer, parameter :: MPI_COMM_SELF=1
  type(fckit_mpi_comm) :: comm

  ! Intel's MPI does not define MPI_COMM_SELF equal to 1
  !write(0,*) "test_add_comm", MPI_COMM_SELF
  !comm = fckit_mpi_comm(MPI_COMM_SELF)
  !
  !FCTEST_CHECK_EQUAL( comm%size(), 1 )
  !FCTEST_CHECK_EQUAL( comm%rank(), 0 )

END_TEST



TEST( test_set_comm_default )
  use fckit_mpi_module
  use, intrinsic :: iso_c_binding
  type(fckit_mpi_comm) :: comm

  write(0,*) "test_set_comm_default"
  call fckit_mpi_setCommDefault("self")
  comm = fckit_mpi_comm()
  FCTEST_CHECK_EQUAL( comm%size(), 1 )
  FCTEST_CHECK_EQUAL( comm%rank(), 0 )

END_TEST

END_TESTSUITE
