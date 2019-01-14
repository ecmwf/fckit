! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fctest.h"

TESTSUITE( test_mpi )

TESTSUITE_INIT
  use fckit_module
  call fckit_main%init()
END_TESTSUITE_INIT

TESTSUITE_FINALIZE
  use fckit_module
  call fckit_main%final()
END_TESTSUITE_FINALIZE

TEST( test_comm )
  use fckit_module
  use, intrinsic :: iso_c_binding
  implicit none
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
  use fckit_module
  use, intrinsic :: iso_c_binding
  implicit none
  integer :: fcomm_self
  type(fckit_mpi_comm) :: comm1, comm2

  comm1 = fckit_mpi_comm("self")
  fcomm_self = comm1%communicator()

  comm2 = fckit_mpi_comm(fcomm_self)

  FCTEST_CHECK_EQUAL( comm2%size(), 1 )
  FCTEST_CHECK_EQUAL( comm2%rank(), 0 )

END_TEST



TEST( test_set_comm_default )
  use fckit_mpi_module
  use, intrinsic :: iso_c_binding
  implicit none
  type(fckit_mpi_comm) :: comm

  write(0,*) "test_set_comm_default"
  call fckit_mpi_setCommDefault("self")
  comm = fckit_mpi_comm()
  FCTEST_CHECK_EQUAL( comm%size(), 1 )
  FCTEST_CHECK_EQUAL( comm%rank(), 0 )

END_TEST

TEST( test_uninitialised )
  use fckit_module
  use, intrinsic :: iso_c_binding
  type(fckit_mpi_comm) :: comm

  write(0,*) "test_uninitialised"
  FCTEST_CHECK_EQUAL( comm%size(), 1 )
  FCTEST_CHECK_EQUAL( comm%rank(), 0 )

END_TEST

TEST( test_allreduce )
  use fckit_mpi_module
  use, intrinsic :: iso_c_binding
  implicit none
  type(fckit_mpi_comm) :: comm
  real(c_double)  :: real64, res_real64, real64_r1(2),      res_real64_r1(2)
  real(c_float)   :: real32, res_real32, real32_r2(3,2),    res_real32_r2(3,2)
  integer(c_int32_t) :: int32,  res_int32,  int32_r3(4,3,2),   res_int32_r3(4,3,2), j
  integer(c_long) :: int64,  res_int64,  int64_r4(4,3,2,2), res_int64_r4(4,3,2,2), check_prod, check_sum

  FCKIT_SUPPRESS_UNUSED( real64_r1 )
  FCKIT_SUPPRESS_UNUSED( res_real64_r1 )
  FCKIT_SUPPRESS_UNUSED( res_int32_r3 )
  FCKIT_SUPPRESS_UNUSED( res_real32_r2 )
  FCKIT_SUPPRESS_UNUSED( real32_r2 )

  write(0,*) "test_allreduce"
  comm = fckit_mpi_comm("world")

  real64 = 2
  call comm%allreduce(real64,res_real64,fckit_mpi_sum())
  FCTEST_CHECK_EQUAL( int(res_real64), 2*comm%size() )
  real64 = comm%rank()+1
  call comm%allreduce(real64,res_real64,fckit_mpi_max())
  FCTEST_CHECK_EQUAL( int(res_real64), comm%size())
  call comm%allreduce(real64,res_real64,fckit_mpi_min())
  FCTEST_CHECK_EQUAL( int(res_real64), 1 )

  check_prod = 1
  check_sum = 0
  do j=1,comm%size()
    check_prod = check_prod * j
    check_sum  = check_sum + j
  enddo
  call comm%allreduce(real64,res_real64,fckit_mpi_prod())
  FCTEST_CHECK_EQUAL( int(res_real64), int(check_prod) )

  real32 = 3
  call comm%allreduce(real32,res_real32,fckit_mpi_sum())
  FCTEST_CHECK_EQUAL( int(res_real32), 3*comm%size() )

  int32 = 4
  call comm%allreduce(int32,res_int32,fckit_mpi_sum())
  FCTEST_CHECK_EQUAL( int(res_int32), 4*comm%size() )

  int64 = 5
  call comm%allreduce(int64,res_int64,fckit_mpi_sum())
  FCTEST_CHECK_EQUAL( int(res_int64), 5*comm%size() )


  int64_r4(1,1,1,1) = 2
  int64_r4(2,3,1,2) = comm%rank()+1
  int64_r4(3,1,2,1) = comm%size()

  call comm%allreduce(int64_r4,res_int64_r4,fckit_mpi_prod())
  FCTEST_CHECK_EQUAL(res_int64_r4(2,3,1,2),check_prod)

  call comm%allreduce(int64_r4,res_int64_r4,fckit_mpi_sum())
  FCTEST_CHECK_EQUAL(res_int64_r4(2,3,1,2),check_sum)
  FCTEST_CHECK_EQUAL(res_int64_r4(3,1,2,1),int(comm%size()*comm%size(),c_long))
  FCTEST_CHECK_EQUAL(res_int64_r4(1,1,1,1),int(comm%size()*2,c_long))

  call comm%allreduce(int64_r4,res_int64_r4,fckit_mpi_max())
  FCTEST_CHECK_EQUAL(res_int64_r4(2,3,1,2),int(comm%size(),c_long))
  FCTEST_CHECK_EQUAL(res_int64_r4(3,1,2,1),int(comm%size(),c_long))
  FCTEST_CHECK_EQUAL(res_int64_r4(1,1,1,1),int(2,c_long))

  call comm%allreduce(int64_r4,res_int64_r4,fckit_mpi_min())
  FCTEST_CHECK_EQUAL(res_int64_r4(2,3,1,2),int(1,c_long))
  FCTEST_CHECK_EQUAL(res_int64_r4(3,1,2,1),int(comm%size(),c_long))
  FCTEST_CHECK_EQUAL(res_int64_r4(1,1,1,1),int(2,c_long))

END_TEST

TEST( test_allreduce_inplace )
  use fckit_mpi_module
  use, intrinsic :: iso_c_binding
  implicit none
  type(fckit_mpi_comm) :: comm
  real(c_double)  :: real64, real64_r1(2)
  real(c_float)   :: real32, real32_r2(3,2)
  integer(c_int32_t) :: int32,  int32_r3(4,3,2), j
  integer(c_long) :: int64,  int64_r4(4,3,2,2), check_prod, check_sum

  FCKIT_SUPPRESS_UNUSED( real64_r1 )
  FCKIT_SUPPRESS_UNUSED( int32_r3 )
  FCKIT_SUPPRESS_UNUSED( real32_r2 )

  write(0,*) "test_allreduce_inplace"
  comm = fckit_mpi_comm("world")

  real64 = 2
  call comm%allreduce(real64,fckit_mpi_sum())
  FCTEST_CHECK_EQUAL( int(real64), 2*comm%size() )
  real64 = comm%rank()+1
  call comm%allreduce(real64,fckit_mpi_max())
  FCTEST_CHECK_EQUAL( int(real64), comm%size())
  real64 = comm%rank()+1
  call comm%allreduce(real64,fckit_mpi_min())
  FCTEST_CHECK_EQUAL( int(real64), 1 )

  check_prod = 1
  check_sum = 0
  do j=1,comm%size()
    check_prod = check_prod * j
    check_sum  = check_sum + j
  enddo
  real64 = comm%rank()+1
  call comm%allreduce(real64,fckit_mpi_prod())
  FCTEST_CHECK_EQUAL( int(real64), int(check_prod) )

  real32 = 3
  call comm%allreduce(real32,fckit_mpi_sum())
  FCTEST_CHECK_EQUAL( int(real32), 3*comm%size() )

  int32 = 4
  call comm%allreduce(int32,fckit_mpi_sum())
  FCTEST_CHECK_EQUAL( int(int32), 4*comm%size() )

  int64 = 5
  call comm%allreduce(int64,fckit_mpi_sum())
  FCTEST_CHECK_EQUAL( int(int64), 5*comm%size() )


  int64_r4(1,1,1,1) = 2
  int64_r4(2,3,1,2) = comm%rank()+1
  int64_r4(3,1,2,1) = comm%size()

  call comm%allreduce(int64_r4,fckit_mpi_prod())
  FCTEST_CHECK_EQUAL(int64_r4(2,3,1,2),check_prod)

  int64_r4(1,1,1,1) = 2
  int64_r4(2,3,1,2) = comm%rank()+1
  int64_r4(3,1,2,1) = comm%size()

  call comm%allreduce(int64_r4,fckit_mpi_sum())
  FCTEST_CHECK_EQUAL(int64_r4(2,3,1,2),check_sum)
  FCTEST_CHECK_EQUAL(int64_r4(3,1,2,1),int(comm%size()*comm%size(),c_long))
  FCTEST_CHECK_EQUAL(int64_r4(1,1,1,1),int(comm%size()*2,c_long))

  int64_r4(1,1,1,1) = 2
  int64_r4(2,3,1,2) = comm%rank()+1
  int64_r4(3,1,2,1) = comm%size()

  call comm%allreduce(int64_r4,fckit_mpi_max())
  FCTEST_CHECK_EQUAL(int64_r4(2,3,1,2),int(comm%size(),c_long))
  FCTEST_CHECK_EQUAL(int64_r4(3,1,2,1),int(comm%size(),c_long))
  FCTEST_CHECK_EQUAL(int64_r4(1,1,1,1),int(2,c_long))

  int64_r4(1,1,1,1) = 2
  int64_r4(2,3,1,2) = comm%rank()+1
  int64_r4(3,1,2,1) = comm%size()

  call comm%allreduce(int64_r4,fckit_mpi_min())
  FCTEST_CHECK_EQUAL(int64_r4(2,3,1,2),int(1,c_long))
  FCTEST_CHECK_EQUAL(int64_r4(3,1,2,1),int(comm%size(),c_long))
  FCTEST_CHECK_EQUAL(int64_r4(1,1,1,1),int(2,c_long))

END_TEST

TEST( test_allgather )
  use fckit_module
  use, intrinsic ::iso_c_binding
  implicit none
  type(fckit_mpi_comm) :: comm
  real(c_double)  :: real64, real64_r1(2)
  real(c_double), allocatable :: res_real64(:), res_real64_r1(:)
  real(c_double), allocatable :: real64v_r1(:), res_real64v_r1(:)
  real(c_float)   :: real32, real32_r1(2)
  real(c_float), allocatable :: res_real32(:), res_real32_r1(:)
  real(c_float), allocatable :: real32v_r1(:), res_real32v_r1(:)
  integer(c_int32_t) :: int32,  int32_r1(2), j
  integer(c_int32_t), allocatable :: res_int32(:),  res_int32_r1(:)
  integer(c_int32_t), allocatable :: int32v_r1(:),  res_int32v_r1(:)
  integer(c_long) :: int64,  int64_r1(2)
  integer(c_long), allocatable :: res_int64(:),  res_int64_r1(:)
  integer(c_long), allocatable :: int64v_r1(:),  res_int64v_r1(:)
  integer, allocatable :: recvcounts(:), displs(:)

  write(0,*) "test_allgather"
  comm = fckit_mpi_comm("world")

  int32 = 2
  allocate(res_int32(comm%size()))
  call comm%allgather(int32,res_int32)
  FCTEST_CHECK_EQUAL(minval(res_int32),int32)
  FCTEST_CHECK_EQUAL(maxval(res_int32),int32)
  deallocate(res_int32)

  int64 = 3
  allocate(res_int64(comm%size()))
  call comm%allgather(int64,res_int64)
  FCTEST_CHECK_EQUAL(minval(res_int64),int64)
  FCTEST_CHECK_EQUAL(maxval(res_int64),int64)
  deallocate(res_int64)

  real32 = 4
  allocate(res_real32(comm%size()))
  call comm%allgather(real32,res_real32)
  FCTEST_CHECK_EQUAL(minval(res_real32),real32)
  FCTEST_CHECK_EQUAL(maxval(res_real32),real32)
  deallocate(res_real32)

  real64 = 5
  allocate(res_real64(comm%size()))
  call comm%allgather(real64,res_real64)
  FCTEST_CHECK_EQUAL(minval(res_real64),real64)
  FCTEST_CHECK_EQUAL(maxval(res_real64),real64)
  deallocate(res_real64)

  int32_r1 = (/ 2,3 /)
  allocate(res_int32_r1(size(int32_r1)*comm%size()))
  call comm%allgather(int32_r1,res_int32_r1,size(int32_r1))
  FCTEST_CHECK_EQUAL(minval(res_int32_r1),minval(int32_r1))
  FCTEST_CHECK_EQUAL(maxval(res_int32_r1),maxval(int32_r1))
  deallocate(res_int32_r1)

  int64_r1 = (/ 4,5 /)
  allocate(res_int64_r1(size(int64_r1)*comm%size()))
  call comm%allgather(int64_r1,res_int64_r1,size(int64_r1))
  FCTEST_CHECK_EQUAL(minval(res_int64_r1),minval(int64_r1))
  FCTEST_CHECK_EQUAL(maxval(res_int64_r1),maxval(int64_r1))
  deallocate(res_int64_r1)

  real32_r1 = (/ 6,7 /)
  allocate(res_real32_r1(size(real32_r1)*comm%size()))
  call comm%allgather(real32_r1,res_real32_r1,size(real32_r1))
  FCTEST_CHECK_EQUAL(minval(res_real32_r1),minval(real32_r1))
  FCTEST_CHECK_EQUAL(maxval(res_real32_r1),maxval(real32_r1))
  deallocate(res_real32_r1)

  real64_r1 = (/ 8,9 /)
  allocate(res_real64_r1(size(real64_r1)*comm%size()))
  call comm%allgather(real64_r1,res_real64_r1,size(real64_r1))
  FCTEST_CHECK_EQUAL(minval(res_real64_r1),minval(real64_r1))
  FCTEST_CHECK_EQUAL(maxval(res_real64_r1),maxval(real64_r1))
  deallocate(res_real64_r1)

  allocate(recvcounts(comm%size()),displs(comm%size()))

  allocate(int32v_r1(comm%rank()+1))
  int32v_r1 = comm%rank()
  recvcounts(1) = 1
  displs(1) = 0
  do j = 2,comm%size()
     recvcounts(j) = j
     displs(j) = displs(j-1)+recvcounts(j-1)
  enddo
  allocate(res_int32v_r1(sum(recvcounts)))
  call comm%allgather(int32v_r1,res_int32v_r1,size(int32v_r1),recvcounts,displs)
  FCTEST_CHECK_EQUAL(minval(res_int32v_r1),0)
  FCTEST_CHECK_EQUAL(maxval(res_int32v_r1),comm%size()-1)
  deallocate(int32v_r1,res_int32v_r1)

  allocate(int64v_r1(comm%size()-comm%rank()))
  int64v_r1 = comm%rank()
  recvcounts(1) = comm%size()
  displs(1) = 0
  do j = 2,comm%size()
     recvcounts(j) = comm%size() - j + 1
     displs(j) = displs(j-1)+recvcounts(j-1)
  enddo
  allocate(res_int64v_r1(sum(recvcounts)))
  call comm%allgather(int64v_r1,res_int64v_r1,size(int64v_r1),recvcounts,displs)
  FCTEST_CHECK_EQUAL(int(minval(res_int64v_r1)),0)
  FCTEST_CHECK_EQUAL(int(maxval(res_int64v_r1)),comm%size()-1)
  deallocate(int64v_r1,res_int64v_r1)

  allocate(real32v_r1(comm%rank()+1))
  real32v_r1 = comm%rank()
  recvcounts(1) = 1
  displs(1) = 0
  do j = 2,comm%size()
     recvcounts(j) = j
     displs(j) = displs(j-1)+recvcounts(j-1)
  enddo
  allocate(res_real32v_r1(sum(recvcounts)))
  call comm%allgather(real32v_r1,res_real32v_r1,size(real32v_r1),recvcounts,displs)
  FCTEST_CHECK_EQUAL(minval(res_real32v_r1),real(0,c_float))
  FCTEST_CHECK_EQUAL(maxval(res_real32v_r1),real(comm%size()-1,c_float))
  deallocate(real32v_r1,res_real32v_r1)

  allocate(real64v_r1(comm%size()-comm%rank()))
  real64v_r1 = comm%rank()
  recvcounts(1) = comm%size()
  displs(1) = 0
  do j = 2,comm%size()
     recvcounts(j) = comm%size() - j + 1
     displs(j) = displs(j-1)+recvcounts(j-1)
  enddo
  allocate(res_real64v_r1(sum(recvcounts)))
  call comm%allgather(real64v_r1,res_real64v_r1,size(real64v_r1),recvcounts,displs)
  FCTEST_CHECK_EQUAL(minval(res_real64v_r1),real(0,c_double))
  FCTEST_CHECK_EQUAL(maxval(res_real64v_r1),real(comm%size()-1,c_double))
  deallocate(real64v_r1,res_real64v_r1)

  deallocate(recvcounts,displs)

END_TEST

TEST( test_broadcast )
  use fckit_mpi_module
  use, intrinsic :: iso_c_binding
  implicit none
  type(fckit_mpi_comm) :: comm
  real(c_double)  :: real64, real64_r1(2)
  real(c_float)   :: real32, real32_r2(3,2)
  integer(c_int32_t) :: int32,  int32_r3(4,3,2)
  integer(c_long) :: int64,  int64_r4(4,3,2,2)
  logical :: logical_r1(4)
  character(len=30) :: string_r0

  FCKIT_SUPPRESS_UNUSED( real64_r1 )
  FCKIT_SUPPRESS_UNUSED( int64 )
  FCKIT_SUPPRESS_UNUSED( real32_r2 )

  write(0,*) "test_broadcast"
  comm = fckit_mpi_comm("world")
  if(comm%rank()==0) real64 = 0.1_c_double
  call comm%broadcast(real64,root=0)
  FCTEST_CHECK_CLOSE(real64, 0.1_c_double,1.e-9_c_double)

  if(comm%rank()==0) real32 = 0.2_c_float
  call comm%broadcast(real32,root=0)
  FCTEST_CHECK_CLOSE(real32, 0.2_c_float,1.e-5_c_float)

  if(comm%rank()==comm%size()-1) int32 = 3
  call comm%broadcast(int32,root=comm%size()-1)
  FCTEST_CHECK_EQUAL(int32, 3)

  if(comm%rank()==0) int64_r4(2,2,1,2) = 1_c_long
  call comm%broadcast(int64_r4,root=0)
  FCTEST_CHECK_EQUAL(int64_r4(2,2,1,2), 1_c_long)

  if(comm%rank()==0) int32_r3(1,3,2) = 2
  call comm%broadcast(int32_r3,root=0)
  FCTEST_CHECK_EQUAL(int32_r3(1,3,2), 2_c_int32_t)

  if(comm%rank()==comm%size()-1) int32_r3(2,1,1) = 3
  call comm%broadcast(int32_r3,root=comm%size()-1)
  FCTEST_CHECK_EQUAL(int32_r3(2,1,1), 3)

  if(comm%rank()==comm%size()-1) logical_r1(2) = .true.
  call comm%broadcast(logical_r1,root=comm%size()-1)
  FCTEST_CHECK_EQUAL(logical_r1(2), .true.)

  if(comm%rank()==comm%size()-1) string_r0 = "path/filename"
  call comm%broadcast(string_r0,root=comm%size()-1)
  FCTEST_CHECK_EQUAL(string_r0, "path/filename")

END_TEST


TEST( test_nonblocking_send_receive )
  use fckit_mpi_module
  use, intrinsic :: iso_c_binding
  implicit none
  type(fckit_mpi_comm) :: comm
  integer :: sendreq, recvreq
  type(fckit_mpi_status) :: status
  integer :: tag=1
  real(c_double)  :: send_real64, recv_real64

  write(0,*) "test_nonblocking_send_receive"
  comm = fckit_mpi_comm("world")

  send_real64 = 0._c_double


  if( comm%rank()==comm%size()-1) then

    recvreq = comm%ireceive(recv_real64,0,tag)

    write(0,*) "receive-request:",recvreq

  endif

  if(comm%rank()==0) then

    send_real64 = 0.1_c_double
    sendreq = comm%isend(send_real64,comm%size()-1,tag)

    write(0,*) "send-request:",sendreq

  endif


  if( comm%rank()==comm%size()-1) then
    call comm%wait(recvreq,status)
    FCTEST_CHECK_CLOSE(recv_real64, 0.1_c_double,1.e-9_c_double)
  endif

  if(comm%rank()==0) then
    call comm%wait(sendreq,status)
  endif

!   FCTEST_CHECK_EQUAL(status%source(), 0)
!   FCTEST_CHECK_EQUAL(status%tag(), tag)
!   FCTEST_CHECK_EQUAL(status%error(), 0)

END_TEST

TEST( test_blocking_send_receive )
  use fckit_mpi_module
  use, intrinsic :: iso_c_binding
  implicit none
  type(fckit_mpi_comm) :: comm
  type(fckit_mpi_status) :: status
  integer :: tag=99
  real(c_double)  :: send_real64, recv_real64

  write(0,*) "test_blocking_send_receive"
  comm = fckit_mpi_comm("world")

  send_real64 = 0._c_double

  if(comm%rank()==0) then

    send_real64 = 0.1_c_double
    call comm%send(send_real64,comm%size()-1,tag)

    send_real64 = 0.2_c_double
    call comm%send(send_real64,comm%size()-1,tag+1)

  endif
  if( comm%rank()==comm%size()-1) then

    call comm%receive(recv_real64,0,tag,status)
    FCTEST_CHECK_CLOSE(recv_real64, 0.1_c_double,1.e-9_c_double)
    FCTEST_CHECK_EQUAL(status%source(), 0)
    FCTEST_CHECK_EQUAL(status%tag(), tag)
    FCTEST_CHECK_EQUAL(status%error(), 0)

    call comm%receive(recv_real64,0,tag=comm%anytag(),status=status)
    FCTEST_CHECK_EQUAL(status%tag(), tag+1)
    FCTEST_CHECK_CLOSE(recv_real64, 0.2_c_double,1.e-9_c_double)

  endif

END_TEST


END_TESTSUITE
