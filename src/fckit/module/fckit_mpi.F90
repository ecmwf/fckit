! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fckit.h"

module fckit_mpi_module
  !! Wrap eckit MPI capabilities.
  !!
  !! Depending on use of mpirun, aprun, srun, etc, a serial or true MPI
  !! implementation is used, so that serial jobs do not require an alternative
  !! library linked such as ```mpi_serial```

use fckit_object_module, only: fckit_object
use fckit_buffer_module, only: fckit_buffer

implicit none
private

private :: fckit_object
private :: fckit_buffer

!========================================================================
! Public interface

public :: fckit_mpi_comm
public :: fckit_mpi_status
public :: fckit_mpi_setCommDefault
public :: fckit_mpi_sum
public :: fckit_mpi_prod
public :: fckit_mpi_min
public :: fckit_mpi_max
public :: fckit_mpi_minloc
public :: fckit_mpi_maxloc

!========================================================================

interface fckit_mpi_setCommDefault
  module procedure fckit_mpi_setCommDefault_int
  module procedure fckit_mpi_setCommDefault_name
end interface

!========================================================================

type, extends(fckit_object) :: fckit_mpi_comm
  !! MPI communicator object
  !!
  !! This object forms the basis for communications within one
  !! MPI communicator
  !!
  !! To use the default communicator, use:
  !!
  !!```fortran
  !!type(fckit_mpi_comm) :: comm
  !!comm = fckit_mpi_comm()
  !!```
  !!
  !! Special constructors can be used such as:
  !!
  !!```fortran
  !!type(fckit_mpi_comm) :: comm
  !!comm = fckit_mpi_comm("world")
  !!```
  !!
  !!```fortran
  !!type(fckit_mpi_comm) :: comm
  !!comm = fckit_mpi_comm("self")
  !!```
  !!
  !! Existing fortran MPI communicators can be wrapped:
  !!
  !!```fortran
  !!integer, intent(in) :: MPL_COMM
  !!type(fckit_mpi_comm) :: comm
  !!comm = fckit_mpi_comm(MPL_COMM)
  !!```
  !!
  !! The default constructor can be changed with the routine
  !! [[fckit_mpi_module:fckit_mpi_setCommDefault(subroutine)]]

contains
  procedure, public :: final => fckit_mpi_comm__final
  procedure, public :: delete

  procedure, public :: communicator
    !! Fortran MPI communicator handle

  procedure, public :: size
    !! Number of MPI tasks participating in this communicator

  procedure, public :: rank
    !! Rank of this MPI task in this communicator

  procedure, public :: barrier
    !! MPI Barrier in this communicator

  procedure, public :: abort
    !! MPI Abort

  procedure, public :: anytag
    !! anytag

  procedure, public :: anysource
    !! anysource

  procedure, private :: allreduce_int32_r0
  procedure, private :: allreduce_int32_r1
  procedure, private :: allreduce_int32_r2
  procedure, private :: allreduce_int32_r3
  procedure, private :: allreduce_int32_r4
  procedure, private :: allreduce_int64_r0
  procedure, private :: allreduce_int64_r1
  procedure, private :: allreduce_int64_r2
  procedure, private :: allreduce_int64_r3
  procedure, private :: allreduce_int64_r4
  procedure, private :: allreduce_real32_r0
  procedure, private :: allreduce_real32_r1
  procedure, private :: allreduce_real32_r2
  procedure, private :: allreduce_real32_r3
  procedure, private :: allreduce_real32_r4
  procedure, private :: allreduce_real64_r0
  procedure, private :: allreduce_real64_r1
  procedure, private :: allreduce_real64_r2
  procedure, private :: allreduce_real64_r3
  procedure, private :: allreduce_real64_r4
  procedure, private :: allreduce_inplace_int32_r0
  procedure, private :: allreduce_inplace_int32_r1
  procedure, private :: allreduce_inplace_int32_r2
  procedure, private :: allreduce_inplace_int32_r3
  procedure, private :: allreduce_inplace_int32_r4
  procedure, private :: allreduce_inplace_int64_r0
  procedure, private :: allreduce_inplace_int64_r1
  procedure, private :: allreduce_inplace_int64_r2
  procedure, private :: allreduce_inplace_int64_r3
  procedure, private :: allreduce_inplace_int64_r4
  procedure, private :: allreduce_inplace_real32_r0
  procedure, private :: allreduce_inplace_real32_r1
  procedure, private :: allreduce_inplace_real32_r2
  procedure, private :: allreduce_inplace_real32_r3
  procedure, private :: allreduce_inplace_real32_r4
  procedure, private :: allreduce_inplace_real64_r0
  procedure, private :: allreduce_inplace_real64_r1
  procedure, private :: allreduce_inplace_real64_r2
  procedure, private :: allreduce_inplace_real64_r3
  procedure, private :: allreduce_inplace_real64_r4
  procedure, private :: allgather_int32_r0
  procedure, private :: allgather_int64_r0
  procedure, private :: allgather_real32_r0
  procedure, private :: allgather_real64_r0
  procedure, private :: allgather_int32_r1
  procedure, private :: allgather_int64_r1
  procedure, private :: allgather_real32_r1
  procedure, private :: allgather_real64_r1
  procedure, private :: allgather_int32_r2
  procedure, private :: allgather_int64_r2
  procedure, private :: allgather_real32_r2
  procedure, private :: allgather_real64_r2
  procedure, private :: allgather_int32_r3
  procedure, private :: allgather_int64_r3
  procedure, private :: allgather_real32_r3
  procedure, private :: allgather_real64_r3
  procedure, private :: allgather_int32_r4
  procedure, private :: allgather_int64_r4
  procedure, private :: allgather_real32_r4
  procedure, private :: allgather_real64_r4
  procedure, private :: allgatherv_int32_r1
  procedure, private :: allgatherv_int64_r1
  procedure, private :: allgatherv_real32_r1
  procedure, private :: allgatherv_real64_r1
  procedure, private :: allgatherv_int32_r2
  procedure, private :: allgatherv_int64_r2
  procedure, private :: allgatherv_real32_r2
  procedure, private :: allgatherv_real64_r2
  procedure, private :: allgatherv_int32_r3
  procedure, private :: allgatherv_int64_r3
  procedure, private :: allgatherv_real32_r3
  procedure, private :: allgatherv_real64_r3
  procedure, private :: allgatherv_int32_r4
  procedure, private :: allgatherv_int64_r4
  procedure, private :: allgatherv_real32_r4
  procedure, private :: allgatherv_real64_r4
  procedure, private :: broadcast_int32_r0
  procedure, private :: broadcast_int32_r1
  procedure, private :: broadcast_int32_r2
  procedure, private :: broadcast_int32_r3
  procedure, private :: broadcast_int32_r4
  procedure, private :: broadcast_int64_r0
  procedure, private :: broadcast_int64_r1
  procedure, private :: broadcast_int64_r2
  procedure, private :: broadcast_int64_r3
  procedure, private :: broadcast_int64_r4
  procedure, private :: broadcast_real32_r0
  procedure, private :: broadcast_real32_r1
  procedure, private :: broadcast_real32_r2
  procedure, private :: broadcast_real32_r3
  procedure, private :: broadcast_real32_r4
  procedure, private :: broadcast_real64_r0
  procedure, private :: broadcast_real64_r1
  procedure, private :: broadcast_real64_r2
  procedure, private :: broadcast_real64_r3
  procedure, private :: broadcast_real64_r4
  procedure, private :: send_int32_r0
  procedure, private :: send_int32_r1
  procedure, private :: send_int32_r2
  procedure, private :: send_int32_r3
  procedure, private :: send_int32_r4
  procedure, private :: send_int64_r0
  procedure, private :: send_int64_r1
  procedure, private :: send_int64_r2
  procedure, private :: send_int64_r3
  procedure, private :: send_int64_r4
  procedure, private :: send_real32_r0
  procedure, private :: send_real32_r1
  procedure, private :: send_real32_r2
  procedure, private :: send_real32_r3
  procedure, private :: send_real32_r4
  procedure, private :: send_real64_r0
  procedure, private :: send_real64_r1
  procedure, private :: send_real64_r2
  procedure, private :: send_real64_r3
  procedure, private :: send_real64_r4
  procedure, private :: send_logical_r0
  procedure, private :: send_logical_r1
  procedure, private :: receive_int32_r0
  procedure, private :: receive_int32_r1
  procedure, private :: receive_int32_r2
  procedure, private :: receive_int32_r3
  procedure, private :: receive_int32_r4
  procedure, private :: receive_int64_r0
  procedure, private :: receive_int64_r1
  procedure, private :: receive_int64_r2
  procedure, private :: receive_int64_r3
  procedure, private :: receive_int64_r4
  procedure, private :: receive_real32_r0
  procedure, private :: receive_real32_r1
  procedure, private :: receive_real32_r2
  procedure, private :: receive_real32_r3
  procedure, private :: receive_real32_r4
  procedure, private :: receive_real64_r0
  procedure, private :: receive_real64_r1
  procedure, private :: receive_real64_r2
  procedure, private :: receive_real64_r3
  procedure, private :: receive_real64_r4
  procedure, private :: receive_logical_r0
  procedure, private :: receive_logical_r1
  procedure, private :: isend_int32_r0
  procedure, private :: isend_int32_r1
  procedure, private :: isend_int32_r2
  procedure, private :: isend_int32_r3
  procedure, private :: isend_int32_r4
  procedure, private :: isend_int64_r0
  procedure, private :: isend_int64_r1
  procedure, private :: isend_int64_r2
  procedure, private :: isend_int64_r3
  procedure, private :: isend_int64_r4
  procedure, private :: isend_real32_r0
  procedure, private :: isend_real32_r1
  procedure, private :: isend_real32_r2
  procedure, private :: isend_real32_r3
  procedure, private :: isend_real32_r4
  procedure, private :: isend_real64_r0
  procedure, private :: isend_real64_r1
  procedure, private :: isend_real64_r2
  procedure, private :: isend_real64_r3
  procedure, private :: isend_real64_r4
  procedure, private :: ireceive_int32_r0
  procedure, private :: ireceive_int32_r1
  procedure, private :: ireceive_int32_r2
  procedure, private :: ireceive_int32_r3
  procedure, private :: ireceive_int32_r4
  procedure, private :: ireceive_int64_r0
  procedure, private :: ireceive_int64_r1
  procedure, private :: ireceive_int64_r2
  procedure, private :: ireceive_int64_r3
  procedure, private :: ireceive_int64_r4
  procedure, private :: ireceive_real32_r0
  procedure, private :: ireceive_real32_r1
  procedure, private :: ireceive_real32_r2
  procedure, private :: ireceive_real32_r3
  procedure, private :: ireceive_real32_r4
  procedure, private :: ireceive_real64_r0
  procedure, private :: ireceive_real64_r1
  procedure, private :: ireceive_real64_r2
  procedure, private :: ireceive_real64_r3
  procedure, private :: ireceive_real64_r4
  procedure, private :: alltoallv_real32_r1
  procedure, private :: alltoallv_real64_r1

  !> MPI allreduce interface for most array and scalar types
  generic, public :: allreduce => &
    & allreduce_int32_r0  ,&
    & allreduce_int32_r1  ,&
    & allreduce_int32_r2  ,&
    & allreduce_int32_r3  ,&
    & allreduce_int32_r4  ,&
    & allreduce_int64_r0  ,&
    & allreduce_int64_r1  ,&
    & allreduce_int64_r2  ,&
    & allreduce_int64_r3  ,&
    & allreduce_int64_r4  ,&
    & allreduce_real32_r0 ,&
    & allreduce_real32_r1 ,&
    & allreduce_real32_r2 ,&
    & allreduce_real32_r3 ,&
    & allreduce_real32_r4 ,&
    & allreduce_real64_r0 ,&
    & allreduce_real64_r1 ,&
    & allreduce_real64_r2 ,&
    & allreduce_real64_r3 ,&
    & allreduce_real64_r4 ,&
    & allreduce_inplace_int32_r0  ,&
    & allreduce_inplace_int32_r1  ,&
    & allreduce_inplace_int32_r2  ,&
    & allreduce_inplace_int32_r3  ,&
    & allreduce_inplace_int32_r4  ,&
    & allreduce_inplace_int64_r0  ,&
    & allreduce_inplace_int64_r1  ,&
    & allreduce_inplace_int64_r2  ,&
    & allreduce_inplace_int64_r3  ,&
    & allreduce_inplace_int64_r4  ,&
    & allreduce_inplace_real32_r0 ,&
    & allreduce_inplace_real32_r1 ,&
    & allreduce_inplace_real32_r2 ,&
    & allreduce_inplace_real32_r3 ,&
    & allreduce_inplace_real32_r4 ,&
    & allreduce_inplace_real64_r0 ,&
    & allreduce_inplace_real64_r1 ,&
    & allreduce_inplace_real64_r2 ,&
    & allreduce_inplace_real64_r3 ,&
    & allreduce_inplace_real64_r4

  !> MPI allgather interface for most array and scalar types
  generic, public :: allgather => &
    & allgather_int32_r0  ,&
    & allgather_int64_r0  ,&
    & allgather_real32_r0  ,&
    & allgather_real64_r0  ,&
    & allgather_int32_r1  ,&
    & allgather_int64_r1  ,&
    & allgather_real32_r1  ,&
    & allgather_real64_r1  ,&
    & allgather_int32_r2  ,&
    & allgather_int64_r2  ,&
    & allgather_real32_r2  ,&
    & allgather_real64_r2  ,&
    & allgather_int32_r3  ,&
    & allgather_int64_r3  ,&
    & allgather_real32_r3  ,&
    & allgather_real64_r3  ,&
    & allgather_int32_r4  ,&
    & allgather_int64_r4  ,&
    & allgather_real32_r4  ,&
    & allgather_real64_r4  ,&
    & allgatherv_int32_r1  ,&
    & allgatherv_int64_r1  ,&
    & allgatherv_real32_r1 ,&
    & allgatherv_real64_r1 ,&
    & allgatherv_int32_r2  ,&
    & allgatherv_int64_r2  ,&
    & allgatherv_real32_r2 ,&
    & allgatherv_real64_r2 ,&
    & allgatherv_int32_r3  ,&
    & allgatherv_int64_r3  ,&
    & allgatherv_real32_r3 ,&
    & allgatherv_real64_r3 ,&
    & allgatherv_int32_r4  ,&
    & allgatherv_int64_r4  ,&
    & allgatherv_real32_r4  ,&
    & allgatherv_real64_r4

  !> MPI broadcast for most array and scalar types
  generic, public :: broadcast => &
    & broadcast_int32_r0  ,&
    & broadcast_int32_r1  ,&
    & broadcast_int32_r2  ,&
    & broadcast_int32_r3  ,&
    & broadcast_int32_r4  ,&
    & broadcast_int64_r0  ,&
    & broadcast_int64_r1  ,&
    & broadcast_int64_r2  ,&
    & broadcast_int64_r3  ,&
    & broadcast_int64_r4  ,&
    & broadcast_real32_r0 ,&
    & broadcast_real32_r1 ,&
    & broadcast_real32_r2 ,&
    & broadcast_real32_r3 ,&
    & broadcast_real32_r4 ,&
    & broadcast_real64_r0 ,&
    & broadcast_real64_r1 ,&
    & broadcast_real64_r2 ,&
    & broadcast_real64_r3 ,&
    & broadcast_real64_r4

  !> MPI broadcast file to buffer
  procedure, public :: broadcast_file

  !> MPI send for most array and scalar types
  generic, public :: send => &
    & send_int32_r0  ,&
    & send_int32_r1  ,&
    & send_int32_r2  ,&
    & send_int32_r3  ,&
    & send_int32_r4  ,&
    & send_int64_r0  ,&
    & send_int64_r1  ,&
    & send_int64_r2  ,&
    & send_int64_r3  ,&
    & send_int64_r4  ,&
    & send_real32_r0 ,&
    & send_real32_r1 ,&
    & send_real32_r2 ,&
    & send_real32_r3 ,&
    & send_real32_r4 ,&
    & send_real64_r0 ,&
    & send_real64_r1 ,&
    & send_real64_r2 ,&
    & send_real64_r3 ,&
    & send_real64_r4 , &
    & send_logical_r0, &
    & send_logical_r1

  !> MPI receive for most array and scalar types
  generic, public :: receive => &
    & receive_int32_r0  ,&
    & receive_int32_r1  ,&
    & receive_int32_r2  ,&
    & receive_int32_r3  ,&
    & receive_int32_r4  ,&
    & receive_int64_r0  ,&
    & receive_int64_r1  ,&
    & receive_int64_r2  ,&
    & receive_int64_r3  ,&
    & receive_int64_r4  ,&
    & receive_real32_r0 ,&
    & receive_real32_r1 ,&
    & receive_real32_r2 ,&
    & receive_real32_r3 ,&
    & receive_real32_r4 ,&
    & receive_real64_r0 ,&
    & receive_real64_r1 ,&
    & receive_real64_r2 ,&
    & receive_real64_r3 ,&
    & receive_real64_r4 ,&
    & receive_logical_r0,&
    & receive_logical_r1

  !> MPI asynchronous send for most array and scalar types
  generic, public :: isend => &
    & isend_int32_r0  ,&
    & isend_int32_r1  ,&
    & isend_int32_r2  ,&
    & isend_int32_r3  ,&
    & isend_int32_r4  ,&
    & isend_int64_r0  ,&
    & isend_int64_r1  ,&
    & isend_int64_r2  ,&
    & isend_int64_r3  ,&
    & isend_int64_r4  ,&
    & isend_real32_r0 ,&
    & isend_real32_r1 ,&
    & isend_real32_r2 ,&
    & isend_real32_r3 ,&
    & isend_real32_r4 ,&
    & isend_real64_r0 ,&
    & isend_real64_r1 ,&
    & isend_real64_r2 ,&
    & isend_real64_r3 ,&
    & isend_real64_r4

  !> MPI asynchronous receive for most array and scalar types
  generic, public :: ireceive => &
    & ireceive_int32_r0  ,&
    & ireceive_int32_r1  ,&
    & ireceive_int32_r2  ,&
    & ireceive_int32_r3  ,&
    & ireceive_int32_r4  ,&
    & ireceive_int64_r0  ,&
    & ireceive_int64_r1  ,&
    & ireceive_int64_r2  ,&
    & ireceive_int64_r3  ,&
    & ireceive_int64_r4  ,&
    & ireceive_real32_r0 ,&
    & ireceive_real32_r1 ,&
    & ireceive_real32_r2 ,&
    & ireceive_real32_r3 ,&
    & ireceive_real32_r4 ,&
    & ireceive_real64_r0 ,&
    & ireceive_real64_r1 ,&
    & ireceive_real64_r2 ,&
    & ireceive_real64_r3 ,&
    & ireceive_real64_r4

  !> MPI allgather interface for most array and scalar types
  generic, public :: alltoallv => &
    & alltoallv_real32_r1  ,&
    & alltoallv_real64_r1

  !> MPI wait for this communicator
  procedure, public :: wait

#if FCKIT_HAVE_FINAL
  final :: fckit_mpi_comm__final_auto
#endif

endtype

interface fckit_mpi_comm
  module procedure comm_constructor
  module procedure comm_wrap
end interface


type fckit_mpi_status
  integer, private :: status(3)
contains
  procedure, public :: source => status_source
  procedure, public :: tag => status_tag
  procedure, public :: error => status_error
end type

!========================================================================

interface

  function fckit__mpi__comm_communicator(comm) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int32_t
    integer(c_int32_t) :: fckit__mpi__comm_communicator
    type(c_ptr), value :: comm
  end function

  ! const fckit::mpi::Comm* fckit__mpi__comm_default()
  function fckit__mpi__comm_default() bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr
    type(c_ptr) :: fckit__mpi__comm_default
  end function

  ! const fckit::mpi::Comm* fckit__mpi__comm(const char* name)
  function fckit__mpi__comm(name) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_char
    type(c_ptr) :: fckit__mpi__comm
    character(kind=c_char), dimension(*) :: name
  end function

  ! const fckit::mpi::Comm* fckit__mpi__comm_wrap(int comm)
  function fckit__mpi__comm_wrap(comm) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int32_t
    type(c_ptr) :: fckit__mpi__comm_wrap
    integer(c_int32_t), value :: comm
  end function

  function fckit__mpi__size(this) bind(c)
    use, intrinsic :: iso_c_binding, only : c_int32_t, c_ptr
    integer(c_int32_t) :: fckit__mpi__size
    type(c_ptr), value :: this
  end function

  function fckit__mpi__rank(this) bind(c)
    use, intrinsic :: iso_c_binding, only : c_int32_t, c_ptr
    integer(c_int32_t) :: fckit__mpi__rank
    type(c_ptr), value :: this
  end function

  subroutine fckit__mpi__barrier(this) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr
    type(c_ptr), value :: this
  end subroutine

  subroutine fckit__mpi__abort(this,error_code) bind(c)
    use, intrinsic :: iso_c_binding, only : c_int32_t, c_ptr
    type(c_ptr), value :: this
    integer(c_int32_t), value :: error_code
  end subroutine

  ! void fckit__mpi__setCommDefault_int(int comm)
  subroutine fckit__mpi__setCommDefault_int(comm) bind(c,name="fckit__mpi__setCommDefault_int")
    use, intrinsic :: iso_c_binding, only : c_int32_t
    integer(c_int32_t), value :: comm
  end subroutine

  ! void fckit__mpi__setCommDefault_name(const char* name)
  subroutine fckit__mpi__setCommDefault_name(name) bind(c,name="fckit__mpi__setCommDefault_name")
    use, intrinsic :: iso_c_binding, only : c_char
    character(kind=c_char), dimension(*) :: name
  end subroutine

  function fckit__mpi__sum() bind(c)
    use, intrinsic :: iso_c_binding, only : c_int32_t
    integer(c_int32_t) :: fckit__mpi__sum
  end function

  function fckit__mpi__prod() bind(c)
    use, intrinsic :: iso_c_binding, only : c_int32_t
    integer(c_int32_t) :: fckit__mpi__prod
  end function

  function fckit__mpi__min() bind(c)
    use, intrinsic :: iso_c_binding, only : c_int32_t
    integer(c_int32_t) :: fckit__mpi__min
  end function

  function fckit__mpi__max() bind(c)
    use, intrinsic :: iso_c_binding, only : c_int32_t
    integer(c_int32_t) :: fckit__mpi__max
  end function

  function fckit__mpi__minloc() bind(c)
    use, intrinsic :: iso_c_binding, only : c_int32_t
    integer(c_int32_t) :: fckit__mpi__minloc
  end function

  function fckit__mpi__maxloc() bind(c)
    use, intrinsic :: iso_c_binding, only : c_int32_t
    integer(c_int32_t) :: fckit__mpi__maxloc
  end function

  subroutine fckit__mpi__allreduce_int32(comm,in,out,count,operation) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t, c_size_t
    type(c_ptr), value :: comm
    integer(c_int32_t), dimension(*) :: in
    integer(c_int32_t), dimension(*) :: out
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: operation
  end subroutine

  subroutine fckit__mpi__allreduce_int64(comm,in,out,count,operation) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t, c_int64_t, c_size_t
    type(c_ptr), value :: comm
    integer(c_int64_t), dimension(*) :: in
    integer(c_int64_t), dimension(*) :: out
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: operation
  end subroutine

  subroutine fckit__mpi__allreduce_real32(comm,in,out,count,operation) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t, c_float, c_size_t
    type(c_ptr), value :: comm
    real(c_float), dimension(*) :: in
    real(c_float), dimension(*) :: out
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: operation
  end subroutine

  subroutine fckit__mpi__allreduce_real64(comm,in,out,count,operation) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t, c_double, c_size_t
    type(c_ptr), value :: comm
    real(c_double), dimension(*) :: in
    real(c_double), dimension(*) :: out
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: operation
  end subroutine

  subroutine fckit__mpi__allreduce_inplace_int32(comm,inout,count,operation) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t, c_size_t
    type(c_ptr), value :: comm
    integer(c_int32_t), dimension(*) :: inout
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: operation
  end subroutine

  subroutine fckit__mpi__allreduce_inplace_int64(comm,inout,count,operation) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t, c_int64_t, c_size_t
    type(c_ptr), value :: comm
    integer(c_int64_t), dimension(*) :: inout
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: operation
  end subroutine

  subroutine fckit__mpi__allreduce_inplace_real32(comm,inout,count,operation) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t, c_float, c_size_t
    type(c_ptr), value :: comm
    real(c_float), dimension(*) :: inout
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: operation
  end subroutine

  subroutine fckit__mpi__allreduce_inplace_real64(comm,inout,count,operation) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t, c_double, c_size_t
    type(c_ptr), value :: comm
    real(c_double), dimension(*) :: inout
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: operation
  end subroutine

  subroutine fckit__mpi__allgather_int32(comm,in,out) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t
    type(c_ptr), value :: comm
    integer(c_int32_t) :: in
    integer(c_int32_t), dimension(*) :: out
  end subroutine

  subroutine fckit__mpi__allgather_int64(comm,in,out) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int64_t
    type(c_ptr), value :: comm
    integer(c_int64_t) :: in
    integer(c_int64_t), dimension(*) :: out
  end subroutine

  subroutine fckit__mpi__allgather_real32(comm,in,out) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_float
    type(c_ptr), value :: comm
    real(c_float) :: in
    real(c_float), dimension(*) :: out
  end subroutine

  subroutine fckit__mpi__allgather_real64(comm,in,out) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_double
    type(c_ptr), value :: comm
    real(c_double) :: in
    real(c_double), dimension(*) :: out
  end subroutine

  subroutine fckit__mpi__allgatherv_int32(comm,in,out,sendcount,recvcounts,displs) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t, c_size_t
    type(c_ptr), value :: comm
    integer(c_int32_t), dimension(*) :: in, out
    integer(c_size_t), value :: sendcount
    integer(c_int32_t), dimension(*) :: recvcounts, displs
  end subroutine

  subroutine fckit__mpi__allgatherv_int64(comm,in,out,sendcount,recvcounts,displs) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t, c_int64_t, c_size_t
    type(c_ptr), value :: comm
    integer(c_int64_t), dimension(*) :: in, out
    integer(c_size_t), value :: sendcount
    integer(c_int32_t), dimension(*) :: recvcounts, displs
  end subroutine

  subroutine fckit__mpi__allgatherv_real32(comm,in,out,sendcount,recvcounts,displs) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t, c_float, c_size_t
    type(c_ptr), value :: comm
    real(c_float), dimension(*) :: in, out
    integer(c_size_t), value :: sendcount
    integer(c_int32_t), dimension(*) :: recvcounts, displs
  end subroutine

  subroutine fckit__mpi__allgatherv_real64(comm,in,out,sendcount,recvcounts,displs) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t, c_double, c_size_t
    type(c_ptr), value :: comm
    real(c_double), dimension(*) :: in, out
    integer(c_size_t), value :: sendcount
    integer(c_int32_t), dimension(*) :: recvcounts, displs
  end subroutine

  subroutine fckit__mpi__broadcast_int32(comm,buffer,count,root) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t, c_size_t
    type(c_ptr), value :: comm
    integer(c_int32_t), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_size_t), value :: root
  end subroutine

  subroutine fckit__mpi__broadcast_int64(comm,buffer,count,root) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int64_t, c_size_t
    type(c_ptr), value :: comm
    integer(c_int64_t), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_size_t), value :: root
  end subroutine

  subroutine fckit__mpi__broadcast_real32(comm,buffer,count,root) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_float, c_size_t
    type(c_ptr), value :: comm
    real(c_float), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_size_t), value :: root
  end subroutine

  subroutine fckit__mpi__broadcast_real64(comm,buffer,count,root) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_double, c_size_t
    type(c_ptr), value :: comm
    real(c_double), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_size_t), value :: root
  end subroutine

  function fckit__mpi__broadcast_file(comm,path,root) result(buffer) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_char
    type(c_ptr) :: buffer
    type(c_ptr), value :: comm
    character(kind=c_char), dimension(*) :: path
    integer(c_size_t), value :: root
  end function

  function fckit__mpi__anytag(comm) bind(c)
    use, intrinsic :: iso_c_binding, only : c_int32_t, c_ptr
    integer(c_int32_t) fckit__mpi__anytag
    type(c_ptr), value :: comm
  end function

  function fckit__mpi__anysource(comm) bind(c)
    use, intrinsic :: iso_c_binding, only : c_int32_t, c_ptr
    integer(c_int32_t) fckit__mpi__anysource
    type(c_ptr), value :: comm
  end function

  subroutine fckit__mpi__send_int32(comm,buffer,count,dest,tag) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int32_t
    type(c_ptr), value :: comm
    integer(c_int32_t), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: dest
    integer(c_int32_t), value :: tag
  end subroutine

  subroutine fckit__mpi__send_int64(comm,buffer,count,dest,tag) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int64_t, c_size_t, c_int32_t
    type(c_ptr), value :: comm
    integer(c_int64_t), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: dest
    integer(c_int32_t), value :: tag
  end subroutine

  subroutine fckit__mpi__send_real32(comm,buffer,count,dest,tag) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_float, c_size_t, c_int32_t
    type(c_ptr), value :: comm
    real(c_float), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: dest
    integer(c_int32_t), value :: tag
  end subroutine

  subroutine fckit__mpi__send_real64(comm,buffer,count,dest,tag) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_double, c_size_t, c_int32_t
    type(c_ptr), value :: comm
    real(c_double), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: dest
    integer(c_int32_t), value :: tag
  end subroutine

  subroutine fckit__mpi__receive_int32(comm,buffer,count,source,tag,status) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int32_t
    type(c_ptr), value :: comm
    integer(c_int32_t), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: source
    integer(c_int32_t), value :: tag
    integer(c_int32_t), dimension(*) :: status
  end subroutine

  subroutine fckit__mpi__receive_int64(comm,buffer,count,source,tag,status) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int64_t, c_size_t, c_int32_t
    type(c_ptr), value :: comm
    integer(c_int64_t), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: source
    integer(c_int32_t), value :: tag
    integer(c_int32_t), dimension(*) :: status
  end subroutine

  subroutine fckit__mpi__receive_real32(comm,buffer,count,source,tag,status) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_float, c_size_t, c_int32_t
    type(c_ptr), value :: comm
    real(c_float), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: source
    integer(c_int32_t), value :: tag
    integer(c_int32_t), dimension(*) :: status
  end subroutine

  subroutine fckit__mpi__receive_real64(comm,buffer,count,source,tag,status) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_double, c_size_t, c_int32_t
    type(c_ptr), value :: comm
    real(c_double), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: source
    integer(c_int32_t), value :: tag
    integer(c_int32_t), dimension(*) :: status
  end subroutine

  function fckit__mpi__isend_int32(comm,buffer,count,dest,tag) result(request) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int32_t
    integer(c_int32_t) :: request
    type(c_ptr), value :: comm
    integer(c_int32_t), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: dest
    integer(c_int32_t), value :: tag
  end function

  function fckit__mpi__isend_int64(comm,buffer,count,dest,tag) result(request) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int64_t, c_size_t, c_int32_t
    integer(c_int32_t) :: request
    type(c_ptr), value :: comm
    integer(c_int64_t), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: dest
    integer(c_int32_t), value :: tag
  end function

  function fckit__mpi__isend_real32(comm,buffer,count,dest,tag) result(request) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_float, c_size_t, c_int32_t
    integer(c_int32_t) :: request
    type(c_ptr), value :: comm
    real(c_float), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: dest
    integer(c_int32_t), value :: tag
  end function

  function fckit__mpi__isend_real64(comm,buffer,count,dest,tag) result(request) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_double, c_size_t, c_int32_t
    integer(c_int32_t) :: request
    type(c_ptr), value :: comm
    real(c_double), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: dest
    integer(c_int32_t), value :: tag
  end function

  function fckit__mpi__ireceive_int32(comm,buffer,count,source,tag) result(request) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int32_t
    integer(c_int32_t) :: request
    type(c_ptr), value :: comm
    integer(c_int32_t), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: source
    integer(c_int32_t), value :: tag
  end function

  function fckit__mpi__ireceive_int64(comm,buffer,count,source,tag) result(request) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int64_t, c_size_t, c_int32_t
    integer(c_int32_t) :: request
    type(c_ptr), value :: comm
    integer(c_int64_t), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: source
    integer(c_int32_t), value :: tag
  end function

  function fckit__mpi__ireceive_real32(comm,buffer,count,source,tag) result(request) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_float, c_size_t, c_int32_t
    integer(c_int32_t) :: request
    type(c_ptr), value :: comm
    real(c_float), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: source
    integer(c_int32_t), value :: tag
  end function

  function fckit__mpi__ireceive_real64(comm,buffer,count,source,tag) result(request) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_double, c_size_t, c_int32_t
    integer(c_int32_t) :: request
    type(c_ptr), value :: comm
    real(c_double), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: source
    integer(c_int32_t), value :: tag
  end function

  subroutine fckit__mpi__alltoallv_real32(comm,in,scounts,sdispl,out,rcounts,rdispl) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t, c_float
    type(c_ptr), value :: comm
    real(c_float), dimension(*) :: in, out
    integer(c_int32_t), dimension(*) :: scounts, sdispl
    integer(c_int32_t), dimension(*) :: rcounts, rdispl
  end subroutine

  subroutine fckit__mpi__alltoallv_real64(comm,in,scounts,sdispl,out,rcounts,rdispl) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t, c_double
    type(c_ptr), value :: comm
    real(c_double), dimension(*) :: in, out
    integer(c_int32_t), dimension(*) :: scounts, sdispl
    integer(c_int32_t), dimension(*) :: rcounts, rdispl
  end subroutine

  subroutine fckit__mpi__wait(comm,request,status) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t
    type(c_ptr), value :: comm
    integer(c_int32_t), value :: request
    integer(c_int32_t), dimension(*) :: status
  end subroutine

end interface

!========================================================================
contains
!========================================================================

function fckit_mpi_sum() result(code)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: code
  code = fckit__mpi__sum()
end function

function fckit_mpi_prod() result(code)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: code
  code = fckit__mpi__prod()
end function

function fckit_mpi_min() result(code)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: code
  code = fckit__mpi__min()
end function

function fckit_mpi_max() result(code)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: code
  code = fckit__mpi__max()
end function

function fckit_mpi_minloc() result(code)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: code
  code = fckit__mpi__minloc()
end function

function fckit_mpi_maxloc() result(code)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: code
  code = fckit__mpi__maxloc()
end function

!---------------------------------------------------------------------------------------

function status_source(this) result(source)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: source
  class(fckit_mpi_status), intent(in) :: this
  source = this%status(1)
end function

function status_tag(this) result(tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: tag
  class(fckit_mpi_status), intent(in) :: this
  tag = this%status(2)
end function

function status_error(this) result(error)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: error
  class(fckit_mpi_status), intent(in) :: this
  error = this%status(3)
end function

!---------------------------------------------------------------------------------------

#if 1
subroutine fckit_mpi_setCommDefault_int(comm)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t), intent(in) :: comm
  call fckit__mpi__setCommDefault_int(comm)
end subroutine

!---------------------------------------------------------------------------------------

subroutine fckit_mpi_setCommDefault_name(name)
  use, intrinsic :: iso_c_binding, only : c_char
  use fckit_c_interop_module, only : c_str
  character(kind=c_char,len=*), intent(in), optional :: name
  call fckit__mpi__setCommDefault_name(c_str(name))
end subroutine
#endif
!---------------------------------------------------------------------------------------

function comm_constructor(name) result(this)
  use, intrinsic :: iso_c_binding, only : c_char
  use fckit_c_interop_module, only : c_str
  type(fckit_mpi_comm) :: this
  character(kind=c_char,len=*), intent(in), optional :: name
  if( present(name) ) then
    call this%reset_c_ptr( fckit__mpi__comm(c_str(name)))
  else
    call this%reset_c_ptr( fckit__mpi__comm_default() )
  endif
end function

function comm_wrap(comm) result(this)
  use, intrinsic :: iso_c_binding , only: c_int32_t
  type(fckit_mpi_comm) :: this
  integer(c_int32_t), intent(in) :: comm
  call this%reset_c_ptr( fckit__mpi__comm_wrap(comm) )
end function

!---------------------------------------------------------------------------------------

subroutine delete(this)
  use fckit_c_interop_module
  class(fckit_mpi_comm), intent(inout) :: this
!   call c_ptr_free(this%c_ptr())
  FCKIT_SUPPRESS_UNUSED( this )
end subroutine

!---------------------------------------------------------------------------------------

subroutine fckit_mpi_comm__final(this)
  class(fckit_mpi_comm), intent(inout) :: this
  if( .not. this%is_null() ) then
    call this%delete()
  endif
end subroutine

!---------------------------------------------------------------------------------------

FCKIT_FINAL subroutine fckit_mpi_comm__final_auto(this)
  type(fckit_mpi_comm), intent(inout) :: this
  call this%final()
end subroutine

!---------------------------------------------------------------------------------------

function communicator(this)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: communicator
  class(fckit_mpi_comm), intent(in) :: this
  communicator = fckit__mpi__comm_communicator(this%c_ptr())
end function

!---------------------------------------------------------------------------------------

function rank(this)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: rank
  class(fckit_mpi_comm), intent(in) :: this
  rank = fckit__mpi__rank(this%c_ptr())
end function

!---------------------------------------------------------------------------------------

function size(this)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: size
  class(fckit_mpi_comm), intent(in) :: this
  size = fckit__mpi__size(this%c_ptr())
end function

!---------------------------------------------------------------------------------------

function anytag(this)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: anytag
  class(fckit_mpi_comm), intent(in) :: this
  anytag = fckit__mpi__anytag(this%c_ptr())
end function

!---------------------------------------------------------------------------------------

function anysource(this)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: anysource
  class(fckit_mpi_comm), intent(in) :: this
  anysource = fckit__mpi__anysource(this%c_ptr())
end function

!---------------------------------------------------------------------------------------

subroutine barrier(this)
  class(fckit_mpi_comm), intent(in) :: this
  call fckit__mpi__barrier(this%c_ptr())
end subroutine

!---------------------------------------------------------------------------------------

subroutine abort(this,error_code)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(in), optional :: error_code
  if( present(error_code) ) then
    call fckit__mpi__abort(this%c_ptr(),error_code)
  else
    call fckit__mpi__abort(this%c_ptr(),-1)
  endif
end subroutine

!---------------------------------------------------------------------------------------

subroutine allreduce_int32_r0(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(in)    :: in
  integer(c_int32_t), intent(inout) :: out
  integer(c_int32_t), intent(in) :: operation
  integer(c_int32_t), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int32(this%c_ptr(),view_in,view_out,1_c_size_t,operation)
end subroutine

subroutine allreduce_int32_r1(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(in)    :: in(:)
  integer(c_int32_t), intent(inout) :: out(:)
  integer(c_int32_t), intent(in) :: operation
  integer(c_int32_t), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int32(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_int32_r2(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(in)    :: in(:,:)
  integer(c_int32_t), intent(inout) :: out(:,:)
  integer(c_int32_t), intent(in) :: operation
  integer(c_int32_t), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int32(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_int32_r3(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(in)    :: in(:,:,:)
  integer(c_int32_t), intent(inout) :: out(:,:,:)
  integer(c_int32_t), intent(in) :: operation
  integer(c_int32_t), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int32(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_int32_r4(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(in)    :: in(:,:,:,:)
  integer(c_int32_t), intent(inout) :: out(:,:,:,:)
  integer(c_int32_t), intent(in) :: operation
  integer(c_int32_t), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int32(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_int64_r0(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(in)    :: in
  integer(c_int64_t), intent(inout) :: out
  integer(c_int32_t), intent(in) :: operation
  integer(c_int64_t), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int64(this%c_ptr(),view_in,view_out,1_c_size_t,operation)
end subroutine

subroutine allreduce_int64_r1(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(in)    :: in(:)
  integer(c_int64_t), intent(inout) :: out(:)
  integer(c_int32_t), intent(in) :: operation
  integer(c_int64_t), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int64(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_int64_r2(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(in)    :: in(:,:)
  integer(c_int64_t), intent(inout) :: out(:,:)
  integer(c_int32_t), intent(in) :: operation
  integer(c_int64_t), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int64(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_int64_r3(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(in)    :: in(:,:,:)
  integer(c_int64_t), intent(inout) :: out(:,:,:)
  integer(c_int32_t), intent(in) :: operation
  integer(c_int64_t), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int64(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_int64_r4(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(in)    :: in(:,:,:,:)
  integer(c_int64_t), intent(inout) :: out(:,:,:,:)
  integer(c_int32_t), intent(in) :: operation
  integer(c_int64_t), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int64(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_real32_r0(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in)    :: in
  real(c_float), intent(inout) :: out
  integer(c_int32_t), intent(in) :: operation
  real(c_float), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real32(this%c_ptr(),view_in,view_out,1_c_size_t,operation)
end subroutine

subroutine allreduce_real32_r1(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in)    :: in(:)
  real(c_float), intent(inout) :: out(:)
  integer(c_int32_t), intent(in) :: operation
  real(c_float), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real32(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_real32_r2(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in)    :: in(:,:)
  real(c_float), intent(inout) :: out(:,:)
  integer(c_int32_t), intent(in) :: operation
  real(c_float), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real32(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_real32_r3(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in)    :: in(:,:,:)
  real(c_float), intent(inout) :: out(:,:,:)
  integer(c_int32_t), intent(in) :: operation
  real(c_float), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real32(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_real32_r4(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in)    :: in(:,:,:,:)
  real(c_float), intent(inout) :: out(:,:,:,:)
  integer(c_int32_t), intent(in) :: operation
  real(c_float), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real32(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_real64_r0(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in)    :: in
  real(c_double), intent(inout) :: out
  integer(c_int32_t), intent(in) :: operation
  real(c_double), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real64(this%c_ptr(),view_in,view_out,1_c_size_t,operation)
end subroutine

subroutine allreduce_real64_r1(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in)    :: in(:)
  real(c_double), intent(inout) :: out(:)
  integer(c_int32_t), intent(in) :: operation
  real(c_double), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real64(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_real64_r2(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in)    :: in(:,:)
  real(c_double), intent(inout) :: out(:,:)
  integer(c_int32_t), intent(in) :: operation
  real(c_double), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real64(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_real64_r3(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in)    :: in(:,:,:)
  real(c_double), intent(inout) :: out(:,:,:)
  integer(c_int32_t), intent(in) :: operation
  real(c_double), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real64(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_real64_r4(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in)    :: in(:,:,:,:)
  real(c_double), intent(inout) :: out(:,:,:,:)
  integer(c_int32_t), intent(in) :: operation
  real(c_double), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real64(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

!---------------------------------------------------------------------------------------

subroutine allreduce_inplace_int32_r0(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: inout
  integer(c_int32_t), intent(in) :: operation
  integer(c_int32_t), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int32(this%c_ptr(),view_inout,1_c_size_t,operation)
end subroutine

subroutine allreduce_inplace_int32_r1(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: inout(:)
  integer(c_int32_t), intent(in) :: operation
  integer(c_int32_t), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int32(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_int32_r2(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: inout(:,:)
  integer(c_int32_t), intent(in) :: operation
  integer(c_int32_t), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int32(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_int32_r3(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: inout(:,:,:)
  integer(c_int32_t), intent(in) :: operation
  integer(c_int32_t), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int32(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_int32_r4(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: inout(:,:,:,:)
  integer(c_int32_t), intent(in) :: operation
  integer(c_int32_t), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int32(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_int64_r0(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: inout
  integer(c_int32_t), intent(in) :: operation
  integer(c_int64_t), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int64(this%c_ptr(),view_inout,1_c_size_t,operation)
end subroutine

subroutine allreduce_inplace_int64_r1(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: inout(:)
  integer(c_int32_t), intent(in) :: operation
  integer(c_int64_t), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int64(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_int64_r2(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: inout(:,:)
  integer(c_int32_t), intent(in) :: operation
  integer(c_int64_t), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int64(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_int64_r3(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: inout(:,:,:)
  integer(c_int32_t), intent(in) :: operation
  integer(c_int64_t), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int64(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_int64_r4(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: inout(:,:,:,:)
  integer(c_int32_t), intent(in) :: operation
  integer(c_int64_t), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int64(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_real32_r0(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: inout
  integer(c_int32_t), intent(in) :: operation
  real(c_float), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real32(this%c_ptr(),view_inout,1_c_size_t,operation)
end subroutine

subroutine allreduce_inplace_real32_r1(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: inout(:)
  integer(c_int32_t), intent(in) :: operation
  real(c_float), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real32(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_real32_r2(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: inout(:,:)
  integer(c_int32_t), intent(in) :: operation
  real(c_float), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real32(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_real32_r3(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: inout(:,:,:)
  integer(c_int32_t), intent(in) :: operation
  real(c_float), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real32(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_real32_r4(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: inout(:,:,:,:)
  integer(c_int32_t), intent(in) :: operation
  real(c_float), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real32(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_real64_r0(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: inout
  integer(c_int32_t), intent(in) :: operation
  real(c_double), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real64(this%c_ptr(),view_inout,1_c_size_t,operation)
end subroutine

subroutine allreduce_inplace_real64_r1(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: inout(:)
  integer(c_int32_t), intent(in) :: operation
  real(c_double), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real64(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_real64_r2(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: inout(:,:)
  integer(c_int32_t), intent(in) :: operation
  real(c_double), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real64(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_real64_r3(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout)    :: inout(:,:,:)
  integer(c_int32_t), intent(in) :: operation
  real(c_double), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real64(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_real64_r4(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: inout(:,:,:,:)
  integer(c_int32_t), intent(in) :: operation
  real(c_double), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real64(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

!---------------------------------------------------------------------------------------
subroutine allgather_int32_r0(this,in,out)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(in) :: in
  integer(c_int32_t), intent(inout) :: out(:)
  integer(c_int32_t), pointer :: view_out(:)
  view_out => array_view1d(out)
  call fckit__mpi__allgather_int32(this%c_ptr(),in,view_out)
end subroutine

subroutine allgather_int64_r0(this,in,out)
  use, intrinsic :: iso_c_binding, only : c_int64_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(in) :: in
  integer(c_int64_t), intent(inout) :: out(:)
  integer(c_int64_t), pointer :: view_out(:)
  view_out => array_view1d(out)
  call fckit__mpi__allgather_int64(this%c_ptr(),in,view_out)
end subroutine

subroutine allgather_real32_r0(this,in,out)
  use, intrinsic :: iso_c_binding, only : c_float
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in) :: in
  real(c_float), intent(inout) :: out(:)
  real(c_float), pointer :: view_out(:)
  view_out => array_view1d(out)
  call fckit__mpi__allgather_real32(this%c_ptr(),in,view_out)
end subroutine

subroutine allgather_real64_r0(this,in,out)
  use, intrinsic :: iso_c_binding, only : c_double
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in) :: in
  real(c_double), intent(inout) :: out(:)
  real(c_double), pointer :: view_out(:)
  view_out => array_view1d(out)
  call fckit__mpi__allgather_real64(this%c_ptr(),in,view_out)
end subroutine

subroutine allgather_int32_r1(this,in,out,sendcount)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(in) :: in(:)
  integer(c_int32_t), intent(inout) :: out(:)
  integer(c_int32_t), intent(in) :: sendcount
  integer(c_int32_t) :: p
  integer(c_int32_t), allocatable:: recvcounts(:), displs(:)
  integer(c_int32_t), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  allocate(recvcounts(this%size()),displs(this%size()))
  recvcounts(:) = sendcount
  do p=1,this%size()
     displs(p) = (p-1)*sendcount
  enddo
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_int32(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
  deallocate(recvcounts,displs)
end subroutine

subroutine allgather_int64_r1(this,in,out,sendcount)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(in) :: in(:)
  integer(c_int64_t), intent(inout) :: out(:)
  integer(c_int32_t), intent(in) :: sendcount
  integer(c_int32_t) :: p
  integer(c_int32_t), allocatable:: recvcounts(:), displs(:)
  integer(c_int64_t), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  allocate(recvcounts(this%size()),displs(this%size()))
  recvcounts(:) = sendcount
  do p=1,this%size()
     displs(p) = (p-1)*sendcount
  enddo
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_int64(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
  deallocate(recvcounts,displs)
end subroutine

subroutine allgather_real32_r1(this,in,out,sendcount)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in) :: in(:)
  real(c_float), intent(inout) :: out(:)
  integer(c_int32_t), intent(in) :: sendcount
  integer(c_int32_t) :: p
  integer(c_int32_t), allocatable:: recvcounts(:), displs(:)
  real(c_float), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  allocate(recvcounts(this%size()),displs(this%size()))
  recvcounts(:) = sendcount
  do p=1,this%size()
     displs(p) = (p-1)*sendcount
  enddo
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_real32(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
  deallocate(recvcounts,displs)
end subroutine

subroutine allgather_real64_r1(this,in,out,sendcount)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in) :: in(:)
  real(c_double), intent(inout) :: out(:)
  integer(c_int32_t), intent(in) :: sendcount
  integer(c_int32_t) :: p
  integer(c_int32_t), allocatable:: recvcounts(:), displs(:)
  real(c_double), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  allocate(recvcounts(this%size()),displs(this%size()))
  recvcounts(:) = sendcount
  do p=1,this%size()
     displs(p) = (p-1)*sendcount
  enddo
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_real64(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
  deallocate(recvcounts,displs)
end subroutine

subroutine allgather_int32_r2(this,in,out,sendcount)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(in) :: in(:,:)
  integer(c_int32_t), intent(inout) :: out(:,:)
  integer(c_int32_t), intent(in) :: sendcount
  integer(c_int32_t) :: p
  integer(c_int32_t), allocatable:: recvcounts(:), displs(:)
  integer(c_int32_t), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  allocate(recvcounts(this%size()),displs(this%size()))
  recvcounts(:) = sendcount
  do p=1,this%size()
     displs(p) = (p-1)*sendcount
  enddo
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_int32(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
  deallocate(recvcounts,displs)
end subroutine

subroutine allgather_int64_r2(this,in,out,sendcount)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(in) :: in(:,:)
  integer(c_int64_t), intent(inout) :: out(:,:)
  integer(c_int32_t), intent(in) :: sendcount
  integer(c_int32_t) :: p
  integer(c_int32_t), allocatable:: recvcounts(:), displs(:)
  integer(c_int64_t), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  allocate(recvcounts(this%size()),displs(this%size()))
  recvcounts(:) = sendcount
  do p=1,this%size()
     displs(p) = (p-1)*sendcount
  enddo
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_int64(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
  deallocate(recvcounts,displs)
end subroutine

subroutine allgather_real32_r2(this,in,out,sendcount)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in) :: in(:,:)
  real(c_float), intent(inout) :: out(:,:)
  integer(c_int32_t), intent(in) :: sendcount
  integer(c_int32_t) :: p
  integer(c_int32_t), allocatable:: recvcounts(:), displs(:)
  real(c_float), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  allocate(recvcounts(this%size()),displs(this%size()))
  recvcounts(:) = sendcount
  do p=1,this%size()
     displs(p) = (p-1)*sendcount
  enddo
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_real32(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
  deallocate(recvcounts,displs)
end subroutine

subroutine allgather_real64_r2(this,in,out,sendcount)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in) :: in(:,:)
  real(c_double), intent(inout) :: out(:,:)
  integer(c_int32_t), intent(in) :: sendcount
  integer(c_int32_t) :: p
  integer(c_int32_t), allocatable:: recvcounts(:), displs(:)
  real(c_double), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  allocate(recvcounts(this%size()),displs(this%size()))
  recvcounts(:) = sendcount
  do p=1,this%size()
     displs(p) = (p-1)*sendcount
  enddo
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_real64(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
  deallocate(recvcounts,displs)
end subroutine

subroutine allgather_int32_r3(this,in,out,sendcount)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(in) :: in(:,:,:)
  integer(c_int32_t), intent(inout) :: out(:,:,:)
  integer(c_int32_t), intent(in) :: sendcount
  integer(c_int32_t) :: p
  integer(c_int32_t), allocatable:: recvcounts(:), displs(:)
  integer(c_int32_t), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  allocate(recvcounts(this%size()),displs(this%size()))
  recvcounts(:) = sendcount
  do p=1,this%size()
     displs(p) = (p-1)*sendcount
  enddo
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_int32(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
  deallocate(recvcounts,displs)
end subroutine

subroutine allgather_int64_r3(this,in,out,sendcount)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(in) :: in(:,:,:)
  integer(c_int64_t), intent(inout) :: out(:,:,:)
  integer(c_int32_t), intent(in) :: sendcount
  integer(c_int32_t) :: p
  integer(c_int32_t), allocatable:: recvcounts(:), displs(:)
  integer(c_int64_t), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  allocate(recvcounts(this%size()),displs(this%size()))
  recvcounts(:) = sendcount
  do p=1,this%size()
     displs(p) = (p-1)*sendcount
  enddo
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_int64(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
  deallocate(recvcounts,displs)
end subroutine

subroutine allgather_real32_r3(this,in,out,sendcount)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in) :: in(:,:,:)
  real(c_float), intent(inout) :: out(:,:,:)
  integer(c_int32_t), intent(in) :: sendcount
  integer(c_int32_t) :: p
  integer(c_int32_t), allocatable:: recvcounts(:), displs(:)
  real(c_float), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  allocate(recvcounts(this%size()),displs(this%size()))
  recvcounts(:) = sendcount
  do p=1,this%size()
     displs(p) = (p-1)*sendcount
  enddo
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_real32(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
  deallocate(recvcounts,displs)
end subroutine

subroutine allgather_real64_r3(this,in,out,sendcount)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in) :: in(:,:,:)
  real(c_double), intent(inout) :: out(:,:,:)
  integer(c_int32_t), intent(in) :: sendcount
  integer(c_int32_t) :: p
  integer(c_int32_t), allocatable:: recvcounts(:), displs(:)
  real(c_double), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  allocate(recvcounts(this%size()),displs(this%size()))
  recvcounts(:) = sendcount
  do p=1,this%size()
     displs(p) = (p-1)*sendcount
  enddo
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_real64(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
  deallocate(recvcounts,displs)
end subroutine

subroutine allgather_int32_r4(this,in,out,sendcount)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(in) :: in(:,:,:,:)
  integer(c_int32_t), intent(inout) :: out(:,:,:,:)
  integer(c_int32_t), intent(in) :: sendcount
  integer(c_int32_t) :: p
  integer(c_int32_t), allocatable:: recvcounts(:), displs(:)
  integer(c_int32_t), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  allocate(recvcounts(this%size()),displs(this%size()))
  recvcounts(:) = sendcount
  do p=1,this%size()
     displs(p) = (p-1)*sendcount
  enddo
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_int32(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
  deallocate(recvcounts,displs)
end subroutine

subroutine allgather_int64_r4(this,in,out,sendcount)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(in) :: in(:,:,:,:)
  integer(c_int64_t), intent(inout) :: out(:,:,:,:)
  integer(c_int32_t), intent(in) :: sendcount
  integer(c_int32_t) :: p
  integer(c_int32_t), allocatable:: recvcounts(:), displs(:)
  integer(c_int64_t), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  allocate(recvcounts(this%size()),displs(this%size()))
  recvcounts(:) = sendcount
  do p=1,this%size()
     displs(p) = (p-1)*sendcount
  enddo
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_int64(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
  deallocate(recvcounts,displs)
end subroutine

subroutine allgather_real32_r4(this,in,out,sendcount)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in) :: in(:,:,:,:)
  real(c_float), intent(inout) :: out(:,:,:,:)
  integer(c_int32_t), intent(in) :: sendcount
  integer(c_int32_t) :: p
  integer(c_int32_t), allocatable:: recvcounts(:), displs(:)
  real(c_float), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  allocate(recvcounts(this%size()),displs(this%size()))
  recvcounts(:) = sendcount
  do p=1,this%size()
     displs(p) = (p-1)*sendcount
  enddo
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_real32(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
  deallocate(recvcounts,displs)
end subroutine

subroutine allgather_real64_r4(this,in,out,sendcount)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in) :: in(:,:,:,:)
  real(c_double), intent(inout) :: out(:,:,:,:)
  integer(c_int32_t), intent(in) :: sendcount
  integer(c_int32_t) :: p
  integer(c_int32_t), allocatable:: recvcounts(:), displs(:)
  real(c_double), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  allocate(recvcounts(this%size()),displs(this%size()))
  recvcounts(:) = sendcount
  do p=1,this%size()
     displs(p) = (p-1)*sendcount
  enddo
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_real64(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
  deallocate(recvcounts,displs)
end subroutine

subroutine allgatherv_int32_r1(this,in,out,sendcount,recvcounts,displs)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(in) :: in(:)
  integer(c_int32_t), intent(inout) :: out(:)
  integer(c_int32_t), value :: sendcount
  integer(c_int32_t), intent(in) :: recvcounts(:), displs(:)
  integer(c_int32_t), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_int32(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
end subroutine

subroutine allgatherv_int64_r1(this,in,out,sendcount,recvcounts,displs)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(in) :: in(:)
  integer(c_int64_t), intent(inout) :: out(:)
  integer(c_int32_t), value :: sendcount
  integer(c_int32_t), intent(in) :: recvcounts(:), displs(:)
  integer(c_int64_t), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_int64(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
end subroutine

subroutine allgatherv_real32_r1(this,in,out,sendcount,recvcounts,displs)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in) :: in(:)
  real(c_float), intent(inout) :: out(:)
  integer(c_int32_t), value :: sendcount
  integer(c_int32_t), intent(in) :: recvcounts(:), displs(:)
  real(c_float), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_real32(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
end subroutine

subroutine allgatherv_real64_r1(this,in,out,sendcount,recvcounts,displs)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in) :: in(:)
  real(c_double), intent(inout) :: out(:)
  integer(c_int32_t), value :: sendcount
  integer(c_int32_t), intent(in) :: recvcounts(:), displs(:)
  real(c_double), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_real64(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
end subroutine

subroutine allgatherv_int32_r2(this,in,out,sendcount,recvcounts,displs)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(in) :: in(:,:)
  integer(c_int32_t), intent(inout) :: out(:,:)
  integer(c_int32_t), value :: sendcount
  integer(c_int32_t), intent(in) :: recvcounts(:), displs(:)
  integer(c_int32_t), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_int32(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
end subroutine

subroutine allgatherv_int64_r2(this,in,out,sendcount,recvcounts,displs)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(in) :: in(:,:)
  integer(c_int64_t), intent(inout) :: out(:,:)
  integer(c_int32_t), value :: sendcount
  integer(c_int32_t), intent(in) :: recvcounts(:), displs(:)
  integer(c_int64_t), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_int64(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
end subroutine

subroutine allgatherv_real32_r2(this,in,out,sendcount,recvcounts,displs)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in) :: in(:,:)
  real(c_float), intent(inout) :: out(:,:)
  integer(c_int32_t), value :: sendcount
  integer(c_int32_t), intent(in) :: recvcounts(:), displs(:)
  real(c_float), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_real32(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
end subroutine

subroutine allgatherv_real64_r2(this,in,out,sendcount,recvcounts,displs)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in) :: in(:,:)
  real(c_double), intent(inout) :: out(:,:)
  integer(c_int32_t), value :: sendcount
  integer(c_int32_t), intent(in) :: recvcounts(:), displs(:)
  real(c_double), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_real64(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
end subroutine

subroutine allgatherv_int32_r3(this,in,out,sendcount,recvcounts,displs)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(in) :: in(:,:,:)
  integer(c_int32_t), intent(inout) :: out(:,:,:)
  integer(c_int32_t), value :: sendcount
  integer(c_int32_t), intent(in) :: recvcounts(:), displs(:)
  integer(c_int32_t), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_int32(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
end subroutine

subroutine allgatherv_int64_r3(this,in,out,sendcount,recvcounts,displs)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(in) :: in(:,:,:)
  integer(c_int64_t), intent(inout) :: out(:,:,:)
  integer(c_int32_t), value :: sendcount
  integer(c_int32_t), intent(in) :: recvcounts(:), displs(:)
  integer(c_int64_t), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_int64(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
end subroutine

subroutine allgatherv_real32_r3(this,in,out,sendcount,recvcounts,displs)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in) :: in(:,:,:)
  real(c_float), intent(inout) :: out(:,:,:)
  integer(c_int32_t), value :: sendcount
  integer(c_int32_t), intent(in) :: recvcounts(:), displs(:)
  real(c_float), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_real32(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
end subroutine

subroutine allgatherv_real64_r3(this,in,out,sendcount,recvcounts,displs)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in) :: in(:,:,:)
  real(c_double), intent(inout) :: out(:,:,:)
  integer(c_int32_t), value :: sendcount
  integer(c_int32_t), intent(in) :: recvcounts(:), displs(:)
  real(c_double), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_real64(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
end subroutine

subroutine allgatherv_int32_r4(this,in,out,sendcount,recvcounts,displs)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(in) :: in(:,:,:,:)
  integer(c_int32_t), intent(inout) :: out(:,:,:,:)
  integer(c_int32_t), value :: sendcount
  integer(c_int32_t), intent(in) :: recvcounts(:), displs(:)
  integer(c_int32_t), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_int32(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
end subroutine

subroutine allgatherv_int64_r4(this,in,out,sendcount,recvcounts,displs)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(in) :: in(:,:,:,:)
  integer(c_int64_t), intent(inout) :: out(:,:,:,:)
  integer(c_int32_t), value :: sendcount
  integer(c_int32_t), intent(in) :: recvcounts(:), displs(:)
  integer(c_int64_t), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_int64(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
end subroutine

subroutine allgatherv_real32_r4(this,in,out,sendcount,recvcounts,displs)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in) :: in(:,:,:,:)
  real(c_float), intent(inout) :: out(:,:,:,:)
  integer(c_int32_t), value :: sendcount
  integer(c_int32_t), intent(in) :: recvcounts(:), displs(:)
  real(c_float), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_real32(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
end subroutine

subroutine allgatherv_real64_r4(this,in,out,sendcount,recvcounts,displs)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in) :: in(:,:,:,:)
  real(c_double), intent(inout) :: out(:,:,:,:)
  integer(c_int32_t), value :: sendcount
  integer(c_int32_t), intent(in) :: recvcounts(:), displs(:)
  real(c_double), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)  
  call fckit__mpi__allgatherv_real64(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
end subroutine

!---------------------------------------------------------------------------------------

subroutine broadcast_int32_r0(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: root
  integer(c_int32_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_int32_r1(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: root
  integer(c_int32_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_int32_r2(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: root
  integer(c_int32_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_int32_r3(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: root
  integer(c_int32_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_int32_r4(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: root
  integer(c_int32_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_int64_r0(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: root
  integer(c_int64_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_int64_r1(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: root
  integer(c_int64_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_int64_r2(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: root
  integer(c_int64_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_int64_r3(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: root
  integer(c_int64_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_int64_r4(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: root
  integer(c_int64_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_real32_r0(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: root
  real(c_float), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_real32_r1(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: root
  real(c_float), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_real32_r2(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: root
  real(c_float), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_real32_r3(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: root
  real(c_float), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_real32_r4(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: root
  real(c_float), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_real64_r0(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: root
  real(c_double), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_real64_r1(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: root
  real(c_double), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_real64_r2(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: root
  real(c_double), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_real64_r3(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: root
  real(c_double), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_real64_r4(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: root
  real(c_double), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

!---------------------------------------------------------------------------------------

function broadcast_file(this,path,root) result(buffer)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t, c_ptr, c_char
  use fckit_c_interop_module, only : c_str
  type(fckit_buffer) :: buffer
  class(fckit_mpi_comm), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: path
  integer(c_int32_t), intent(in) :: root
  buffer = fckit_buffer( fckit__mpi__broadcast_file(this%c_ptr(),c_str(path),int(root,c_size_t)), share=.true. )
  call buffer%return()
end function

!---------------------------------------------------------------------------------------

subroutine send_int32_r0(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int32_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_int32_r1(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int32_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_int32_r2(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int32_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_int32_r3(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int32_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_int32_r4(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int32_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_int64_r0(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int64_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_int64_r1(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int64_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_int64_r2(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int64_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_int64_r3(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int64_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_int64_r4(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int64_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_real32_r0(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_float), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_real32_r1(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_float), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_real32_r2(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_float), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_real32_r3(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_float), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_real32_r4(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_float), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_real64_r0(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_double), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_real64_r1(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_double), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_real64_r2(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_double), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_real64_r3(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_double), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_real64_r4(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_double), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_logical_r0(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  logical, intent(in) :: buffer
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int32_t) :: ibuffer
  integer(c_int32_t), pointer :: view_buffer(:)
  if (buffer) then
     ibuffer = 1
  else
     ibuffer = 0
  endif
  view_buffer  => array_view1d(ibuffer)
  call fckit__mpi__send_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

subroutine send_logical_r1(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  logical, intent(in) :: buffer(1:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int32_t), allocatable :: ibuffer(:)
  integer(c_int32_t), pointer :: view_buffer(:)
  integer :: j
  allocate(ibuffer(ubound(buffer,1)))
  do j=1,ubound(buffer,1)
     if (buffer(j)) then
        ibuffer(j) = 1
     else
        ibuffer(j) = 0
     endif
  enddo
  view_buffer  => array_view1d(ibuffer)
  call fckit__mpi__send_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
  deallocate(ibuffer)
end subroutine

!---------------------------------------------------------------------------------------

subroutine receive_int32_r0(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  integer(c_int32_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_int32_r1(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  integer(c_int32_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_int32_r2(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  integer(c_int32_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_int32_r3(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  integer(c_int32_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_int32_r4(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  integer(c_int32_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_int64_r0(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  integer(c_int64_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_int64_r1(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  integer(c_int64_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_int64_r2(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  integer(c_int64_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_int64_r3(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  integer(c_int64_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_int64_r4(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  integer(c_int64_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_real32_r0(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  real(c_float), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_real32_r1(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  real(c_float), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_real32_r2(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  real(c_float), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_real32_r3(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  real(c_float), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_real32_r4(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  real(c_float), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_real64_r0(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  real(c_double), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_real64_r1(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  real(c_double), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_real64_r2(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  real(c_double), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_real64_r3(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  real(c_double), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_real64_r4(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  real(c_double), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

subroutine receive_logical_r0(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  logical, intent(out) :: buffer
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  integer(c_int32_t) :: ibuffer
  integer(c_int32_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(ibuffer)
  call fckit__mpi__receive_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if (ibuffer == 0) then
     buffer = .false.
  else
     buffer = .true.
  endif
  if( present(status) ) status = status_out
end subroutine

subroutine receive_logical_r1(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  logical, intent(inout) :: buffer(1:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  integer(c_int32_t), allocatable :: ibuffer(:)
  integer(c_int32_t), pointer :: view_buffer(:)
  integer :: j
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  allocate(ibuffer(ubound(buffer,1)))
  view_buffer  => array_view1d(ibuffer)
  call fckit__mpi__receive_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  do j=1,ubound(buffer,1)
     if (ibuffer(j) == 0) then
        buffer(j) = .false.
     else
        buffer(j) = .true.
     endif
  enddo
  if( present(status) ) status = status_out
end subroutine

!---------------------------------------------------------------------------------------

function isend_int32_r0(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int32_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_int32_r1(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int32_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_int32_r2(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int32_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_int32_r3(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int32_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_int32_r4(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int32_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_int64_r0(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int64_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_int64_r1(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int64_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_int64_r2(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int64_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_int64_r3(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int64_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_int64_r4(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  integer(c_int64_t), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_real32_r0(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_float), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_real32_r1(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_float), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_real32_r2(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_float), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_real32_r3(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_float), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_real32_r4(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_float), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_real64_r0(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_double), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_real64_r1(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_double), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_real64_r2(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_double), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_real64_r3(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_double), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

function isend_real64_r4(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  real(c_double), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

!---------------------------------------------------------------------------------------

function ireceive_int32_r0(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  integer(c_int32_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_int32_r1(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  integer(c_int32_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_int32_r2(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  integer(c_int32_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_int32_r3(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  integer(c_int32_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_int32_r4(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int32_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  integer(c_int32_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_int64_r0(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  integer(c_int64_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_int64_r1(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  integer(c_int64_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_int64_r2(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  integer(c_int64_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_int64_r3(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  integer(c_int64_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_int64_r4(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_int64_t, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int64_t), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  integer(c_int64_t), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_real32_r0(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  real(c_float), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_real32_r1(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  real(c_float), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_real32_r2(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  real(c_float), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_real32_r3(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  real(c_float), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_real32_r4(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  real(c_float), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_real64_r0(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  real(c_double), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_real64_r1(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  real(c_double), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_real64_r2(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  real(c_double), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_real64_r3(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:,:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  real(c_double), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

function ireceive_real64_r4(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer(:,:,:,:)
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  real(c_double), pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%c_ptr())
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function
!---------------------------------------------------------------------------------------

subroutine alltoallv_real32_r1(this,in,scounts,sdispl,out,rcounts,rdispl)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_float
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in) :: in(:)
  integer(c_int32_t), intent(in) :: scounts(:), sdispl(:)
  real(c_float), intent(inout) :: out(:)
  integer(c_int32_t), intent(in) :: rcounts(:), rdispl(:)
  real(c_float), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_sc(:), view_sd(:)
  integer(c_int32_t), pointer :: view_rc(:), view_rd(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_sc => array_view1d(scounts)
  view_sd => array_view1d(sdispl)  
  view_rc => array_view1d(rcounts)
  view_rd => array_view1d(rdispl)  
  call fckit__mpi__alltoallv_real32(this%c_ptr(),view_in,view_sc,view_sd,view_out,view_rc,view_rd)
end subroutine

subroutine alltoallv_real64_r1(this,in,scounts,sdispl,out,rcounts,rdispl)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_double
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in) :: in(:)
  integer(c_int32_t), intent(in) :: scounts(:), sdispl(:)
  real(c_double), intent(inout) :: out(:)
  integer(c_int32_t), intent(in) :: rcounts(:), rdispl(:)
  real(c_double), pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_sc(:), view_sd(:)
  integer(c_int32_t), pointer :: view_rc(:), view_rd(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_sc => array_view1d(scounts)
  view_sd => array_view1d(sdispl)  
  view_rc => array_view1d(rcounts)
  view_rd => array_view1d(rdispl)  
  call fckit__mpi__alltoallv_real64(this%c_ptr(),view_in,view_sc,view_sd,view_out,view_rc,view_rd)
end subroutine

!---------------------------------------------------------------------------------------

subroutine wait(this,request,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(in) :: request
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  call fckit__mpi__wait(this%c_ptr(),request,status_out%status)
  if( present(status) ) status = status_out
end subroutine

!---------------------------------------------------------------------------------------

!========================================================================

end module
