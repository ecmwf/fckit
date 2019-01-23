! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fckit.h"

!========================================================================

#! Following is fypp preprocessor code to allow generic programming in Fortran
#! to reduce the amount of code in this file 20-fold

#:mute
#:set ranks  = [0,1,2,3,4]
#:set dim    = ['','(:)','(:,:)','(:,:,:)','(:,:,:,:)','(:,:,:,:,:)']

#:set ftypes = ['integer(c_int32_t)','integer(c_int64_t)','real(c_float)','real(c_double)','logical']
#:set ctypes = ['int32','int64','real32','real64','logical']
#:set dtypes = ['int32', 'int64', 'real32', 'real64','logical']

#:set types  = list(zip(dtypes,ftypes,ctypes))


#:def begin_interface( interface )
#:global _interface
#:set _interface = interface
#:global counter
#:set counter = 0
#:global prefixes
#:set prefixes = list()
#:global individual_interfaces
#:set individual_interfaces = list()
#:enddef

#:def generate_interfaces( prefix )
#:global prefixes
#:global counter
#:set prefixes = prefixes + [prefix]
#:for rank in ranks
#:for dtype in dtypes
  procedure, private :: ${prefix}$_${dtype}$_r${rank}$
  #:set counter = counter + 1
#:endfor
#:endfor
#:enddef

#:def add_interface( interface )
#:global individual_interfaces
#:global counter
#:set individual_interfaces = individual_interfaces + [interface]
  procedure, private :: ${interface}$
  #:set counter = counter + 1
#:enddef

#:def end_interface( )
#:global counter
#:global prefixes
#:global individual_interfaces
#:set last = counter
#:set counter = 1
  generic, public :: ${_interface}$ => &
#:for interface in individual_interfaces
    & ${interface}$#{if counter < last}#, &#{endif}#
    #:set counter = counter + 1
#:endfor
#:for prefix in prefixes
#:for rank in ranks
#:for dtype in dtypes
    & ${prefix}$_${dtype}$_r${rank}$#{if counter < last}#, &#{endif}#
    #:set counter = counter + 1
#:endfor
#:endfor
#:endfor
#:enddef
#:endmute

!========================================================================

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
  !! [[fckit_mpi_module:fckit_mpi_setCommDefault(interface)]]

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

  !> MPI allreduce interface for most array and scalar types
  @:begin_interface( allreduce )
  @:generate_interfaces( allreduce )
  @:generate_interfaces( allreduce_inplace )
  @:end_interface()

  !> MPI broadcast for most array and scalar types
  @:begin_interface( broadcast )
  @:generate_interfaces( broadcast )
  @:add_interface( broadcast_string )
  @:end_interface()

  !> MPI broadcast file to buffer
  procedure, public :: broadcast_file

  !> MPI allgather interface for most array and scalar types
  @:begin_interface( allgather )
  @:generate_interfaces( allgather )
  @:generate_interfaces( allgatherv )
  @:end_interface()

  !> MPI send for most array and scalar types
  @:begin_interface( send )
  @:generate_interfaces( send )
  @:end_interface()

  !> MPI receive for most array and scalar types
  @:begin_interface( receive )
  @:generate_interfaces( receive )
  @:end_interface()

  !> MPI asynchronous send for most array and scalar types
  @:begin_interface( isend )
  @:generate_interfaces( isend )
  @:end_interface()

  !> MPI asynchronous receive for most array and scalar types
  @:begin_interface( ireceive )
  @:generate_interfaces( ireceive )
  @:end_interface()

  !> MPI allgather interface for most array and scalar types
  @:begin_interface( alltoall )
  @:generate_interfaces( alltoallv )
  @:end_interface()

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

#:for dtype,ftype,ctype in types
  subroutine fckit__mpi__allreduce_${ctype}$(comm,in,out,count,operation) bind(c)
    use, intrinsic :: iso_c_binding
    type(c_ptr), value :: comm
    ${ftype}$, dimension(*) :: in
    ${ftype}$, dimension(*) :: out
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: operation
  end subroutine

    subroutine fckit__mpi__allreduce_inplace_${ctype}$(comm,inout,count,operation) bind(c)
    use, intrinsic :: iso_c_binding
    type(c_ptr), value :: comm
    ${ftype}$, dimension(*) :: inout
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: operation
  end subroutine

  subroutine fckit__mpi__allgather_${ctype}$(comm,in,out) bind(c)
    use, intrinsic :: iso_c_binding
    type(c_ptr), value :: comm
    ${ftype}$ :: in
    ${ftype}$, dimension(*) :: out
  end subroutine

  subroutine fckit__mpi__allgatherv_${ctype}$(comm,in,out,sendcount,recvcounts,displs) bind(c)
    use, intrinsic :: iso_c_binding
    type(c_ptr), value :: comm
    ${ftype}$, dimension(*) :: in, out
    integer(c_size_t), value :: sendcount
    integer(c_int32_t), dimension(*) :: recvcounts, displs
  end subroutine

  subroutine fckit__mpi__broadcast_${ctype}$(comm,buffer,count,root) bind(c)
    use, intrinsic :: iso_c_binding
    type(c_ptr), value :: comm
    ${ftype}$, dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_size_t), value :: root
  end subroutine

  subroutine fckit__mpi__send_${ctype}$(comm,buffer,count,dest,tag) bind(c)
    use, intrinsic :: iso_c_binding
    type(c_ptr), value :: comm
    ${ftype}$, dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: dest
    integer(c_int32_t), value :: tag
  end subroutine

  subroutine fckit__mpi__receive_${ctype}$(comm,buffer,count,source,tag,status) bind(c)
    use, intrinsic :: iso_c_binding
    type(c_ptr), value :: comm
    ${ftype}$, dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: source
    integer(c_int32_t), value :: tag
    integer(c_int32_t), dimension(*) :: status
  end subroutine

  function fckit__mpi__isend_${ctype}$(comm,buffer,count,dest,tag) result(request) bind(c)
    use, intrinsic :: iso_c_binding
    integer(c_int32_t) :: request
    type(c_ptr), value :: comm
    ${ftype}$, dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: dest
    integer(c_int32_t), value :: tag
  end function

  function fckit__mpi__ireceive_${ctype}$(comm,buffer,count,source,tag) result(request) bind(c)
    use, intrinsic :: iso_c_binding
    integer(c_int32_t) :: request
    type(c_ptr), value :: comm
    ${ftype}$, dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int32_t), value :: source
    integer(c_int32_t), value :: tag
  end function

  subroutine fckit__mpi__alltoallv_${ctype}$(comm,in,scounts,sdispl,out,rcounts,rdispl) bind(c)
    use, intrinsic :: iso_c_binding
    type(c_ptr), value :: comm
    ${ftype}$, dimension(*) :: in, out
    integer(c_int32_t), dimension(*) :: scounts, sdispl
    integer(c_int32_t), dimension(*) :: rcounts, rdispl
  end subroutine

#:endfor

  subroutine fckit__mpi__broadcast_string(comm,buffer,count,root) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_char, c_size_t
    type(c_ptr), value :: comm
    character(kind=c_char,len=1), dimension(*) :: buffer
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
!   call c_ptr_free(this%CPTR_PGIBUG_A)
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
  communicator = fckit__mpi__comm_communicator(this%CPTR_PGIBUG_A)
end function

!---------------------------------------------------------------------------------------

function rank(this)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: rank
  class(fckit_mpi_comm), intent(in) :: this
  rank = fckit__mpi__rank(this%CPTR_PGIBUG_A)
end function

!---------------------------------------------------------------------------------------

function size(this)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: size
  class(fckit_mpi_comm), intent(in) :: this
  size = fckit__mpi__size(this%CPTR_PGIBUG_A)
end function

!---------------------------------------------------------------------------------------

function anytag(this)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: anytag
  class(fckit_mpi_comm), intent(in) :: this
  anytag = fckit__mpi__anytag(this%CPTR_PGIBUG_A)
end function

!---------------------------------------------------------------------------------------

function anysource(this)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  integer(c_int32_t) :: anysource
  class(fckit_mpi_comm), intent(in) :: this
  anysource = fckit__mpi__anysource(this%CPTR_PGIBUG_A)
end function

!---------------------------------------------------------------------------------------

subroutine barrier(this)
  class(fckit_mpi_comm), intent(in) :: this
  call fckit__mpi__barrier(this%CPTR_PGIBUG_A)
end subroutine

!---------------------------------------------------------------------------------------

subroutine abort(this,error_code)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(in), optional :: error_code
  if( present(error_code) ) then
    call fckit__mpi__abort(this%CPTR_PGIBUG_A,error_code)
  else
    call fckit__mpi__abort(this%CPTR_PGIBUG_A,-1)
  endif
end subroutine

!---------------------------------------------------------------------------------------

#:for dtype,ftype,ctype in types

!---------------------------------------------------------------------------------------

subroutine allgather_${dtype}$_r0(this,in,out)
  use, intrinsic :: iso_c_binding
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  ${ftype}$, intent(in) :: in
  ${ftype}$, intent(inout) :: out(:)
  ${ftype}$, pointer :: view_out(:)
  view_out => array_view1d(out)
  call fckit__mpi__allgather_${ctype}$(this%c_ptr(),in,view_out)
end subroutine

!---------------------------------------------------------------------------------------

subroutine allgatherv_${dtype}$_r0(this,in,out,dummy)
  use, intrinsic :: iso_c_binding
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  ${ftype}$, intent(in) :: in
  ${ftype}$, intent(inout) :: out(:)
  ${ftype}$, intent(in) :: dummy
  ! This routine should never be called because of dummy
end subroutine

!---------------------------------------------------------------------------------------

subroutine alltoallv_${dtype}$_r0(this,in,scounts,sdispl,out,rcounts,rdispl,dummy)
  use, intrinsic :: iso_c_binding
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  ${ftype}$, intent(in) :: in
  integer(c_int32_t), intent(in) :: scounts(:), sdispl(:)
  ${ftype}$, intent(inout) :: out
  integer(c_int32_t), intent(in) :: rcounts(:), rdispl(:)
  ${ftype}$, intent(in) :: dummy
  ! This routine should never be called because of dummy
end subroutine

!---------------------------------------------------------------------------------------

#:endfor

!---------------------------------------------------------------------------------------

#:for rank in ranks[1:]
#:for dtype,ftype,ctype in types

!---------------------------------------------------------------------------------------

subroutine allgather_${dtype}$_r${rank}$(this,in,out,sendcount)
  use, intrinsic :: iso_c_binding
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  ${ftype}$, intent(in) :: in${dim[rank]}$
  ${ftype}$, intent(inout) :: out${dim[rank]}$
  integer(c_int32_t), intent(in) :: sendcount
  integer(c_int32_t) :: p
  integer(c_int32_t), allocatable:: recvcounts(:), displs(:)
  ${ftype}$, pointer :: view_in(:), view_out(:)
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
  call fckit__mpi__allgatherv_${ctype}$(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
  deallocate(recvcounts,displs)
end subroutine

!---------------------------------------------------------------------------------------

subroutine allgatherv_${dtype}$_r${rank}$(this,in,out,sendcount,recvcounts,displs)
  use, intrinsic :: iso_c_binding
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  ${ftype}$, intent(in) :: in${dim[rank]}$
  ${ftype}$, intent(inout) :: out${dim[rank]}$
  integer(c_int32_t), value :: sendcount
  integer(c_int32_t), intent(in) :: recvcounts(:), displs(:)
  ${ftype}$, pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_rc(:), view_dp(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_rc => array_view1d(recvcounts)
  view_dp => array_view1d(displs)
  call fckit__mpi__allgatherv_${ctype}$(this%c_ptr(),view_in,view_out,int(sendcount,c_size_t),view_rc,view_dp)
end subroutine

!---------------------------------------------------------------------------------------

subroutine alltoallv_${dtype}$_r${rank}$(this,in,scounts,sdispl,out,rcounts,rdispl)
  use, intrinsic :: iso_c_binding
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  ${ftype}$, intent(in) :: in${dim[rank]}$
  integer(c_int32_t), intent(in) :: scounts(:), sdispl(:)
  ${ftype}$, intent(inout) :: out${dim[rank]}$
  integer(c_int32_t), intent(in) :: rcounts(:), rdispl(:)
  ${ftype}$, pointer :: view_in(:), view_out(:)
  integer(c_int32_t), pointer :: view_sc(:), view_sd(:)
  integer(c_int32_t), pointer :: view_rc(:), view_rd(:)
  view_in => array_view1d(in)
  view_out => array_view1d(out)
  view_sc => array_view1d(scounts)
  view_sd => array_view1d(sdispl)
  view_rc => array_view1d(rcounts)
  view_rd => array_view1d(rdispl)
  call fckit__mpi__alltoallv_${ctype}$(this%c_ptr(),view_in,view_sc,view_sd,view_out,view_rc,view_rd)
end subroutine

!---------------------------------------------------------------------------------------

#:endfor
#:endfor

!---------------------------------------------------------------------------------------

subroutine broadcast_string(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t, c_char, c_null_char
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  character(len=*),intent(inout) :: buffer
  integer(c_int32_t), intent(in) :: root
  character(kind=c_char,len=1),allocatable :: c_string(:)
  integer :: j
  allocate(c_string(len(buffer)+1))
  if (this%rank() == root) then
     c_string(:)=''
     do j=1,len_trim(buffer)
        c_string(j) = buffer(j:j)
     enddo
     c_string(len(buffer)+1) = c_null_char
  endif
  call fckit__mpi__broadcast_string(this%c_ptr(),c_string,int(len(buffer)+1,c_size_t),int(root,c_size_t))
  do j=1,len(buffer)
     buffer(j:j) = c_string(j)
  enddo
  deallocate(c_string)
end subroutine

!---------------------------------------------------------------------------------------

function broadcast_file(this,path,root) result(buffer)
  use, intrinsic :: iso_c_binding, only : c_int32_t, c_size_t, c_ptr, c_char
  use fckit_c_interop_module, only : c_str
  type(fckit_buffer) :: buffer
  class(fckit_mpi_comm), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: path
  integer(c_int32_t), intent(in) :: root
  buffer = fckit_buffer( fckit__mpi__broadcast_file(this%CPTR_PGIBUG_A,c_str(path),int(root,c_size_t)), share=.true. )
  call buffer%return()
end function

!---------------------------------------------------------------------------------------

#:for rank in ranks
#:for dtype,ftype,ctype in types

!---------------------------------------------------------------------------------------

subroutine allreduce_${dtype}$_r${rank}$(this,in,out,operation)
  use, intrinsic :: iso_c_binding
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  ${ftype}$, intent(in)    :: in${dim[rank]}$
  ${ftype}$, intent(inout) :: out${dim[rank]}$
  integer(c_int32_t), intent(in) :: operation
  ${ftype}$, pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_${ctype}$(this%CPTR_PGIBUG_A,view_in,view_out, &
    int(ubound(view_in,1),c_size_t),operation)
end subroutine

!---------------------------------------------------------------------------------------

subroutine allreduce_inplace_${dtype}$_r${rank}$(this,inout,operation)
  use, intrinsic :: iso_c_binding
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  ${ftype}$, intent(inout) :: inout${dim[rank]}$
  integer(c_int32_t), intent(in) :: operation
  ${ftype}$, pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_${ctype}$(this%CPTR_PGIBUG_A,view_inout, &
    int(ubound(view_inout,1),c_size_t),operation)
end subroutine

!---------------------------------------------------------------------------------------

subroutine broadcast_${ctype}$_r${rank}$(this,buffer,root)
  use, intrinsic :: iso_c_binding
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  ${ftype}$, intent(inout) :: buffer${dim[rank]}$
  integer(c_int32_t), intent(in) :: root
  ${ftype}$, pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_${ctype}$(this%CPTR_PGIBUG_A,view_buffer, &
    int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

!---------------------------------------------------------------------------------------

subroutine send_${ctype}$_r${rank}$(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  ${ftype}$, intent(inout) :: buffer${dim[rank]}$
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  ${ftype}$, pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_${ctype}$(this%CPTR_PGIBUG_A,view_buffer, &
    int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

!---------------------------------------------------------------------------------------

subroutine receive_${ctype}$_r${rank}$(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  ${ftype}$, intent(inout) :: buffer${dim[rank]}$
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  ${ftype}$, pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%CPTR_PGIBUG_A)
  endif
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_${ctype}$(this%CPTR_PGIBUG_A,view_buffer, &
    int(ubound(view_buffer,1),c_size_t),source,tag_opt,status_out%status)
  if( present(status) ) status = status_out
end subroutine

!---------------------------------------------------------------------------------------

function isend_${ctype}$_r${rank}$(this,buffer,dest,tag) result(request)
  use, intrinsic :: iso_c_binding
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  ${ftype}$, intent(inout) :: buffer${dim[rank]}$
  integer(c_int32_t), intent(in) :: dest
  integer(c_int32_t), intent(in) :: tag
  ${ftype}$, pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__isend_${ctype}$(this%CPTR_PGIBUG_A,view_buffer, &
    int(ubound(view_buffer,1),c_size_t),dest,tag)
end function

!---------------------------------------------------------------------------------------

function ireceive_${ctype}$_r${rank}$(this,buffer,source,tag) result(request)
  use, intrinsic :: iso_c_binding
  use fckit_array_module, only: array_view1d
  integer(c_int32_t) :: request
  class(fckit_mpi_comm), intent(in) :: this
  ${ftype}$, intent(inout) :: buffer${dim[rank]}$
  integer(c_int32_t), intent(in) :: source
  integer(c_int32_t), intent(in), optional :: tag
  integer(c_int32_t) :: tag_opt
  ${ftype}$, pointer :: view_buffer(:)
  if( present(tag) ) then
    tag_opt = tag
  else
    tag_opt = fckit__mpi__anytag(this%CPTR_PGIBUG_A)
  endif
  view_buffer  => array_view1d(buffer)
  request = fckit__mpi__ireceive_${ctype}$(this%CPTR_PGIBUG_A,view_buffer, &
    int(ubound(view_buffer,1),c_size_t),source,tag_opt)
end function

!---------------------------------------------------------------------------------------

#:endfor
#:endfor

!---------------------------------------------------------------------------------------

subroutine wait(this,request,status)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int32_t), intent(in) :: request
  type(fckit_mpi_status), optional, intent(out) :: status
  type(fckit_mpi_status) :: status_out
  call fckit__mpi__wait(this%CPTR_PGIBUG_A,request,status_out%status)
  if( present(status) ) status = status_out
end subroutine

!---------------------------------------------------------------------------------------

!========================================================================

end module
