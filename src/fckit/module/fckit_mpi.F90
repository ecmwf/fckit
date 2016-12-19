module fckit_mpi_module
use fckit_object_module, only: fckit_object
implicit none
private

private :: fckit_object

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
  character(len=32) :: name
contains
  procedure, public :: final => final_c
  procedure, public :: delete

  procedure, public :: tag
  procedure, public :: size
  procedure, public :: rank
  procedure, public :: barrier
  procedure, public :: abort
  
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
  procedure, private :: broadcast_int32_r0
  procedure, private :: broadcast_int64_r0
  procedure, private :: broadcast_real32_r0
  procedure, private :: broadcast_real64_r0
  procedure, private :: send_real64_r0
  procedure, private :: receive_real64_r0

  
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

  generic, public :: broadcast => &
    & broadcast_int32_r0   ,&
    & broadcast_int64_r0   ,&
    & broadcast_real32_r0  ,&
    & broadcast_real64_r0
    
  generic, public :: send => &
    & send_real64_r0

  generic, public :: receive => &
    & receive_real64_r0

#ifdef EC_HAVE_Fortran_FINALIZATION
 final :: final_auto
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

  function fckit__mpi__comm_tag(comm) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    integer(c_int) :: fckit__mpi__comm_tag
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
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    type(c_ptr) :: fckit__mpi__comm_wrap
    integer(c_int), value :: comm
  end function

  function fckit__mpi__size(this) bind(c)
    use, intrinsic :: iso_c_binding, only : c_int, c_ptr
    integer(c_int) :: fckit__mpi__size
    type(c_ptr), value :: this
  end function

  function fckit__mpi__rank(this) bind(c)
    use, intrinsic :: iso_c_binding, only : c_int, c_ptr
    integer(c_int) :: fckit__mpi__rank
    type(c_ptr), value :: this
  end function

  subroutine fckit__mpi__barrier(this) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr
    type(c_ptr), value :: this
  end subroutine

  subroutine fckit__mpi__abort(this,error_code) bind(c)
    use, intrinsic :: iso_c_binding, only : c_int, c_ptr
    type(c_ptr), value :: this
    integer(c_int), value :: error_code
  end subroutine

  ! void fckit__mpi__setCommDefault_int(int comm)
  subroutine fckit__mpi__setCommDefault_int(comm) bind(c,name="fckit__mpi__setCommDefault_int")
    use, intrinsic :: iso_c_binding, only : c_int
    integer(c_int), value :: comm
  end subroutine

  ! void fckit__mpi__setCommDefault_name(const char* name)
  subroutine fckit__mpi__setCommDefault_name(name) bind(c,name="fckit__mpi__setCommDefault_name")
    use, intrinsic :: iso_c_binding, only : c_char
    character(kind=c_char), dimension(*) :: name
  end subroutine

  function fckit__mpi__sum() bind(c)
    use, intrinsic :: iso_c_binding, only : c_int
    integer(c_int) :: fckit__mpi__sum
  end function

  function fckit__mpi__prod() bind(c)
    use, intrinsic :: iso_c_binding, only : c_int
    integer(c_int) :: fckit__mpi__prod
  end function
  
  function fckit__mpi__min() bind(c)
    use, intrinsic :: iso_c_binding, only : c_int
    integer(c_int) :: fckit__mpi__min
  end function
  
  function fckit__mpi__max() bind(c)
    use, intrinsic :: iso_c_binding, only : c_int
    integer(c_int) :: fckit__mpi__max
  end function
  
  function fckit__mpi__minloc() bind(c)
    use, intrinsic :: iso_c_binding, only : c_int
    integer(c_int) :: fckit__mpi__minloc
  end function
  
  function fckit__mpi__maxloc() bind(c)
    use, intrinsic :: iso_c_binding, only : c_int
    integer(c_int) :: fckit__mpi__maxloc
  end function

  subroutine fckit__mpi__allreduce_int32(comm,in,out,count,operation) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int, c_size_t
    type(c_ptr), value :: comm
    integer(c_int), dimension(*) :: in
    integer(c_int), dimension(*) :: out
    integer(c_size_t), value :: count
    integer(c_int), value :: operation
  end subroutine
  
  subroutine fckit__mpi__allreduce_int64(comm,in,out,count,operation) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int, c_long, c_size_t
    type(c_ptr), value :: comm
    integer(c_long), dimension(*) :: in
    integer(c_long), dimension(*) :: out
    integer(c_size_t), value :: count
    integer(c_int), value :: operation
  end subroutine

  subroutine fckit__mpi__allreduce_real32(comm,in,out,count,operation) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int, c_float, c_size_t
    type(c_ptr), value :: comm
    real(c_float), dimension(*) :: in
    real(c_float), dimension(*) :: out
    integer(c_size_t), value :: count
    integer(c_int), value :: operation
  end subroutine
  
  subroutine fckit__mpi__allreduce_real64(comm,in,out,count,operation) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int, c_double, c_size_t
    type(c_ptr), value :: comm
    real(c_double), dimension(*) :: in
    real(c_double), dimension(*) :: out
    integer(c_size_t), value :: count
    integer(c_int), value :: operation
  end subroutine

  subroutine fckit__mpi__allreduce_inplace_int32(comm,inout,count,operation) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int, c_size_t
    type(c_ptr), value :: comm
    integer(c_int), dimension(*) :: inout
    integer(c_size_t), value :: count
    integer(c_int), value :: operation
  end subroutine

  subroutine fckit__mpi__allreduce_inplace_int64(comm,inout,count,operation) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int, c_long, c_size_t
    type(c_ptr), value :: comm
    integer(c_long), dimension(*) :: inout
    integer(c_size_t), value :: count
    integer(c_int), value :: operation
  end subroutine

  subroutine fckit__mpi__allreduce_inplace_real32(comm,inout,count,operation) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int, c_float, c_size_t
    type(c_ptr), value :: comm
    real(c_float), dimension(*) :: inout
    integer(c_size_t), value :: count
    integer(c_int), value :: operation
  end subroutine
  
  subroutine fckit__mpi__allreduce_inplace_real64(comm,inout,count,operation) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int, c_double, c_size_t
    type(c_ptr), value :: comm
    real(c_double), dimension(*) :: inout
    integer(c_size_t), value :: count
    integer(c_int), value :: operation
  end subroutine
  
  subroutine fckit__mpi__broadcast_int32(comm,buffer,count,root) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_int, c_size_t
    type(c_ptr), value :: comm
    integer(c_int), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_size_t), value :: root
  end subroutine

  subroutine fckit__mpi__broadcast_int64(comm,buffer,count,root) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_long, c_size_t
    type(c_ptr), value :: comm
    integer(c_long), dimension(*) :: buffer
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
  
  subroutine fckit__mpi__send_real64(comm,buffer,count,dest,tag) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_double, c_size_t, c_int  
    type(c_ptr), value :: comm
    real(c_double), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int), value :: dest
    integer(c_int), value :: tag
  end subroutine
  
  subroutine fckit__mpi__receive_real64(comm,buffer,count,dest,tag,status) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr, c_double, c_size_t, c_int  
    type(c_ptr), value :: comm
    real(c_double), dimension(*) :: buffer
    integer(c_size_t), value :: count
    integer(c_int), value :: dest
    integer(c_int), value :: tag
    integer(c_int), dimension(*) :: status
  end subroutine
  
end interface

!========================================================================
contains
!========================================================================

function fckit_mpi_sum() result(code)
  use, intrinsic :: iso_c_binding, only : c_int
  integer(c_int) :: code
  code = fckit__mpi__sum()
end function

function fckit_mpi_prod() result(code)
  use, intrinsic :: iso_c_binding, only : c_int
  integer(c_int) :: code
  code = fckit__mpi__prod()
end function

function fckit_mpi_min() result(code)
  use, intrinsic :: iso_c_binding, only : c_int
  integer(c_int) :: code
  code = fckit__mpi__min()
end function

function fckit_mpi_max() result(code)
  use, intrinsic :: iso_c_binding, only : c_int
  integer(c_int) :: code
  code = fckit__mpi__max()
end function

function fckit_mpi_minloc() result(code)
  use, intrinsic :: iso_c_binding, only : c_int
  integer(c_int) :: code
  code = fckit__mpi__minloc()
end function

function fckit_mpi_maxloc() result(code)
  use, intrinsic :: iso_c_binding, only : c_int
  integer(c_int) :: code
  code = fckit__mpi__maxloc()
end function

!---------------------------------------------------------------------------------------

subroutine fckit_mpi_setCommDefault_int(comm)
  use, intrinsic :: iso_c_binding, only : c_int
  integer(c_int), intent(in) :: comm
  call fckit__mpi__setCommDefault_int(comm)
end subroutine

!---------------------------------------------------------------------------------------

subroutine fckit_mpi_setCommDefault_name(name)
  use fckit_c_interop_module, only : c_str
  character(len=*), intent(in), optional :: name
  call fckit__mpi__setCommDefault_name(c_str(name))
end subroutine

!---------------------------------------------------------------------------------------

function comm_constructor(name) result(this)
  use fckit_c_interop_module, only : c_str
  type(fckit_mpi_comm) :: this
  character(len=*), intent(in), optional :: name
  if( present(name) ) then
    call this%reset_c_ptr( fckit__mpi__comm(c_str(name)))
  else
    call this%reset_c_ptr( fckit__mpi__comm_default() )
  endif
end function

function comm_wrap(comm) result(this)
  use, intrinsic :: iso_c_binding , only: c_int
  type(fckit_mpi_comm) :: this
  integer(c_int), intent(in) :: comm
  call this%reset_c_ptr( fckit__mpi__comm_wrap(comm) )
end function

!---------------------------------------------------------------------------------------

subroutine delete(this)
  use fckit_c_interop_module
  class(fckit_mpi_comm), intent(inout) :: this
!   call c_ptr_free(this%c_ptr())
end subroutine

!---------------------------------------------------------------------------------------

subroutine final_c(this)
  class(fckit_mpi_comm), intent(inout) :: this
  if( .not. this%is_null() ) then
    call this%delete()
  endif
end subroutine

!---------------------------------------------------------------------------------------

subroutine final_auto(this)
  type(fckit_mpi_comm), intent(inout) :: this
  call this%final()
end subroutine

!---------------------------------------------------------------------------------------

function tag(this)
  integer :: tag
  class(fckit_mpi_comm), intent(in) :: this
  tag = fckit__mpi__comm_tag(this%c_ptr())
end function

!---------------------------------------------------------------------------------------
  
function rank(this)
  integer :: rank
  class(fckit_mpi_comm), intent(in) :: this
  rank = fckit__mpi__rank(this%c_ptr())
end function

!---------------------------------------------------------------------------------------

function size(this)
  integer :: size
  class(fckit_mpi_comm), intent(in) :: this
  size = fckit__mpi__size(this%c_ptr())
end function

!---------------------------------------------------------------------------------------

subroutine barrier(this)
  class(fckit_mpi_comm), intent(in) :: this
  call fckit__mpi__barrier(this%c_ptr())
end subroutine

!---------------------------------------------------------------------------------------

subroutine abort(this,error_code)
  class(fckit_mpi_comm), intent(in) :: this
  integer, intent(in), optional :: error_code
  if( present(error_code) ) then
    call fckit__mpi__abort(this%c_ptr(),error_code)
  else
    call fckit__mpi__abort(this%c_ptr(),-1)
  endif
end subroutine

!---------------------------------------------------------------------------------------

subroutine allreduce_int32_r0(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int), intent(in)    :: in
  integer(c_int), intent(inout) :: out
  integer(c_int), intent(in) :: operation
  integer(c_int), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int32(this%c_ptr(),view_in,view_out,1_c_size_t,operation)
end subroutine

subroutine allreduce_int32_r1(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int), intent(in)    :: in(:)
  integer(c_int), intent(inout) :: out(:)
  integer(c_int), intent(in) :: operation
  integer(c_int), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int32(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_int32_r2(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int), intent(in)    :: in(:,:)
  integer(c_int), intent(inout) :: out(:,:)
  integer(c_int), intent(in) :: operation
  integer(c_int), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int32(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_int32_r3(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int), intent(in)    :: in(:,:,:)
  integer(c_int), intent(inout) :: out(:,:,:)
  integer(c_int), intent(in) :: operation
  integer(c_int), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int32(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_int32_r4(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int), intent(in)    :: in(:,:,:,:)
  integer(c_int), intent(inout) :: out(:,:,:,:)
  integer(c_int), intent(in) :: operation
  integer(c_int), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int32(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_int64_r0(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_long, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_long), intent(in)    :: in
  integer(c_long), intent(inout) :: out
  integer(c_int), intent(in) :: operation
  integer(c_long), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int64(this%c_ptr(),view_in,view_out,1_c_size_t,operation)
end subroutine

subroutine allreduce_int64_r1(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_long, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_long), intent(in)    :: in(:)
  integer(c_long), intent(inout) :: out(:)
  integer(c_int), intent(in) :: operation
  integer(c_long), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int64(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_int64_r2(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_long, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_long), intent(in)    :: in(:,:)
  integer(c_long), intent(inout) :: out(:,:)
  integer(c_int), intent(in) :: operation
  integer(c_long), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int64(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_int64_r3(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_long, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_long), intent(in)    :: in(:,:,:)
  integer(c_long), intent(inout) :: out(:,:,:)
  integer(c_int), intent(in) :: operation
  integer(c_long), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int64(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_int64_r4(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_long, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_long), intent(in)    :: in(:,:,:,:)
  integer(c_long), intent(inout) :: out(:,:,:,:)
  integer(c_int), intent(in) :: operation
  integer(c_long), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_int64(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_real32_r0(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in)    :: in
  real(c_float), intent(inout) :: out
  integer(c_int), intent(in) :: operation
  real(c_float), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real32(this%c_ptr(),view_in,view_out,1_c_size_t,operation)
end subroutine

subroutine allreduce_real32_r1(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in)    :: in(:)
  real(c_float), intent(inout) :: out(:)
  integer(c_int), intent(in) :: operation
  real(c_float), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real32(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_real32_r2(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in)    :: in(:,:)
  real(c_float), intent(inout) :: out(:,:)
  integer(c_int), intent(in) :: operation
  real(c_float), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real32(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_real32_r3(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in)    :: in(:,:,:)
  real(c_float), intent(inout) :: out(:,:,:)
  integer(c_int), intent(in) :: operation
  real(c_float), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real32(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_real32_r4(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(in)    :: in(:,:,:,:)
  real(c_float), intent(inout) :: out(:,:,:,:)
  integer(c_int), intent(in) :: operation
  real(c_float), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real32(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_real64_r0(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in)    :: in
  real(c_double), intent(inout) :: out
  integer(c_int), intent(in) :: operation
  real(c_double), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real64(this%c_ptr(),view_in,view_out,1_c_size_t,operation)
end subroutine

subroutine allreduce_real64_r1(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in)    :: in(:)
  real(c_double), intent(inout) :: out(:)
  integer(c_int), intent(in) :: operation
  real(c_double), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real64(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_real64_r2(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in)    :: in(:,:)
  real(c_double), intent(inout) :: out(:,:)
  integer(c_int), intent(in) :: operation
  real(c_double), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real64(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_real64_r3(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in)    :: in(:,:,:)
  real(c_double), intent(inout) :: out(:,:,:)
  integer(c_int), intent(in) :: operation
  real(c_double), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real64(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

subroutine allreduce_real64_r4(this,in,out,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(in)    :: in(:,:,:,:)
  real(c_double), intent(inout) :: out(:,:,:,:)
  integer(c_int), intent(in) :: operation
  real(c_double), pointer :: view_in(:), view_out(:)
  view_in  => array_view1d(in)
  view_out => array_view1d(out)
  call fckit__mpi__allreduce_real64(this%c_ptr(),view_in,view_out,int(ubound(view_in,1),c_size_t),operation)
end subroutine

!---------------------------------------------------------------------------------------

subroutine allreduce_inplace_int32_r0(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int), intent(inout) :: inout
  integer(c_int), intent(in) :: operation
  integer(c_int), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int32(this%c_ptr(),view_inout,1_c_size_t,operation)
end subroutine

subroutine allreduce_inplace_int32_r1(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int), intent(inout) :: inout(:)
  integer(c_int), intent(in) :: operation
  integer(c_int), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int32(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_int32_r2(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int), intent(inout) :: inout(:,:)
  integer(c_int), intent(in) :: operation
  integer(c_int), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int32(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_int32_r3(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int), intent(inout) :: inout(:,:,:)
  integer(c_int), intent(in) :: operation
  integer(c_int), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int32(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_int32_r4(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int), intent(inout) :: inout(:,:,:,:)
  integer(c_int), intent(in) :: operation
  integer(c_int), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int32(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_int64_r0(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_long, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_long), intent(inout) :: inout
  integer(c_int), intent(in) :: operation
  integer(c_long), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int64(this%c_ptr(),view_inout,1_c_size_t,operation)
end subroutine

subroutine allreduce_inplace_int64_r1(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_long, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_long), intent(inout) :: inout(:)
  integer(c_int), intent(in) :: operation
  integer(c_long), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int64(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_int64_r2(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_long, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_long), intent(inout) :: inout(:,:)
  integer(c_int), intent(in) :: operation
  integer(c_long), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int64(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_int64_r3(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_long, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_long), intent(inout) :: inout(:,:,:)
  integer(c_int), intent(in) :: operation
  integer(c_long), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int64(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_int64_r4(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_long, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_long), intent(inout) :: inout(:,:,:,:)
  integer(c_int), intent(in) :: operation
  integer(c_long), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_int64(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_real32_r0(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: inout
  integer(c_int), intent(in) :: operation
  real(c_float), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real32(this%c_ptr(),view_inout,1_c_size_t,operation)
end subroutine

subroutine allreduce_inplace_real32_r1(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: inout(:)
  integer(c_int), intent(in) :: operation
  real(c_float), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real32(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_real32_r2(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: inout(:,:)
  integer(c_int), intent(in) :: operation
  real(c_float), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real32(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_real32_r3(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: inout(:,:,:)
  integer(c_int), intent(in) :: operation
  real(c_float), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real32(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_real32_r4(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: inout(:,:,:,:)
  integer(c_int), intent(in) :: operation
  real(c_float), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real32(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_real64_r0(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: inout
  integer(c_int), intent(in) :: operation
  real(c_double), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real64(this%c_ptr(),view_inout,1_c_size_t,operation)
end subroutine

subroutine allreduce_inplace_real64_r1(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: inout(:)
  integer(c_int), intent(in) :: operation
  real(c_double), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real64(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_real64_r2(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: inout(:,:)
  integer(c_int), intent(in) :: operation
  real(c_double), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real64(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_real64_r3(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout)    :: inout(:,:,:)
  integer(c_int), intent(in) :: operation
  real(c_double), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real64(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

subroutine allreduce_inplace_real64_r4(this,inout,operation)
  use, intrinsic :: iso_c_binding, only : c_int, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: inout(:,:,:,:)
  integer(c_int), intent(in) :: operation
  real(c_double), pointer :: view_inout(:)
  view_inout => array_view1d(inout)
  call fckit__mpi__allreduce_inplace_real64(this%c_ptr(),view_inout,int(ubound(view_inout,1),c_size_t),operation)
end subroutine

!---------------------------------------------------------------------------------------

subroutine broadcast_int32_r0(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_int), intent(inout) :: buffer
  integer(c_int), intent(in) :: root
  integer(c_int), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_int32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_int64_r0(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int, c_long, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  integer(c_long), intent(inout) :: buffer
  integer(c_int), intent(in) :: root
  integer(c_long), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_int64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_real32_r0(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int, c_float, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_float), intent(inout) :: buffer
  integer(c_int), intent(in) :: root
  real(c_float), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_real32(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

subroutine broadcast_real64_r0(this,buffer,root)
  use, intrinsic :: iso_c_binding, only : c_int, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer
  integer(c_int), intent(in) :: root
  real(c_double), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__broadcast_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),int(root,c_size_t))
end subroutine

!---------------------------------------------------------------------------------------

function status_source(this) result(source)
  integer :: source
  class(fckit_mpi_status), intent(in) :: this
  source = this%status(1)
end function

function status_tag(this) result(tag)
  integer :: tag
  class(fckit_mpi_status), intent(in) :: this
  tag = this%status(2)
end function

function status_error(this) result(error)
  integer :: error
  class(fckit_mpi_status), intent(in) :: this
  error = this%status(3)
end function

!---------------------------------------------------------------------------------------

subroutine send_real64_r0(this,buffer,dest,tag)
  use, intrinsic :: iso_c_binding, only : c_int, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer
  integer(c_int), intent(in) :: dest
  integer(c_int), intent(in) :: tag
  real(c_double), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__send_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),dest,tag)
end subroutine

!---------------------------------------------------------------------------------------

subroutine receive_real64_r0(this,buffer,source,tag,status)
  use, intrinsic :: iso_c_binding, only : c_int, c_double, c_size_t
  use fckit_array_module, only: array_view1d
  class(fckit_mpi_comm), intent(in) :: this
  real(c_double), intent(inout) :: buffer
  integer(c_int), intent(in) :: source
  integer(c_int), intent(in) :: tag
  type(fckit_mpi_status), intent(out) :: status
  real(c_double), pointer :: view_buffer(:)
  view_buffer  => array_view1d(buffer)
  call fckit__mpi__receive_real64(this%c_ptr(),view_buffer,int(ubound(view_buffer,1),c_size_t),source,tag,status%status)
end subroutine

!========================================================================

end module
