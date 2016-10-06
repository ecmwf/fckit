module fckit_mpi_module
use fckit_object_module, only: fckit_object
implicit none
private

private :: fckit_object

!========================================================================
! Public interface

public :: fckit_mpi_comm
public :: fckit_mpi_setCommDefault

! Temporary until ECKIT-166 is fixed
public :: fckit_mpi_finalize

!========================================================================

interface fckit_mpi_setCommDefault
  module procedure fckit_mpi_setCommDefault_int
  module procedure fckit_mpi_setCommDefault_name
end interface

!========================================================================

type, extends(fckit_object) :: fckit_mpi_comm
contains
  procedure, public :: final => final_c
  procedure, public :: delete

  procedure, public :: size
  procedure, public :: rank
  procedure, public :: barrier

#ifdef EC_HAVE_Fortran_FINALIZATION
 final :: final_auto
#else
#warning Automatic finalization not supported by this compiler
#endif

endtype

interface fckit_mpi_comm
  module procedure comm_constructor
  module procedure comm_wrap
end interface


!========================================================================

interface

  ! void eckit__mpi__finalize()
  subroutine eckit__mpi__finalize() bind(c)
  end subroutine

  ! const eckit::mpi::Comm* eckit__mpi__comm_default()
  function eckit__mpi__comm_default() bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr
    type(c_ptr) :: eckit__mpi__comm_default
  end function

  ! const eckit::mpi::Comm* eckit__mpi__comm(const char* name)
  function eckit__mpi__comm(name) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_char
    type(c_ptr) :: eckit__mpi__comm
    character(kind=c_char), dimension(*) :: name
  end function

  ! const eckit::mpi::Comm* eckit__mpi__comm_wrap(int comm)
  function eckit__mpi__comm_wrap(comm) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    type(c_ptr) :: eckit__mpi__comm_wrap
    integer(c_int), value :: comm
  end function

  function eckit__mpi__size(this) bind(c)
    use, intrinsic :: iso_c_binding, only : c_int, c_ptr
    integer(c_int) :: eckit__mpi__size
    type(c_ptr), value :: this
  end function

  function eckit__mpi__rank(this) bind(c)
    use, intrinsic :: iso_c_binding, only : c_int, c_ptr
    integer(c_int) :: eckit__mpi__rank
    type(c_ptr), value :: this
  end function

  subroutine eckit__mpi__barrier(this) bind(c)
    use, intrinsic :: iso_c_binding, only : c_int, c_ptr
    type(c_ptr), value :: this
  end subroutine

  ! void eckit__mpi__setCommDefault_int(int comm)
  subroutine eckit__mpi__setCommDefault_int(comm) bind(c,name="eckit__mpi__setCommDefault_int")
    use, intrinsic :: iso_c_binding, only : c_int
    integer(c_int), value :: comm
  end subroutine

  ! void eckit__mpi__setCommDefault_name(const char* name)
  subroutine eckit__mpi__setCommDefault_name(name) bind(c,name="eckit__mpi__setCommDefault_name")
    use, intrinsic :: iso_c_binding, only : c_char
    character(kind=c_char), dimension(*) :: name
  end subroutine

end interface

!========================================================================
contains
!========================================================================

! Temporary until ECKIT-166 is fixed
subroutine fckit_mpi_finalize()
#ifdef BUG_ECKIT_166
  call eckit__mpi__finalize()
#endif
end subroutine

subroutine fckit_mpi_setCommDefault_int(comm)
  use, intrinsic :: iso_c_binding, only : c_int
  integer(c_int), intent(in) :: comm
  call eckit__mpi__setCommDefault_int(comm)
end subroutine

subroutine fckit_mpi_setCommDefault_name(name)
  use fckit_c_interop_module, only : c_str
  character(len=*), intent(in), optional :: name
  call eckit__mpi__setCommDefault_name(c_str(name))
end subroutine

function comm_constructor(name) result(this)
  use fckit_c_interop_module, only : c_str
  type(fckit_mpi_comm) :: this
  character(len=*), intent(in), optional :: name
  if( present(name) ) then
    call this%reset_c_ptr( eckit__mpi__comm(c_str(name)))
  else
    call this%reset_c_ptr( eckit__mpi__comm_default() )
  endif
end function

function comm_wrap(comm) result(this)
  use, intrinsic :: iso_c_binding , only: c_int
  type(fckit_mpi_comm) :: this
  integer(c_int), intent(in) :: comm
  call this%reset_c_ptr( eckit__mpi__comm_wrap(comm) )
end function

subroutine delete(this)
  use fckit_c_interop_module
  class(fckit_mpi_comm), intent(inout) :: this
!   call c_ptr_free(this%c_ptr())
end subroutine

subroutine final_c(this)
  class(fckit_mpi_comm), intent(inout) :: this
  if( .not. this%is_null() ) then
    call this%delete()
  endif
end subroutine

subroutine final_auto(this)
  type(fckit_mpi_comm), intent(inout) :: this
  call this%final()
end subroutine

function rank(this)
  integer :: rank
  class(fckit_mpi_comm), intent(in) :: this
  rank = eckit__mpi__rank(this%c_ptr())
end function

function size(this)
  integer :: size
  class(fckit_mpi_comm), intent(in) :: this
  size = eckit__mpi__size(this%c_ptr())
end function

subroutine barrier(this)
  integer :: size
  class(fckit_mpi_comm), intent(in) :: this
  call eckit__mpi__barrier(this%c_ptr())
end subroutine

!========================================================================

end module
