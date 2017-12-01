! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module fckit_unique_ptr_module
use fckit_object_module, only: fckit_object
implicit none
private

!========================================================================
! Public interface

public fckit_unique_ptr

!========================================================================

type :: fckit_unique_ptr
  class(fckit_object), pointer :: ptr => null()
contains
  procedure, public :: final => fckit_unique_ptr__final
  final :: fckit_unique_ptr__final_auto
end type

interface fckit_unique_ptr
  module procedure fckit_unique_ptr
end interface


!========================================================================

private :: fckit_object

! =============================================================================
CONTAINS
! =============================================================================

subroutine fckit_unique_ptr__final(this)
  class(fckit_unique_ptr), intent(inout) :: this
  if( associated(this%ptr) ) then
    call this%ptr%final()
    deallocate(this%ptr)
  endif
end subroutine

subroutine fckit_unique_ptr__final_auto(this)
  type(fckit_unique_ptr), intent(inout) :: this
  call this%final()
end subroutine

end module
