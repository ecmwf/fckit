! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fckit.h"

module fckit_final_module

implicit none
private

!========================================================================
! Public interface

public fckit_final

!========================================================================

type, abstract :: fckit_final
contains
  procedure(final_interface), deferred, public :: final
    !! Finalise object
end type

! =============================================================================
CONTAINS
! =============================================================================

subroutine final_interface(this)
  class(fckit_final), intent(inout) :: this
  FCKIT_SUPPRESS_UNUSED(this)
end subroutine

end module
