#if 0
(C) Copyright 2013-2017 ECMWF.

This software is licensed under the terms of the Apache Licence Version 2.0
which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
In applying this licence, ECMWF does not waive the privileges and immunities
granted to it by virtue of its status as an intergovernmental organisation nor
does it submit to any jurisdiction.
#endif

#ifndef DERIVED_TYPE
#error missing template DERIVED_TYPE
#endif
#ifndef SHARED_PTR_TYPE
#error missing template SHARED_PTR_TYPE
#endif
#ifndef SHARED_PTR
#error missing template SHARED_PTR
#endif

!--------------------------------------------------
! function shared_ptr_cast(this) result(success)
!--------------------------------------------------
class(SHARED_PTR_TYPE) :: this
logical :: success
success = .false.
associate( SHARED_PTR => this%shared_ptr )
select type( SHARED_PTR )
  class is(DERIVED_TYPE)
    success = .true.
    this% SHARED_PTR => SHARED_PTR
end select
end associate
!--------------------------------------------------
! end function
!--------------------------------------------------

#undef DERIVED_TYPE
#undef SHARED_PTR_TYPE
#undef SHARED_PTR