! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fckit.h"

module fckit_resource_module
  !! Fortran interface to eckit Resource

implicit none
private

public :: fckit_resource

!------------------------------------------------------------------------------
interface
  !int fckit__resource_int (const char* resource, int default_value, int &value)
  function fckit__resource_int32(resource,default_value,value) result(error_code) bind(c)
    use iso_c_binding, only: c_int32_t, c_char
    integer(c_int32_t) :: error_code
    character(kind=c_char), dimension(*) :: resource
    integer(c_int32_t), value :: default_value
    integer(c_int32_t) :: value
  end function
  !int fckit__resource_int64 (const char* resource, int64 default_value, int64 &value)
  function fckit__resource_int64(resource,default_value,value) result(error_code) bind(c)
    use iso_c_binding, only: c_int32_t, c_int64_t, c_char
    integer(c_int64_t) :: error_code
    character(kind=c_char), dimension(*) :: resource
    integer(c_int64_t), value :: default_value
    integer(c_int64_t) :: value
  end function
  !int fckit__resource_float (const char* resource, int default_value, int &value)
  function fckit__resource_float(resource,default_value,value) result(error_code) bind(c)
    use iso_c_binding, only: c_int32_t, c_float, c_char
    integer(c_int32_t) :: error_code
    character(kind=c_char), dimension(*) :: resource
    real(c_float), value :: default_value
    real(c_float) :: value
  end function
  !int fckit__resource_double (const char* resource, int default_value, int &value)
  function fckit__resource_double(resource,default_value,value) result(error_code) bind(c)
    use iso_c_binding, only: c_int32_t, c_double, c_char
    integer(c_int32_t) :: error_code
    character(kind=c_char), dimension(*) :: resource
    real(c_double), value :: default_value
    real(c_double) :: value
  end function
  !int fckit__resource_string (const char* resource, const char* default_value, char* &value, int &value_size)
  function fckit__resource_string(resource,default_value,value,value_size) result(error_code) bind(c)
    use iso_c_binding, only: c_int32_t, c_ptr, c_char, c_size_t
    integer(c_int32_t) :: error_code
    character(kind=c_char), dimension(*) :: resource
    character(kind=c_char), dimension(*) :: default_value
    type(c_ptr) :: value
    integer(c_size_t) :: value_size
  end function
end interface
!------------------------------------------------------------------------------

interface fckit_resource

! Purpose :
! -------
!   *resource* : Configuration
!
! Author :
! ------
!   October-2016 Willem Deconinck     *ECMWF*
! -----------------------------------------------------------------------------
  module procedure resource_get_int32
  module procedure resource_get_int64
  module procedure resource_get_real32
  module procedure resource_get_real64
  module procedure resource_get_string
end interface fckit_resource

! =============================================================================
CONTAINS
! =============================================================================

subroutine resource_get_int32(resource_str,default_value,value)
  use fckit_main_module
  use fckit_c_interop_module
  use, intrinsic :: iso_c_binding
  character(kind=c_char,len=*), intent(in) :: resource_str
  integer(c_int32_t), intent(in) :: default_value
  integer(c_int32_t), intent(out) :: value
  integer(c_int32_t) :: error_code
  error_code = fckit__resource_int32(c_str(resource_str),default_value,value)
end subroutine

subroutine resource_get_int64(resource_str,default_value,value)
  use fckit_main_module
  use fckit_c_interop_module
  use, intrinsic :: iso_c_binding
  character(kind=c_char,len=*), intent(in) :: resource_str
  integer(c_int64_t), intent(in) :: default_value
  integer(c_int64_t), intent(out) :: value
  integer(c_int32_t) :: error_code
  error_code = fckit__resource_int64(c_str(resource_str),default_value,value)
end subroutine

subroutine resource_get_real32(resource_str,default_value,value)
  use fckit_main_module
  use fckit_c_interop_module
  use, intrinsic :: iso_c_binding
  character(kind=c_char,len=*), intent(in) :: resource_str
  real(c_float), intent(in) :: default_value
  real(c_float), intent(out) :: value
  integer(c_int32_t) :: error_code
  error_code = fckit__resource_float(c_str(resource_str),default_value,value)
end subroutine

subroutine resource_get_real64(resource_str,default_value,value)
  use fckit_main_module
  use fckit_c_interop_module
  use, intrinsic :: iso_c_binding
  character(kind=c_char,len=*), intent(in) :: resource_str
  real(c_double), intent(in) :: default_value
  real(c_double), intent(out) :: value
  integer(c_int32_t) :: error_code
  error_code = fckit__resource_double(c_str(resource_str),default_value,value)
end subroutine

subroutine resource_get_string(resource_str,default_value,value)
  use fckit_main_module
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module
  character(kind=c_char,len=*), intent(in) :: resource_str
  character(kind=c_char,len=*), intent(in) :: default_value
  character(kind=c_char,len=:), allocatable, intent(inout) :: value
  type(c_ptr) :: value_c_ptr
  integer(c_size_t) :: value_size
  integer(c_int32_t) :: error_code
  error_code = fckit__resource_string(c_str(resource_str),c_str(default_value),value_c_ptr,value_size)
  FCKIT_ALLOCATE_CHARACTER(value,value_size)
  value = c_ptr_to_string(value_c_ptr)
  call c_ptr_free(value_c_ptr)
end subroutine

end module fckit_resource_module

