! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module fckit_resource_module
  !! Fortran interface to eckit Resource

implicit none
private

public :: fckit_resource

!------------------------------------------------------------------------------
interface
  !int fckit__resource_int (const char* resource, int default_value, int &value)
  function fckit__resource_int(resource,default_value,value) result(error_code) bind(c)
    use iso_c_binding, only: c_int, c_char
    integer(c_int) :: error_code
    character(kind=c_char), dimension(*) :: resource
    integer(c_int), value :: default_value
    integer(c_int) :: value
  end function
  !int fckit__resource_long (const char* resource, int default_value, int &value)
  function fckit__resource_long(resource,default_value,value) result(error_code) bind(c)
    use iso_c_binding, only: c_int, c_long, c_char
    integer(c_int) :: error_code
    character(kind=c_char), dimension(*) :: resource
    integer(c_long), value :: default_value
    integer(c_long) :: value
  end function
  !int fckit__resource_float (const char* resource, int default_value, int &value)
  function fckit__resource_float(resource,default_value,value) result(error_code) bind(c)
    use iso_c_binding, only: c_int, c_float, c_char
    integer(c_int) :: error_code
    character(kind=c_char), dimension(*) :: resource
    real(c_float), value :: default_value
    real(c_float) :: value
  end function
  !int fckit__resource_double (const char* resource, int default_value, int &value)
  function fckit__resource_double(resource,default_value,value) result(error_code) bind(c)
    use iso_c_binding, only: c_int, c_double, c_char
    integer(c_int) :: error_code
    character(kind=c_char), dimension(*) :: resource
    real(c_double), value :: default_value
    real(c_double) :: value
  end function
  !int fckit__resource_string (const char* resource, const char* default_value, char* &value, int &value_size)
  function fckit__resource_string(resource,default_value,value,value_size) result(error_code) bind(c)
    use iso_c_binding, only: c_int, c_ptr, c_char
    integer(c_int) :: error_code
    character(kind=c_char), dimension(*) :: resource
    character(kind=c_char), dimension(*) :: default_value
    type(c_ptr) :: value
    integer(c_int) :: value_size
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
  character(len=*), intent(in) :: resource_str
  integer(c_int), intent(in) :: default_value
  integer(c_int), intent(out) :: value
  integer(c_int) :: error_code
  error_code = fckit__resource_int(c_str(resource_str),default_value,value)
end subroutine

subroutine resource_get_int64(resource_str,default_value,value)
  use fckit_main_module
  use fckit_c_interop_module
  use, intrinsic :: iso_c_binding
  character(len=*), intent(in) :: resource_str
  integer(c_long), intent(in) :: default_value
  integer(c_long), intent(out) :: value
  integer(c_int) :: error_code
  error_code = fckit__resource_long(c_str(resource_str),default_value,value)
end subroutine

subroutine resource_get_real32(resource_str,default_value,value)
  use fckit_main_module
  use fckit_c_interop_module
  use, intrinsic :: iso_c_binding
  character(len=*), intent(in) :: resource_str
  real(c_float), intent(in) :: default_value
  real(c_float), intent(out) :: value
  integer(c_int) :: error_code
  error_code = fckit__resource_float(c_str(resource_str),default_value,value)
end subroutine

subroutine resource_get_real64(resource_str,default_value,value)
  use fckit_main_module
  use fckit_c_interop_module
  use, intrinsic :: iso_c_binding
  character(len=*), intent(in) :: resource_str
  real(c_double), intent(in) :: default_value
  real(c_double), intent(out) :: value
  integer(c_int) :: error_code
  error_code = fckit__resource_double(c_str(resource_str),default_value,value)
end subroutine

subroutine resource_get_string(resource_str,default_value,value)
  use fckit_main_module
  use, intrinsic :: iso_c_binding
  use fckit_c_interop_module
  character(len=*), intent(in) :: resource_str
  character(len=*), intent(in) :: default_value
  character(len=:), allocatable, intent(inout) :: value
  type(c_ptr) :: value_c_ptr
  integer(c_int) :: value_size
  integer(c_int) :: error_code
  error_code = fckit__resource_string(c_str(resource_str),c_str(default_value),value_c_ptr,value_size)
  allocate(character(len=value_size) :: value )
  value = c_ptr_to_string(value_c_ptr)
  call c_ptr_free(value_c_ptr)
end subroutine

end module fckit_resource_module

