! (C) Copyright 2020 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module fckit_geometry_module
  !! Fortran interface to eckit geometry

implicit none

private

public :: sphere_distance, sphere_lonlat2xyz, sphere_xyz2lonlat

!------------------------------------------------------------------------------
interface
  ! double fckit__sphere_distance(const double & lonA, const double & latA, const double & lonB, const double & latB)
  function fckit__sphere_distance(lonA, latA, lonB, latB) result(distance) bind(C)
    use iso_c_binding, only: c_double
    real(c_double) :: distance
    real(c_double) :: lonA
    real(c_double) :: latA
    real(c_double) :: lonB
    real(c_double) :: latB
  end function

  ! void fckit__sphere_lonlat2xyz(const double & lon, const double & lat, double & x, double & y, double & a)
  subroutine fckit__sphere_lonlat2xyz(lon, lat, x, y, z) bind(C)
    use iso_c_binding, only: c_double
    real(c_double) :: lon
    real(c_double) :: lat
    real(c_double) :: x
    real(c_double) :: y
    real(c_double) :: z
  end subroutine

  ! void fckit__sphere_xyz2lonlat(const double & x, const double & y, const double & z, double & lon, double & lat)
  subroutine fckit__sphere_xyz2lonlat(x, y, z, lon, lat) bind(C)
    use iso_c_binding, only: c_double
    real(c_double) :: x
    real(c_double) :: y
    real(c_double) :: z
    real(c_double) :: lon
    real(c_double) :: lat
  end subroutine
end interface

! =============================================================================
contains
! =============================================================================

function sphere_distance(lonA, latA, lonB, latB) result(distance)
  use iso_c_binding, only: c_double
  real(c_double) :: distance
  real(c_double), intent(in) :: lonA
  real(c_double), intent(in) :: latA
  real(c_double), intent(in) :: lonB
  real(c_double), intent(in) :: latB
  distance = fckit__sphere_distance(lonA, latA, lonB, latB)
end function

subroutine sphere_lonlat2xyz(lon, lat, x, y, z)
  use iso_c_binding, only: c_double
  real(c_double), intent(in) :: lon
  real(c_double), intent(in) :: lat
  real(c_double), intent(out) :: x
  real(c_double), intent(out) :: y
  real(c_double), intent(out) :: z
  call fckit__sphere_lonlat2xyz(lon, lat, x, y, z)
end subroutine

subroutine sphere_xyz2lonlat(x, y, z, lon, lat)
  use iso_c_binding, only: c_double
  real(c_double), intent(in) :: x
  real(c_double), intent(in) :: y
  real(c_double), intent(in) :: z
  real(c_double), intent(out) :: lon
  real(c_double), intent(out) :: lat
  call fckit__sphere_xyz2lonlat(x, y, z, lon, lat)
end subroutine

end module fckit_geometry_module
