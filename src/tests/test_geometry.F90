! (C) Copyright 2020 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


#include "fckit/fctest.h"

! -----------------------------------------------------------------------------

TESTSUITE(fckit_test_geometry)

TESTSUITE_INIT
  use fckit_module
  implicit none
  call fckit_main%init()
END_TESTSUITE_INIT

TESTSUITE_FINALIZE
  use fckit_module
  implicit none
  call fckit_main%final()
END_TESTSUITE_FINALIZE

! -----------------------------------------------------------------------------

TEST( test_sphere_distance )
  use, intrinsic :: iso_c_binding
  use fckit_geometry_module
  use fckit_module

  real(c_double), parameter :: lonA = -0.950855
  real(c_double), parameter :: latA = 51.41981
  real(c_double), parameter :: lonB = -105.27494
  real(c_double), parameter :: latB = 39.977837
  real(c_double) :: distance

  distance = sphere_distance(lonA, latA, lonB, latB)
  FCTEST_CHECK_EQUAL(distance, 1.1766380129677938_c_double)
  write(0,*) "distance = ",distance
END_TEST

! -----------------------------------------------------------------------------

TEST( test_sphere_lonlat2xyz )
  use, intrinsic :: iso_c_binding
  use fckit_geometry_module
  use fckit_module

  real(c_double), parameter :: lon = -0.950855
  real(c_double), parameter :: lat = 51.41981
  real(c_double) :: x, y, z

  call sphere_lonlat2xyz(lon, lat, x, y, z)
  FCTEST_CHECK_EQUAL(x, 0.62352345902735573_c_double)
  FCTEST_CHECK_EQUAL(y, -0.010348665275882086_c_double)
  FCTEST_CHECK_EQUAL(z, 0.78173614549256276_c_double)
  write(0,*) "x = ",x
  write(0,*) "y = ",y
  write(0,*) "z = ",z
END_TEST

! -----------------------------------------------------------------------------

TEST( test_sphere_xyz2lonlat )
  use, intrinsic :: iso_c_binding
  use fckit_geometry_module
  use fckit_module

  real(c_double), parameter :: x = 0.62352345902735573
  real(c_double), parameter :: y = -0.010348665275882086
  real(c_double), parameter :: z = 0.78173614549256276
  real(c_double) :: lon, lat

  call sphere_xyz2lonlat(x, y, z, lon, lat)
  FCTEST_CHECK_EQUAL(lon, -0.95085501743747924_c_double)
  FCTEST_CHECK_EQUAL(lat, 51.419810329104415_c_double)
  write(0,*) "lon = ",lon
  write(0,*) "lat = ",lat
END_TEST

! -----------------------------------------------------------------------------

END_TESTSUITE
