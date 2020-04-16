! (C) Copyright 2020 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


#include "fckit/fctest.h"

! -----------------------------------------------------------------------------

TESTSUITE(fckit_test_kdtree)

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

TEST( test_kdtree )
  use, intrinsic :: iso_c_binding
  use fckit_kdtree_module
  use fckit_module

  integer(c_int32_t), parameter :: n = 2
  real(c_double), dimension(n), parameter :: lon = (/-0.950855,-105.27494/)
  real(c_double), dimension(n), parameter :: lat = (/51.41981,39.977837/)
  real(c_double), parameter :: lonp = 1.973143
  real(c_double), parameter :: latp = 44.218458
  integer(c_int32_t), parameter :: nn = 2
  real(c_double), parameter :: r = 1.0
  type(kdtree) :: kd
  integer(c_int32_t) :: nn_index(nn), nnr
  
  kd = kdtree_create(n, lon, lat)

  call kdtree_k_nearest_neighbors(kd, lonp, latp, nn, nn_index)
  FCTEST_CHECK_EQUAL(nn_index(1), 1_c_int32_t)
  FCTEST_CHECK_EQUAL(nn_index(2), 2_c_int32_t)
  write(0,*) "nn_index(1) = ",nn_index(1)
  write(0,*) "nn_index(2) = ",nn_index(2)

  call kdtree_find_in_sphere(kd, lonp, latp, r, nnr)
  FCTEST_CHECK_EQUAL(nnr, 1_c_int32_t)
  write(0,*) "nnr = ",nnr

  call kdtree_destroy(kd)
END_TEST

! -----------------------------------------------------------------------------

END_TESTSUITE
