! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fctest.h"

TESTSUITE( array )

TEST( test_array_view1d )
  use fckit_array_module, only: array_view1d
  use, intrinsic :: iso_c_binding
  integer(c_int) :: array_int32_r2(20,10)
  integer(c_int), pointer :: view(:)

  write(0,*) "test_array_view1d"
  view => array_view1d(array_int32_r2)
  FCTEST_CHECK_EQUAL( size(view), 200 )
END_TEST

TEST( test_array_stride )
  use fckit_array_module, only: array_stride, array_strides
  use, intrinsic :: iso_c_binding
  integer(c_int) :: array_int32_r2(20,10)

  write(0,*) "test_array_stride"
  FCTEST_CHECK_EQUAL( array_stride(array_int32_r2,1), 1  )
  FCTEST_CHECK_EQUAL( array_stride(array_int32_r2,2), 20 )

  FCTEST_CHECK_EQUAL( array_strides(array_int32_r2), ([1,20]) )

END_TEST

END_TESTSUITE
