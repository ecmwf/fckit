! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fctest.h"

TESTSUITE( fctest_example_simple )

TEST( test1 )
  write(0,*) "test1"
  CHECK_EQUAL( 1, 1 ) ! description here
  CHECK( 1==1 ) ! description here
  CHECK_CLOSE( 0., 1.e-8, 1.e-5 ) ! description here
  CHECK_EQUAL( (/1,2/), (/2,3/) )
  CHECK_EQUAL( (/1._sp,2._sp/),   (/1._sp,2._sp,3._sp/) )
  CHECK_CLOSE( (/1._dp,2.01_dp/), (/1._dp,2._dp/), 0.001_dp )
END_TEST

TEST( test2 )
  write(0,*) "test2"
  CHECK_EQUAL( 1, 2 ) ! description here
  CHECK( 1==2 ) ! description here
  CHECK_CLOSE( 0., 1.e-4, 1.e-5 ) ! description here
  CHECK_EQUAL( (/1._sp,2._sp/), (/1._sp,2._sp/) )
  CHECK_CLOSE( (/1._dp,2.0001_dp/), (/1._dp,2._dp/), 0.001_dp )

END_TEST

TEST( test3 )
 use example_module, only: example_function
 write(0,*) "test3"
 CHECK_EQUAL( example_function(), 1 )
END_TEST

TESTSUITE_FINALIZE
  ! Mark test as passed, as it was supposed to fail otherwise
  exit_status = 0
END_TESTSUITE_FINALIZE

END_TESTSUITE
