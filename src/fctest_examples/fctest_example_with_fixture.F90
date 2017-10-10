! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


#include "fckit/fctest.h"

module fctest_TestFixture
  implicit none
  integer, allocatable :: array(:)
end module

TESTSUITE_WITH_FIXTURE( fctest_example_with_fixture, fctest_TestFixture )

TESTSUITE_INIT
  write(0,*) "initializing testsuite"
  allocate( array(4) )
END_TESTSUITE_INIT

TESTSUITE_FINALIZE
  write(0,*) "finalizing testsuite"
  deallocate( array )
  ! Mark test as passed, as it was supposed to fail otherwise
  exit_status = 0
END_TESTSUITE_FINALIZE



TEST( test1 )
  write(0,*) "test1"
  CHECK_EQUAL( size(array), 4 ) ! description here
END_TEST

TEST( test2 )
  write(0,*) "test2"
  CHECK_EQUAL( size(array), 2 ) ! description here
END_TEST

END_TESTSUITE
