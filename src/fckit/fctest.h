#if 0
(C) Copyright 2013 ECMWF.

This software is licensed under the terms of the Apache Licence Version 2.0
which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
In applying this licence, ECMWF does not waive the privileges and immunities
granted to it by virtue of its status as an intergovernmental organisation nor
does it submit to any jurisdiction.
#endif

#if 0
// clang-format off
#endif

#ifndef FCTEST_H
#define FCTEST_H

#include "fckit/fckit.h"

! TESTSUITE macro: defines a new testsuite
!    To be closed with the END_TESTSUITE macro
#define TESTSUITE( TESTSUITE_NAME ) \
module TESTSUITE_NAME;\
use fctest;\
contains;

! TESTSUITE_WITH_FIXTURE macro: defines a new testsuite
!    with a given module as fixture. This fixture can
!    be used to import required functionality to test
!    To be closed with the END_TESTSUITE macro
#define TESTSUITE_WITH_FIXTURE( TESTSUITE_NAME, TESTSUITE_FIXTURE ) \
module TESTSUITE_NAME;\
use fctest;\
use TESTSUITE_FIXTURE;\
contains

! END_TESTSUITE macro: closes a TESTSUITE_
#define END_TESTSUITE end module

! TEST macro: define a new test within a TESTSUITE_
#define TEST( TEST_NAME ) subroutine TEST_NAME;

! END_TEST macro: closes a TEST_
#define END_TEST end subroutine;

! TESTSUITE_INIT macro: define a function to be called before any other test
#define TESTSUITE_INIT subroutine testsuite_init

! END_TESTSUITE_INIT macro: closes the TESTSUITE_INIT_ function
#define END_TESTSUITE_INIT end subroutine testsuite_init

! TESTSUITE_FINALIZE macro: define a function to be called after any other test
#define TESTSUITE_FINALISE subroutine testsuite_finalize
#define TESTSUITE_FINALIZE subroutine testsuite_finalize

! END_TESTSUITE_FINALIZE macro: closes the TESTSUITE_FINALIZE_ function
#define END_TESTSUITE_FINALIZE end subroutine testsuite_finalize

! CHECK macro: check if an expression is true, otherwise fail the test
#define CHECK( EXPR ) if(.not.(EXPR)) call ERR(__LINE__)
#define FCTEST_CHECK CHECK

! CHECK_EQUAL macro: check if 2 values are exactly equal
#define CHECK_EQUAL(V1,V2) call FCE(V1,V2,__LINE__)
#define FCTEST_CHECK_EQUAL CHECK_EQUAL

! CHECK_EQUAL macro: check if 2 REAL values are equal with a given tolerance
#define CHECK_CLOSE(V1,V2,TOL) call FCC(V1,V2,TOL,__LINE__)
#define FCTEST_CHECK_CLOSE CHECK_CLOSE

! FCTEST_ERROR macro: show error
#define FCTEST_ERROR() call ERR(__LINE__)

#if 0
// clang-format on
#endif

#endif
