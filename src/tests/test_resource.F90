! (C) Copyright 1996-2016 ECMWF.
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fctest.h"

! -----------------------------------------------------------------------------

TESTSUITE(fckit_test_resource)

TESTSUITE_INIT
  use fckit_runtime_module
  call main%init()
END_TESTSUITE_INIT
! -----------------------------------------------------------------------------

TEST( test_resource )
  use fckit_resource_module

  integer(c_int) :: intval
  integer(c_long) :: longval
  real(c_float) :: floatval
  real(c_double) :: doubleval
  character(len=:), allocatable :: stringval

  call fckit_resource("-integer",0_c_int,intval)
  FCTEST_CHECK_EQUAL(intval, 10_c_int)
  write(0,*) "integer = ",intval

  call fckit_resource("-long",0_c_long,longval)
  write(0,*) "long = ",longval
  FCTEST_CHECK_EQUAL(longval, 5000000000_c_long)

  call fckit_resource("-float",0._c_float,floatval)
  FCTEST_CHECK_EQUAL(floatval, 0.123456_c_float )
  write(0,*) "float = ",floatval

  call fckit_resource("-double",0._c_double,doubleval)
  FCTEST_CHECK_EQUAL(doubleval, 0.1234567890123456789_c_double )
  write(0,*) "double = ",doubleval

  call fckit_resource("-string","default",stringval)
  FCTEST_CHECK_EQUAL(stringval, "hello world")
  write(0,*) "string = ",stringval
END_TEST

! -----------------------------------------------------------------------------

END_TESTSUITE

