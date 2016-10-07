! (C) Copyright 1996-2016 ECMWF.
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fctest.h"

! -----------------------------------------------------------------------------

TESTSUITE(fckit_test_log)

TESTSUITE_INIT
  use fckit_runtime_module

  call main%init(output_task=0,output_unit=6,error_unit=0,output_simple=.false.)

END_TESTSUITE_INIT

! -----------------------------------------------------------------------------

TEST( test_log )
  use fckit_log_module

  call fckit_log%debug("debug")
  call fckit_log%info("info")
  call fckit_log%warning("warning")
  call fckit_log%error("error")

END_TEST

TEST( test_fortran_unit )
  use fckit_log_module

  call fckit_log%set_fortran_unit(6,2)
  call fckit_log%info("FORTRAN info",newl=.true.)
  call fckit_log%warning("FORTRAN warning",newl=.false.)
  call fckit_log%info("more FORTRAN info",flush=.true.)
  call fckit_log%warning(" more FORTRAN warning",flush=.true.)

END_TEST

! -----------------------------------------------------------------------------

END_TESTSUITE

