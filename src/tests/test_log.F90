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
  use fckit_main_module
  implicit none
  call main%init()
END_TESTSUITE_INIT

TESTSUITE_FINALIZE
  use fckit_main_module
  implicit none
  call main%final()
END_TESTSUITE_FINALIZE

! -----------------------------------------------------------------------------

TEST( test_log )
  use fckit_log_module

  call log%debug("debug")
  call log%info("info")
  call log%warning("warning")
  call log%error("error")
END_TEST

! -----------------------------------------------------------------------------

TEST( test_fortran_unit )
  use fckit_log_module
  use fckit_main_module

  call log%set_fortran_unit(unit=6,target=timestamplogtarget(),task=0)

  call log%info("FORTRAN info",newl=.true.,flush=.true.)
  call log%warning("FORTRAN warning",newl=.false.)
  call log%info("more FORTRAN info",flush=.true.)
  call log%warning(" more FORTRAN warning",flush=.true.)

END_TEST

! -----------------------------------------------------------------------------

END_TESTSUITE

