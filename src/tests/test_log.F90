! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


#include "fckit/fctest.h"

! -----------------------------------------------------------------------------

TESTSUITE(fckit_test_log)

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

TEST( test_main )
  use fckit_module
  character(len=:), allocatable :: displayname, name
  character(len=128) :: logmsg
  call fckit_main%name(name)
  call fckit_main%displayname(displayname)
  write(logmsg,*) "name = "//name//" , displayname = "//displayname
  call fckit_log%info(logmsg)
END_TEST

! -----------------------------------------------------------------------------

TEST( test_log )
  use fckit_module, only: log => fckit_log

  call log%set_stdout()

  call log%debug("debug")
  call log%info("info")
  call log%warning("warning")
  call log%error("error")
  call log%error("   ")
  call log%error("hello again")

END_TEST

! -----------------------------------------------------------------------------

TEST( test_file )
  use fckit_module, only: log => fckit_log

  call log%add_file("output_file",style=log%SIMPLE)

  call log%info("FILE info",newl=.true.,flush=.true.)
  call log%info("more FILE info",flush=.true.)
  call log%warning(" more FILE warning",flush=.true.)

END_TEST

! -----------------------------------------------------------------------------

TEST( test_fortran_unit )
  use fckit_module, only: log => fckit_log

  call log%set_fortran_unit(unit=6,style=log%TIMESTAMP)

  call log%info("FORTRAN info",newl=.true.,flush=.true.)
  call log%warning("FORTRAN warning",newl=.false.)
  call log%warning(" more FORTRAN warning",flush=.true.)
  call log%info("more FORTRAN info",flush=.true.)

END_TEST

! -----------------------------------------------------------------------------

END_TESTSUITE

