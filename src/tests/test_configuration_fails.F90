! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fctest.h"

! -----------------------------------------------------------------------------

TESTSUITE(fctest_fckit_configuration_fails)

! -----------------------------------------------------------------------------

TESTSUITE_INIT
  use fckit_module
  call fckit_main%init()

  ! With Apple Silicon (M1), using Apple clang 12.0.0 for C++ and Homebrew GCC 10.2.0_4 for Fortran
  ! it appears that the Fortran needs to trap the C++ exceptions via SIGTRAP
  call fckit_signal%set_handler( fckit_signal%SIGTRAP() )
  call fckit_signal%set_handler( fckit_signal%SIGBUS() )
END_TESTSUITE_INIT

! -----------------------------------------------------------------------------

TESTSUITE_FINALIZE
  use fckit_module
  call fckit_main%final()
END_TESTSUITE_FINALIZE

! -----------------------------------------------------------------------------

TEST(test_throw)
!! ENABLE TO TEST IF THROW WILL WORK
#if 1
  use fckit_configuration_module
  type(fckit_Configuration) :: config

  integer :: missing_value

  write(0,*) "~~~~~~~~~~~~~~ SCOPE BEGIN ~~~~~~~~~~~~~~~"

  config = fckit_Configuration()

  call config%get_or_die("missing",missing_value)

  call config%final()
  write(0,*) "~~~~~~~~~~~~~~~ SCOPE END ~~~~~~~~~~~~~~~~"
#endif
END_TEST
! -----------------------------------------------------------------------------

END_TESTSUITE

