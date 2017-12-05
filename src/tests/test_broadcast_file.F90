! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fctest.h"
#include "fckit/fckit_defines.h"

TESTSUITE( test_broadcast_file )

TESTSUITE_INIT
  use fckit_module
  call fckit_main%init()
  call fckit_log%set_fortran_unit(0)

  ! Write a json file
  if( fckit_main%taskID() == 0 ) then
    OPEN (UNIT=199 , FILE="fctest_broadcast.json", STATUS='REPLACE')
    write(199,'(A)') '{"location":{"city":"Reading","company":"ECMWF","street":"Shinfield Road"},'//&
        &          '"records":[{"age":42,"name":"Anne"},{"age":36,"name":"Bob"}]}'
    CLOSE(199)
  endif

END_TESTSUITE_INIT

TESTSUITE_FINALIZE
  use fckit_module
  call fckit_main%final()
END_TESTSUITE_FINALIZE

TEST( broadcast_file_inline )
#if 1
  use fckit_module
  implicit none
  type(fckit_mpi_comm) :: comm
  type(fckit_Configuration) :: config
  write(0,*) "~~~~~~~~~~~~~~ SCOPE BEGIN ~~~~~~~~~~~~~~~"
  comm = fckit_mpi_comm()
  config = fckit_YAMLConfiguration( comm%broadcast_file("fctest_broadcast.json",0) )
  FCTEST_CHECK( config%has("location") )
#ifndef EC_HAVE_Fortran_FINALIZATION
  call config%final()
#endif
  write(0,*) "~~~~~~~~~~~~~~~ SCOPE END ~~~~~~~~~~~~~~~~"
#else
#warning disabled
#endif
END_TEST

TEST( broadcast_file_arg )
#if 0
  use fckit_module
  implicit none
  type(fckit_mpi_comm) :: comm
  type(fckit_Configuration) :: config
  type(fckit_buffer) :: buffer
  write(0,*) "~~~~~~~~~~~~~~ SCOPE BEGIN ~~~~~~~~~~~~~~~"
  comm = fckit_mpi_comm()
  buffer = comm%broadcast_file("fctest_broadcast.json",0)
  config = fckit_YAMLConfiguration( buffer )
  FCTEST_CHECK( config%has("location") )
  FCTEST_CHECK_EQUAL( buffer%owners(), 1 )
#ifndef EC_HAVE_Fortran_FINALIZATION
  call buffer%final()
#endif
  write(0,*) "~~~~~~~~~~~~~~~ SCOPE END ~~~~~~~~~~~~~~~~"
#else
#warning disabled
#endif
END_TEST


END_TESTSUITE
