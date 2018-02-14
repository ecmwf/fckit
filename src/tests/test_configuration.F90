! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


! This File contains Unit Tests for testing the
! C++ / Fortran Interfaces to the Mesh Datastructure
! @author Willem Deconinck

#include "fckit/fctest.h"

#define NO_COMPILER_BUGS 1

#if (defined(__GFORTRAN__) && __GNUC__ == 7 && __GNUC_MINOR__ <=3 )
#undef NO_COMPILER_BUGS
#define NO_COMPILER_BUGS 0
#warning Some tests disabled due to gfortran 7 compiler bug. Only one TEST at a time may be compiled.
#endif

! -----------------------------------------------------------------------------

TESTSUITE(fctest_fckit_configuration)

! -----------------------------------------------------------------------------

TESTSUITE_INIT
  use fckit_module
  call fckit_main%init()
END_TESTSUITE_INIT

! -----------------------------------------------------------------------------

TESTSUITE_FINALIZE
  use fckit_module
  call fckit_main%final()
END_TESTSUITE_FINALIZE

! -----------------------------------------------------------------------------

TEST( test_configuration )
#if 1
  use fckit_configuration_module
  use fckit_log_module

  type(fckit_Configuration) :: config
  type(fckit_Configuration) :: nested
  type(fckit_Configuration) :: list(2)
  logical :: found
  integer :: intval
  integer :: j

  type(fckit_Configuration) :: anested
  type(fckit_Configuration), allocatable :: alist(:)

  write(0,*) "~~~~~~~~~~~~~~ SCOPE BEGIN ~~~~~~~~~~~~~~~"

  ! --------------------- SET ------------------

  ! Corresponding YAML:
  ! {
  !   nested:
  !     list:
  !       -
  !         l1: 21
  !         l2: 22
  !       -
  !         l1: 21
  !         l2: 22
  !     n1: 11
  !     n2: 12
  !   p1: 1
  !   p2: 2
  ! }

  config = fckit_Configuration()


  call config%set("p1",1)
  call config%set("p2",2)

  nested = fckit_Configuration()
  call nested%set("n1",11)
  call nested%set("n2",12)

  do j=1,2
    list(j) = fckit_Configuration()
    call list(j)%set("l1",21)
    call list(j)%set("l2",22)
  enddo
  call nested%set("list",list)

#if ! FCKIT_HAVE_FINAL || FCKIT_FINAL_BROKEN_FOR_AUTOMATIC_ARRAY
do j=1,2
  call list(j)%final()
enddo
#endif
  call config%set("nested",nested)
#if ! FCKIT_HAVE_FINAL
  call nested%final()
#endif

  ! --------------------- JSON ------------------

  call fckit_log%info("config = "//config%json())

  FCTEST_CHECK_EQUAL( config%owners(), 1 )


  ! --------------------- GET ------------------

  found = config%get("p1",intval)
  FCTEST_CHECK( found )
  FCTEST_CHECK_EQUAL( intval , 1 )

  found = config%get("nested",anested)
  FCTEST_CHECK( found )
  found = anested%get("n1",intval)
  FCTEST_CHECK( found )
  FCTEST_CHECK_EQUAL(intval, 11)

  found = anested%get("list",alist)
  FCTEST_CHECK( found )
  FCTEST_CHECK_EQUAL( size(alist), 2 )

  found = alist(1)%get("l1",intval)
  FCTEST_CHECK( found )
  FCTEST_CHECK_EQUAL(intval, 21)
  found = alist(1)%get("l2",intval)
  FCTEST_CHECK( found )
  FCTEST_CHECK_EQUAL(intval, 22)






  found = alist(2)%get("l1",intval)
  FCTEST_CHECK( found )
  FCTEST_CHECK_EQUAL(intval, 21)
  found = alist(2)%get("l2",intval)
  FCTEST_CHECK( found )
  FCTEST_CHECK_EQUAL(intval, 22)

  write(0,*) "deallocate alist..."
#if ! FCKIT_HAVE_FINAL || FCKIT_FINAL_BROKEN_FOR_ALLOCATABLE_ARRAY
  write(0,*) "  + deallocate_fckit_configuration(alist)"
  call deallocate_fckit_configuration(alist)
#else
  write(0,*) "  + deallocate(alist)"
  deallocate(alist)
#endif
  write(0,*) "deallocate alist... done"

  call anested%final()

  ! There is a reported PGI/16.7 bug that makes this test segfault here.
  ! PGI/17.1 has this bug fixed.

  ! ---------------------------------------------

  write(0,*) "config%owners() = ", config%owners()

#if ! FCKIT_HAVE_FINAL
  call config%final()
#endif

  write(0,*) "~~~~~~~~~~~~~~~ SCOPE END ~~~~~~~~~~~~~~~~"

#else
#warning Test "test_configuration" disabled
#endif
END_TEST

! -----------------------------------------------------------------------------

TEST(test_configuration_json_string)
#if 1
#if NO_COMPILER_BUGS
  use fckit_configuration_module
  use fckit_log_module

  type(fckit_Configuration) :: config
  type(fckit_Configuration), allocatable :: records(:)
  character (len=:), allocatable :: name
  character (len=:), allocatable :: json
  character(len=1024) :: msg
  integer :: age
  integer :: jrec

  write(0,*) "~~~~~~~~~~~~~~ SCOPE BEGIN ~~~~~~~~~~~~~~~"

  json='{"records":['//&
   &       '{"name":"Joe",   "age":30},'//&
   &       '{"name":"Alison","age":43}' //&
   &    ']}'

  config = fckit_YAMLConfiguration(json)

  call fckit_log%info(config%json())
  if( config%get("records",records) ) then
    do jrec=1,size(records)
      FCTEST_CHECK( records(jrec)%get("name",name) )
      FCTEST_CHECK( records(jrec)%get("age",age)   )
      write(msg,'(2A,I0,A)') name," is ",age," years old"; call fckit_log%info(msg)
   enddo

   write(0,*) "deallocate records..."
#if ! FCKIT_HAVE_FINAL || FCKIT_FINAL_BROKEN_FOR_ALLOCATABLE_ARRAY
    call deallocate_fckit_configuration(records)
#else
    if( allocated(records) ) deallocate(records)
#endif
    write(0,*) "deallocate records... done"
  endif
  write(0,*) "config%owners() = ", config%owners()

#if ! FCKIT_HAVE_FINAL
  call config%final()
#endif

  write(0,*) "~~~~~~~~~~~~~~~ SCOPE END ~~~~~~~~~~~~~~~~"
#endif
#else
#warning Test "test_configuration_json_string" disabled
#endif
END_TEST

TEST(test_configuration_json_file)
#if 1
#if NO_COMPILER_BUGS
  use fckit_configuration_module
  use fckit_pathname_module
  use fckit_log_module

  type(fckit_Configuration) :: config
  type(fckit_Configuration), allocatable :: records(:)
  type(fckit_Configuration) :: location
  character (len=:), allocatable :: name, company, street, city
  integer :: age
  integer :: jrec
  character(len=1024) :: msg

  write(0,*) "~~~~~~~~~~~~~~ SCOPE BEGIN ~~~~~~~~~~~~~~~"


  ! Write a json file
  OPEN (UNIT=9 , FILE="fctest_configuration.json", STATUS='REPLACE')
  write(9,'(A)') '{"location":{"city":"Reading","company":"ECMWF","street":"Shinfield Road"},'//&
  &'"records":[{"age":42,"name":"Anne"},{"age":36,"name":"Bob"}]}'
  CLOSE(9)

  config = fckit_YAMLConfiguration( fckit_PathName("fctest_configuration.json") )

  call fckit_log%info("config = "//config%json(),flush=.true.)

  if( config%get("records",records) ) then
    do jrec=1,size(records)
      FCTEST_CHECK( records(jrec)%get("name",name) )
      FCTEST_CHECK( records(jrec)%get("age",age)   )
      write(msg,'(2A,I0,A)') name," is ",age," years old"; call fckit_log%info(msg)
    enddo
    write(0,*) "deallocate records..."
#if ! FCKIT_HAVE_FINAL || FCKIT_FINAL_BROKEN_FOR_ALLOCATABLE_ARRAY
    call deallocate_fckit_configuration(records)
#else
    deallocate(records)
#endif
     write(0,*) "deallocate records... done"
  endif
  if( config%get("location",location) ) then
    call fckit_log%info("location = "//location%json(),flush=.true.)

    if( location%get("company",company) ) then
      write(0,*) "company = ",company
    endif
    if( location%get("street",street) ) then
      write(0,*) "street = ",street
    endif
    if( location%get("city",city) ) then
      write(0,*) "city = ",city
    endif
#if ! FCKIT_HAVE_FINAL
    call location%final()
#endif
  endif
  write(0,*) "config%owners() = ", config%owners()
#if ! FCKIT_HAVE_FINAL
  call config%final()
#endif
  write(0,*) "~~~~~~~~~~~~~~~ SCOPE END ~~~~~~~~~~~~~~~~"
#endif
#else
#warning Test "test_configuration_json_file" disabled
#endif
END_TEST

TEST(test_throw)
!! ENABLE TO TEST IF THROW WILL WORK

#if 0
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

