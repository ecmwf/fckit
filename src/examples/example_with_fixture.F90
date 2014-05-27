#include "fctest/fctest.h"

module TestFixture
  implicit none
  integer, allocatable :: array(:)
end module

TESTSUITE_WITH_FIXTURE( example_with_fixture, TestFixture )

TESTSUITE_INIT
  write(0,*) "initializing testsuite"
  allocate( array(4) )
END_TESTSUITE_INIT

TESTSUITE_FINALIZE
  write(0,*) "finalizing testsuite"
  deallocate( array )
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
