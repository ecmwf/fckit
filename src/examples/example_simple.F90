#include "fctest/fctest.h"

TESTSUITE( example_simple )

TEST( test1 )
  write(0,*) "test1"
  CHECK_EQUAL( 1, 1 ) ! description here
  CHECK( 1==1 ) ! description here
  CHECK_CLOSE( 0., 1.e-8, 1.e-5 ) ! description here
END_TEST

TEST( test2 )
  write(0,*) "test2"
  CHECK_EQUAL( 1, 2 ) ! description here
  CHECK( 1==2 ) ! description here
  CHECK_CLOSE( 0., 1.e-4, 1.e-5 ) ! description here
END_TEST

END_TESTSUITE
