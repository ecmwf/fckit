#include "fctest/fctest.h"

TESTSUITE( example_simple )

TEST( test1 )
  write(0,*) "test1"
  CHECK_EQUAL( 1, 1 )
  CHECK( 1==1 )
  CHECK_CLOSE( 0., 1.e-8, 1.e-5 )
END_TEST

TEST( test2 )
  write(0,*) "test2"
  CHECK_EQUAL( 1, 2 )
  CHECK( 1==2 )
  CHECK_CLOSE( 0., 1.e-4, 1.e-5 )
END_TEST

END_TESTSUITE
