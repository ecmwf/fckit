#ifndef FCTEST_H
#define FCTEST_H

#define TESTSUITE( TESTSUITE_NAME ) \
module TESTSUITE_NAME;\
use fctest;\
implicit none;\
contains;
subroutine d;end subroutine;

#define TESTSUITE_WITH_FIXTURE( TESTSUITE_NAME, TESTSUITE_FIXTURE ) \
module TESTSUITE_NAME;\
use fctest;\
use TESTSUITE_FIXTURE;\
implicit none;\
contains;\
subroutine d;end subroutine;

#define END_TESTSUITE end module

#define TEST( TEST_NAME ) subroutine TEST_NAME;
#define END_TEST end subroutine;

#define TESTSUITE_INIT subroutine testsuite_init
#define END_TESTSUITE_INIT end subroutine testsuite_init

#define TESTSUITE_FINALISE subroutine testsuite_finalize
#define TESTSUITE_FINALIZE subroutine testsuite_finalize
#define END_TESTSUITE_FINALIZE end subroutine testsuite_finalize


#define CHECK( EXPR ) if(.not.(EXPR)) call ERR(__LINE__)
#define CHECK_EQUAL(V1,V2) call FCE(V1,V2,__LINE__)
#define CHECK_CLOSE(V1,V2,TOL) call FCC(V1,V2,TOL,__LINE__)

#endif
