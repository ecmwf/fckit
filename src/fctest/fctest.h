
#define TESTSUITE( TESTSUITE_NAME ) \
module TESTSUITE_NAME;\
implicit none;\
integer :: exit_status;\
contains;

#define TESTSUITE_WITH_FIXTURE( TESTSUITE_NAME, TESTSUITE_FIXTURE ) \
module TESTSUITE_NAME;\
use TESTSUITE_FIXTURE;\
implicit none;\
integer :: exit_status;\
contains;

#define TEST( TEST_NAME ) subroutine TEST_NAME;

#define END_TEST end subroutine;

#define END_TESTSUITE end module

#define TESTSUITE_INIT subroutine testsuite_init

#define END_TESTSUITE_INIT end subroutine testsuite_init

#define TESTSUITE_FINALISE subroutine testsuite_finalize
#define TESTSUITE_FINALIZE subroutine testsuite_finalize
  
#define END_TESTSUITE_FINALIZE end subroutine testsuite_finalize


#define CHECK_EQUAL(V1,V2)\
if( .not. (V1 == V2) ) then;\
  write(0,'(7A,I3)') "CHECK_EQUAL(",#V1,",",#V2,") failed in ",__FILE__,":",__LINE__;\
  write(0,*) "[",V1,"!=",V2,"]";\
  exit_status = 1;\
endif
    
#define CHECK( EXPR )\
if( .not. (EXPR) ) then;\
  write(0,'(5A,I3)') "CHECK(",#EXPR,") failed in ",__FILE__,":",__LINE__;\
  exit_status = 1;\
endif
      
#define CHECK_CLOSE(V1,V2,ABS_TOL)\
if( .not. (abs(V1-V2)<=ABS_TOL) ) then;\
  write(0,'(9A,I3)') "CHECK_CLOSE(",#V1,",",#V2,",",#ABS_TOL,") failed in ",__FILE__,":",__LINE__;\
  write(0,*) "[",V1,"!=",V2,"]";\
  exit_status = 1;\
endif