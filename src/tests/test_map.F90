#include "fckit/fctest.h"

! -----------------------------------------------------------------------------

TESTSUITE(fctest_test_map_suite)

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




TEST( test_map )
#if 1

use iso_c_binding, only : c_float, c_double
use fckit_map_module

type(fckit_map) :: tmap

tmap = fckit_map()

#else
#warning Test "test_map" disabled
#endif
END_TEST



TEST( test_map_insert )
#if 1

use iso_c_binding, only : c_float, c_double, c_int
use fckit_map_module
use fckit_configuration_module

type(fckit_map) :: tmap1

integer(c_int)  :: ivalue = 999
real(c_float)   :: fvalue = 1.11
real(c_double)  :: dvalue = 2.22

type(fckit_configuration) :: test_config
type(fckit_configuration) :: retrieved_config
logical :: found
logical :: logval
integer :: intval


test_config = fckit_Configuration()
call test_config%set("int1",1)
call test_config%set("int2",2)
call test_config%set("logical_true",.True.)
call test_config%set("logical_false",.False.)



tmap1 = fckit_map()

call tmap1%insert("ivalue", ivalue)
call tmap1%insert("fvalue", fvalue)
call tmap1%insert("dvalue", dvalue)
call tmap1%insert("config_ptr", test_config%c_ptr())


! must contain
FCTEST_CHECK(tmap1%has("ivalue"))
FCTEST_CHECK(tmap1%has("fvalue"))
FCTEST_CHECK(tmap1%has("dvalue"))
FCTEST_CHECK(tmap1%has("config_ptr"))

! must not contain
FCTEST_CHECK_EQUAL(tmap1%has("non-existent-key"),.false.)

! check size
FCTEST_CHECK_EQUAL(tmap1%size(),4)

! get values..
FCTEST_CHECK_EQUAL(tmap1%get_int32("ivalue"), 999)
FCTEST_CHECK_EQUAL(tmap1%get_real32("fvalue"), fvalue)
FCTEST_CHECK_EQUAL(tmap1%get_real64("dvalue"), dvalue)

call retrieved_config%reset_c_ptr( tmap1%get_c_ptr("config_ptr") )

found = retrieved_config%get("int1",intval)
FCTEST_CHECK( found )
FCTEST_CHECK_EQUAL( intval , 1 )

found = retrieved_config%get("int2",intval)
FCTEST_CHECK( found )
FCTEST_CHECK_EQUAL( intval , 2 )

found = retrieved_config%get("logical_true",logval)
FCTEST_CHECK( found )
FCTEST_CHECK_EQUAL( logval , .true. )

found = retrieved_config%get("logical_false",logval)
FCTEST_CHECK( found )
FCTEST_CHECK_EQUAL( logval , .false. )

#else
#warning Test "test_map_insert" disabled
#endif
END_TEST






TEST( test_map_of_tensor )
#if 1

use iso_c_binding, only : c_float
use fckit_map_module
use fckit_tensor_module, only : fckit_tensor_real32

type(fckit_map)    :: tensor_map

real(c_float)      :: data_rank2(3,4)
type(fckit_tensor_real32) :: tensor_rank2
type(fckit_tensor_real32) :: tensor_rank2_retrieved
type(fckit_tensor_real32) :: tensor_rank2_retrieved_ptr2ctor
type(fckit_tensor_real32) :: tensor_rank2_retrieved_sub

integer(c_int32_t), allocatable :: tensor_shape(:)



! construct a map
tensor_map = fckit_map()

! tensor from an array rank 2
data_rank2(1,:) = 0.0
data_rank2(2,:) = 1.0
data_rank2(3,:) = 2.0
tensor_rank2 = fckit_tensor_real32(data_rank2)
FCTEST_CHECK_EQUAL( tensor_rank2%size(), 3*4)
FCTEST_CHECK_EQUAL( tensor_rank2%rank(), 2)

allocate(tensor_shape(tensor_rank2%rank()))
tensor_shape = tensor_rank2%shape()
FCTEST_CHECK_EQUAL( size(tensor_shape), 2)
FCTEST_CHECK_EQUAL( tensor_shape(1) , 3 )
FCTEST_CHECK_EQUAL( tensor_shape(2) , 4 )
deallocate(tensor_shape)

! insert the tensor into the map
call tensor_map%insert("tensor_rank2_ptr", tensor_rank2%c_ptr())

! map must contain the key
FCTEST_CHECK(tensor_map%has("tensor_rank2_ptr"))

! get the c_ptr
call tensor_rank2_retrieved%reset_c_ptr( tensor_map%get_c_ptr("tensor_rank2_ptr") )
FCTEST_CHECK_EQUAL( tensor_rank2%size() , tensor_rank2_retrieved%size() )
FCTEST_CHECK_EQUAL( tensor_rank2%rank() , tensor_rank2_retrieved%rank() )

! try to construct from pointer
tensor_rank2_retrieved_ptr2ctor = fckit_tensor_real32( tensor_map%get_c_ptr("tensor_rank2_ptr") )
FCTEST_CHECK_EQUAL( tensor_rank2%size() , tensor_rank2_retrieved_ptr2ctor%size() )
FCTEST_CHECK_EQUAL( tensor_rank2%rank() , tensor_rank2_retrieved_ptr2ctor%rank() )

! retrieve directly the fckit_shared_obj
call tensor_map%get("tensor_rank2_ptr", tensor_rank2_retrieved_sub)
FCTEST_CHECK_EQUAL( tensor_rank2%size() , tensor_rank2_retrieved_sub%size() )
FCTEST_CHECK_EQUAL( tensor_rank2%rank() , tensor_rank2_retrieved_sub%rank() )


allocate(tensor_shape(tensor_rank2_retrieved%rank()))
tensor_shape = tensor_rank2_retrieved%shape()
FCTEST_CHECK_EQUAL( size(tensor_shape), 2)
FCTEST_CHECK_EQUAL( tensor_shape(1) , 3 )
FCTEST_CHECK_EQUAL( tensor_shape(2) , 4 )
deallocate(tensor_shape)

#else
#warning Test "test_map_of_tensor" disabled
#endif
END_TEST












END_TESTSUITE