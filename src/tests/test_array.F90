! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fctest.h"

TESTSUITE( array )

TEST( test_array_view1d )
  use fckit_array_module, only: array_view1d
  use, intrinsic :: iso_c_binding
  integer(c_int32_t), target :: array_int32_r2(20,10)
  integer(c_int32_t), pointer :: view(:)

  write(0,*) "test_array_view1d"
  view => array_view1d(array_int32_r2)
  FCTEST_CHECK_EQUAL( size(view), 200 )
END_TEST

TEST( test_array_stride )
  use fckit_array_module, only: array_stride, array_strides
  use, intrinsic :: iso_c_binding
  integer(c_int32_t) :: array_int32_r2(20,10)
  integer(c_int32_t) :: array_int32_r4(20,10,4,2)

  write(0,*) "test_array_stride"
  FCTEST_CHECK_EQUAL( array_stride(array_int32_r2,1), 1  )
  FCTEST_CHECK_EQUAL( array_stride(array_int32_r2,2), 20 )
  FCTEST_CHECK_EQUAL( array_strides(array_int32_r2), ([1,20]) )

  FCTEST_CHECK_EQUAL( array_stride(array_int32_r4,1), 1  )
  FCTEST_CHECK_EQUAL( array_stride(array_int32_r4,2), 20 )
  FCTEST_CHECK_EQUAL( array_stride(array_int32_r4,3), 200 )
  FCTEST_CHECK_EQUAL( array_stride(array_int32_r4,4), 800 )
  FCTEST_CHECK_EQUAL( array_strides(array_int32_r4), ([1,20,200,800]) )
END_TEST

TEST( test_array_stride_discontiguous )
  use fckit_array_module, only: array_stride, array_strides
  use, intrinsic :: iso_c_binding
  implicit none
  integer(c_int32_t), target :: array_int32_r4(20,10,4,2)

  write(0,*) "test_array_stride_discontiguous"
  FCTEST_CHECK_EQUAL( array_strides(array_int32_r4(:,5,2,:)), ([1,800]) )
  FCTEST_CHECK_EQUAL( array_strides(array_int32_r4(:,:,2,:)), ([1,20,800]) )
  FCTEST_CHECK_EQUAL( array_strides(array_int32_r4(5:5,:,2,:)), ([1,20,800]) )
  FCTEST_CHECK_EQUAL( array_strides(array_int32_r4(5,:,2,:)), ([20,800]) )
  ! Above FCTEST_CHECK_EQUAL is known to fail with NVHPC 25.9 on Linux_aarch64 (e.g. GH200)
  ! because the flag -Mnotarget_temps is not supported on Linux_aarch64 (FCKIT_HAVE_NOREPACK=0)
  if (FCKIT_HAVE_NOREPACK==0 .AND. FCTEST_LAST_CHECK_FAILED()) then
    write(0,'(A)') "WARNING: Above FCTEST_CHECK_EQUAL is known to fail because FCKIT_HAVE_NOREPACK=0"
    write(0,'(A)') "--> IGNORING THIS CHECK"
  endif
END_TEST

END_TESTSUITE
