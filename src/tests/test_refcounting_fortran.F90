! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.



#include "fckit/fctest.h"

! -----------------------------------------------------------------------------

module fcta_refcounting_f_fxt
use fckit_refcounted_module, only : fckit__new_Owned, fckit__delete_Owned
use fckit_refcounted_fortran_module
use fckit_c_interop_module
use fctest
use iso_c_binding
implicit none

type :: payload_t
  integer :: id
contains
endtype

type, extends(fckit_refcounted_fortran) :: RefObj
  type(payload_t), pointer :: payload
  integer :: uid = -1
contains
  procedure :: delete => RefObj__delete
  procedure :: copy => RefObj__copy
  procedure :: id

#ifdef EC_HAVE_Fortran_FINALIZATION
  final :: RefObj__final
#endif
endtype

interface RefObj
  module procedure RefObj__constructor
end interface

contains

subroutine RefObj__final(this)
  type(RefObj) :: this
  write(0,*) '>>>>>>>>>>>>>>> RefObj__final uid',this%uid
  call this%final()
  write(0,*) '<<<<<<<<<<<<<<< RefObj__final uid',this%uid
end subroutine

function id(this)
  integer :: id
  class(RefObj), intent(in) :: this
  id = this%payload%id
end function

function create_obj(id) result(obj)
  type(RefObj) :: obj
  integer :: id
  obj%uid = -2
  obj = RefObj(id)
  write(0,*) "-------------1 owners",obj%owners()
  call obj%return()
  write(0,*) "-------------2 owners",obj%owners()
  write(0,*) "-------------exiting scope create_obj"
  obj%uid = 2
end function


function create_obj_2(id) result(obj)
  type(RefObj) :: obj
  integer :: id
  obj%uid = -3
  obj = create_obj(id)
  write(0,*) "-------------3 owners",obj%owners()
  call obj%return()
  write(0,*) "-------------4 owners",obj%owners()
  write(0,*) "-------------exiting scope create_obj_w"
end function

function RefObj__constructor(id) result(this)
  type(RefObj) :: this
  integer, intent(in) :: id
  write(0,*) "constructing obj ", id
!  call this%reset_c_ptr( fckit__new_Owned() )
  write(0,*) "                    ptr = ",c_ptr_to_loc(this%c_ptr())
  allocate( this%payload )
  this%payload%id = id
  this%uid = id
  call this%return()
end function

#if 1
subroutine RefObj__delete(this)
  class(RefObj), intent(inout) :: this
  write(0,*) "deleting obj",this%id()
  deallocate(this%payload)
!  call fckit__delete_Owned(this%c_ptr())
end subroutine
#endif


subroutine RefObj__copy(this,obj_in)
  class(RefObj), intent(inout) :: this
  class(fckit_refcounted_fortran), target, intent(in) :: obj_in
  class(refObj), pointer :: obj_in_cast

  select type (obj_in)
    type is (RefObj)
      obj_in_cast => obj_in
  end select
  write(0,'(A,I0,A,I0,A,I0)') "copy obj ",obj_in_cast%id(),' with uid ',obj_in_cast%uid,' into ',this%uid
  this%payload => obj_in_cast%payload
end subroutine

subroutine consume_obj(obj)
  class(RefObj) :: obj
  call obj%attach()
  write(0,*) "Consume obj",obj%id(),"  owners =",obj%owners() !,obj%id
  call obj%detach()
end subroutine

end module fcta_refcounting_f_fxt

! -----------------------------------------------------------------------------

TESTSUITE_WITH_FIXTURE(fckit_test_refcounting_f,fcta_refcounting_f_fxt)

! -----------------------------------------------------------------------------

TESTSUITE_INIT
END_TESTSUITE_INIT

! -----------------------------------------------------------------------------

TESTSUITE_FINALIZE
END_TESTSUITE_FINALIZE

! -----------------------------------------------------------------------------

TEST( test_ref )
  use fckit_c_interop_module
  type(RefObj) :: obj, bjo

#ifdef EC_HAVE_Fortran_FINALIZATION
  write(0,*) "Fortran supports automatic finalization!"
#endif

  obj = RefObj(1)
  FCTEST_CHECK_EQUAL( obj%owners(), 1 )
  FCTEST_CHECK_EQUAL( obj%id(), 1 )

  call consume_obj(obj)
  FCTEST_CHECK_EQUAL( obj%owners(), 1 )

  !call obj%final() will be done in next statement upon assignment(=)
  write(0,*) "=============create/assign..."
  obj%uid = -3
  obj = create_obj(3)
  write(0,*) "owners",obj%owners()
  write(0,*) "=============create/assign... done"
  FCTEST_CHECK_EQUAL( obj%id(), 3 )
  FCTEST_CHECK_EQUAL( obj%owners(), 1 )
  bjo = obj
  FCTEST_CHECK_EQUAL( obj%owners(), 2 )
  obj = bjo
  FCTEST_CHECK_EQUAL( obj%owners(), 2 )

#ifndef EC_HAVE_Fortran_FINALIZATION
  call obj%final()
#endif
  call consume_obj(bjo)
#ifndef EC_HAVE_Fortran_FINALIZATION
  call bjo%final()
#endif

END_TEST

! -----------------------------------------------------------------------------

END_TESTSUITE

