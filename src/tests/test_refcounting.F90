! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


#include "fckit/fctest.h"
#include "fckit/fckit_defines.h"

! -----------------------------------------------------------------------------

module fcta_refcounting_fxt
use fckit_refcounted_module
use fckit_c_interop_module
use fctest
use iso_c_binding
implicit none

integer, SAVE :: deleted = 0

type :: payload_t
  integer :: id
contains
endtype

type, extends(fckit_refcounted) :: RefObj
  type(payload_t), pointer :: payload => null()
contains
  procedure, public :: delete => RefObj__delete
  procedure, public :: copy   => RefObj__copy
  procedure :: id

#if  EC_HAVE_Fortran_FINALIZATION
  final :: RefObj__final_auto
#endif

endtype

interface RefObj
  module procedure RefObj__constructor
end interface

contains

function id(this)
  integer :: id
  class(RefObj), intent(in) :: this
  if( associated( this%payload ) ) then
    id = this%payload%id
  else
    id = -1
  endif
end function

function create_obj(id) result(anobj)
  type(RefObj) :: anobj
  integer :: id
  write(0,'(A,I0,A)') ">>>  anobj = RefObj(",id,")"
  anobj = RefObj(id)
  write(0,'(A,I0,A)') "<<<  anobj = RefObj(",id,")"
  call anobj%return()
  write(0,*) "create_obj --> owners = ",anobj%owners()
end function

function RefObj__constructor(id) result(this)
  type(RefObj) :: this
  integer, intent(in) :: id
  write(0,*)''
  call this%reset_c_ptr( fckit__new_Owned() )
  write(0,*) "constructing obj ", id, " ptr = ",c_ptr_to_loc(this%c_ptr())
  allocate( this%payload )
  this%payload%id = id
  call this%return()
  write(0,*) "RefObj__constructor --> owners = ",this%owners()
end function

subroutine RefObj__final_auto(this)
  type(RefObj) :: this
  write(0,*) "RefObj__final_auto"
  if( this%id() >= 0 ) then
    write(0,*) "final obj",this%id(), " starting with owners = ",this%owners()
  else
    write(0,*) "final obj",this%id(), " (uninitialized) "
  endif
#if Fortran_FINAL_NOT_PROPAGATING
  call this%final()
#endif
end subroutine

#if 1
subroutine RefObj__delete(this)
  class(RefObj), intent(inout) :: this
  write(0,*) "deleting obj",this%id(), " ptr = ", c_ptr_to_loc(this%c_ptr())
  deallocate(this%payload)
  call fckit__delete_Owned(this%c_ptr())
  deleted = deleted + 1
end subroutine
#endif


subroutine RefObj__copy(this,obj_in)
  class(RefObj), intent(inout) :: this
  class(fckit_refcounted), target, intent(in) :: obj_in
  class(refObj), pointer :: obj_in_cast

  select type (obj_in)
    type is (RefObj)
      obj_in_cast => obj_in
  end select
  write(0,*) "copy obj ",obj_in_cast%id()," to ",this%id()
  this%payload => obj_in_cast%payload
  write(0,*) "   check: obj is now ", this%id()
end subroutine

subroutine consume_obj(obj)
  class(RefObj), intent(in) :: obj
  call obj%attach()
  write(0,*) "Consume obj",obj%id(),"  owners =",obj%owners() !,obj%id
  call obj%detach()
end subroutine

end module fcta_refcounting_fxt

! -----------------------------------------------------------------------------

TESTSUITE_WITH_FIXTURE(fckit_test_refcounting,fcta_refcounting_fxt)

! -----------------------------------------------------------------------------

TESTSUITE_INIT
END_TESTSUITE_INIT

! -----------------------------------------------------------------------------

TESTSUITE_FINALIZE
END_TESTSUITE_FINALIZE

! -----------------------------------------------------------------------------

TEST( test_ref )
  use fckit_c_interop_module
  type(RefObj) :: obj1, bjo, obj2

#if EC_HAVE_Fortran_FINALIZATION
  write(0,*) "Fortran supports automatic finalization!"
#endif

  FCTEST_CHECK_EQUAL( obj1%id(), -1 )

  obj1 = RefObj(1)
  FCTEST_CHECK_EQUAL( obj1%owners(), 1 )
  FCTEST_CHECK_EQUAL( obj1%id(), 1 )

  write(0,*) ">>> consume_obj"
  call consume_obj(obj1)
  write(0,*) "<<< consume_obj"

  FCTEST_CHECK_EQUAL( obj1%owners(), 1 )

  !call obj%final() will be done in next statement upon assignment(=)
  write(0,*) ">>> obj2 = create_obj(3)"
  obj2 = create_obj(3)
  write(0,*) "<<< obj2 = create_obj(3)"

  FCTEST_CHECK_EQUAL( obj2%owners(), 1 )

  FCTEST_CHECK( associated(obj2%payload) )
  FCTEST_CHECK_EQUAL( obj2%id(), 3 )
  FCTEST_CHECK_EQUAL( obj2%owners(), 1 )

write(0,*) " >>> bjo=obj2"
  bjo = obj2
write(0,*) " <<< bjo=obj2"
  FCTEST_CHECK_EQUAL( obj2%owners(), 2 )
! no-op
!  obj2 = bjo
  FCTEST_CHECK_EQUAL( obj2%owners(), 2 )

  FCTEST_CHECK_EQUAL( obj1%owners(), 1 )

#if ! EC_HAVE_Fortran_FINALIZATION
  call obj1%final()
#else
  write(0,*) "Trust automatic finalisation to delete obj1 when scope ends"
#endif
  call consume_obj(bjo)
#if ! EC_HAVE_Fortran_FINALIZATION
  call bjo%final()
#else
  write(0,*) "Trust automatic finalisation to delete bjo when scope ends"
#endif
#if ! EC_HAVE_Fortran_FINALIZATION
  call obj2%final()
#else
  write(0,*) "Trust automatic finalisation to delete obj2 when scope ends"
#endif


END_TEST

TEST( test_deleted )
#if ! EC_HAVE_Fortran_FINALIZATION
  FCTEST_CHECK_EQUAL( deleted , 2 )
#else
  write(0,*) "WARNING: baseclass fckit_refcounted%delete() is called instead of RefObj%delete()"
#endif
END_TEST

! -----------------------------------------------------------------------------

END_TESTSUITE

