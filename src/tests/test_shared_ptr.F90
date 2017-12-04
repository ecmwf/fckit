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

module fcta_shared_ptr_f_fxt
use fckit_object_module
use fckit_final_module
use fckit_shared_ptr_module
use fckit_c_interop_module
use fctest
use iso_c_binding
implicit none

! -----------------------------------------------------------------------------

!!> Object that is to be shared
type, extends(fckit_final) :: Object_shared
  integer :: uid = -1
    !! Data of Object (could be anything, like allocatable pointers)
contains
  procedure :: final => Object_final
    !! Destructor of the data (e.g. if deallocations need to happen)
endtype

!!> Smart shared pointer which delegates functionality
type, extends(fckit_shared_ptr) :: Object
  class(Object_shared), pointer, private :: shared => null()
    !! Pointer to data which is shared
contains
  procedure :: shared_ptr_cast
    !! Obligatory function to cast "shared" pointer
  procedure :: id   => Object_id
    !! Member function, which works on the data
endtype

!!> Constructors
interface Object
  module procedure Object_constructor
end interface

! -----------------------------------------------------------------------------

type :: Unrelated_ptr
contains
  final :: Unrelated_ptr_final
endtype

type, extends(fckit_shared_ptr) :: Unrelated
  class(Unrelated_ptr), pointer, private :: data
contains
  procedure :: shared_ptr_cast => Unrelated_shared_ptr_cast
endtype

!!> Constructors
interface Unrelated
  module procedure Unrelated_constructor
end interface

! -----------------------------------------------------------------------------
contains
! -----------------------------------------------------------------------------

function Object_constructor(id) result(this)
  type(Object) :: this
  integer :: id
  allocate( Object_shared::this%shared )
  write(0,*) __LINE__
  call this%make_shared(this%shared)
  write(0,*) __LINE__
  this%shared%uid = id
  write(0,*) __LINE__
  call this%return()
  write(0,*) __LINE__
  write(0,*) "uid =" , this%id()
end function

subroutine Object_final(this)
  class(Object_shared), intent(inout) :: this
  write(0,*) '>>>>>>>>>> Object_shared__final uid',this%uid
  this%uid = 0
end subroutine

subroutine Object_final_auto(this)
  type(Object_shared), intent(inout) :: this
  write(0,*) '>>>>> Object_shared__final_auto uid',this%uid
  call this%final()
end subroutine

function shared_ptr_cast(this) result(success)
#define DERIVED_TYPE Object_shared
#define SHARED_PTR_TYPE Object
#define SHARED_PTR shared
#include "fckit/shared_ptr_cast.h"
end function

function Object_id(this)
  integer :: Object_id
  class(Object), intent(in) :: this
  Object_id = this%shared%uid
end function

! -----------------------------------------------------------------------------

function Unrelated_constructor() result(this)
  type(Unrelated) :: this
  allocate( Unrelated_ptr :: this%data ) 
  call this%make_shared( this%data )
  call this%return()
  write(0,*) "Unrelated_constructor"
end function

subroutine Unrelated_ptr_final(this)
  TYPE(Unrelated_ptr), intent(inout) :: this
  write(0,*) "final unrelated"
end subroutine

function Unrelated_shared_ptr_cast(this) result(success)
#define DERIVED_TYPE Unrelated_ptr
#define SHARED_PTR_TYPE Unrelated
#define SHARED_PTR data
#include "fckit/shared_ptr_cast.h"
end function

! -----------------------------------------------------------------------------

function create_Object() result(obj)
  type(Object) :: obj
  obj = Object(999)
  write(0,*) "------------- owners",obj%owners()
  call obj%return()
  write(0,*) "------------- owners",obj%owners()
  write(0,*) "-------------exiting scope create_Object"
end function

! -----------------------------------------------------------------------------

end module fcta_shared_ptr_f_fxt

! -----------------------------------------------------------------------------

TESTSUITE_WITH_FIXTURE(fckit_test_shared_ptr_f,fcta_shared_ptr_f_fxt)

! -----------------------------------------------------------------------------

TESTSUITE_INIT
END_TESTSUITE_INIT

! -----------------------------------------------------------------------------

TESTSUITE_FINALIZE
END_TESTSUITE_FINALIZE

! -----------------------------------------------------------------------------

TEST( test_ref )
  type(Object) :: a
!   type(fckit_shared_ptr) :: b
!   type(Unrelated) :: c
!   type(Object) :: d

#ifdef EC_HAVE_Fortran_FINALIZATION
  write(0,*) "Fortran supports automatic finalization!"
#endif

  a = Object(1)
  write(0,*) __LINE__
  FCTEST_CHECK_EQUAL( a%owners(), 1 )
  write(0,*) __LINE__
  
  FCTEST_CHECK_EQUAL( a%id(), 1 )
  write(0,*) __LINE__
  
!
!   b = a
!   FCTEST_CHECK_EQUAL( a%owners(), 2 )
!
!   !c = b ! --> bad cast (runtime error)
!
!     c = Unrelated()
!
!   FCTEST_CHECK_EQUAL( a%owners(), 2 )
!   FCTEST_CHECK_EQUAL( b%owners(), 2 )
!
!   d = create_Object()
!   FCTEST_CHECK_EQUAL( d%owners(), 1 )
!   FCTEST_CHECK_EQUAL( d%id(), 999 )

! #ifndef EC_HAVE_Fortran_FINALIZATION
!  call a%final()
! call b%final()
! call c%final()
! call d%final()
! #endif

  write(0,*)
  write(0,*) " ~~~~~~~~~~~~~~~~~~~~~~~~~ END SCOPE ~~~~~~~~~~~~~~~~~~~~~~~~~ "
  write(0,*)

END_TEST

! -----------------------------------------------------------------------------

END_TESTSUITE

