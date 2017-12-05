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
! use fckit_object_module
use fckit_final_module
use fckit_shared_ptr_module
use fckit_shared_object_module
! use fckit_c_interop_module
use fctest
implicit none

integer, SAVE :: final_called = 0
integer, SAVE :: final_called_after_scope = 0
logical, SAVE :: scope_ended = .false.

! -----------------------------------------------------------------------------

! Unsafe because final is not guaranteed
type :: ObjectFortranUnsafe 
  integer :: id = -1
    !! Data of Object (could be anything, like allocatable pointers)
contains
#ifdef EC_HAVE_Fortran_FINALIZATION
  final :: ObjectFortranUnsafe_final
#endif
endtype

! Safer because even if auto-final is not guaranteed, a manual final may finalise
type, extends(fckit_final) :: ObjectFortranSafer
  integer :: id = -1
    !! Data of Object (could be anything, like allocatable pointers)
contains
  procedure :: final => ObjectFortranSafer_final
    !! Destructor of the data (e.g. if deallocations need to happen)
endtype

! -----------------------------------------------------------------------------

interface
    function new_Object(i) bind(c,name="new_Object")
      use, intrinsic :: iso_c_binding, only : c_ptr, c_int
      type(c_ptr) :: new_Object
      integer(c_int), value :: i
    end function

   subroutine delete_Object(cptr) bind(c,name="delete_Object")
     use, intrinsic :: iso_c_binding, only : c_ptr
     type(c_ptr), value :: cptr
   end subroutine
   
   function Object__id(this) bind(c,name="Object__id")
     use, intrinsic :: iso_c_binding, only : c_ptr, c_int
     type(c_ptr), value :: this
     integer(c_int) :: Object__id
   end function
   
   function cxx_destructor_called() bind(c,name="cxx_destructor_called")
     use, intrinsic :: iso_c_binding, only : c_int
     integer(c_int) :: cxx_destructor_called
   end function

   subroutine cxx_reset_counters() bind(c,name="cxx_reset_counters")
   end subroutine
   
end interface

type, extends(fckit_shared_object) :: ObjectCxx
contains
! public :
  procedure :: id => ObjectCxx_id
end type

interface ObjectCxx
  module procedure ObjectCxx_constructor
end interface

! -----------------------------------------------------------------------------
contains
! -----------------------------------------------------------------------------

subroutine reset_counters()
  final_called = 0
  final_called_after_scope = 0
  scope_ended = .false.
  call cxx_reset_counters()
end subroutine

subroutine end_scope()
  scope_ended = .true.
end subroutine

function ObjectCxx_constructor(id) result(this)
  type(ObjectCxx) :: this
  integer :: id
  call this%make_shared( new_Object(id) , fckit_c_deleter(delete_Object) )
  call this%return()
end function

function ObjectCxx_id(this) result(id)
  class(ObjectCxx) :: this
  integer :: id
  id = Object__id( this%c_ptr() )
end function


subroutine ObjectFortranSafer_final(this)
  class(ObjectFortranSafer), intent(inout) :: this
  write(0,'(A,I0)') "ObjectFortranSafer_final id",this%id
  this%id = 0
  final_called = final_called + 1
  if( scope_ended ) final_called_after_scope = final_called_after_scope+1
end subroutine

subroutine ObjectFortranUnSafe_final(this)
  type(ObjectFortranUnSafe), intent(inout) :: this
  write(0,'(A,I0)') "ObjectFortranUnSafe_final id",this%id
  this%id = 0
  final_called = final_called + 1
  if( scope_ended ) final_called_after_scope = final_called_after_scope+1
end subroutine

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

function create_ObjectFortranSafer(id) result(obj)
  type(fckit_shared_ptr) :: obj
  integer :: id
  class(ObjectFortranSafer), pointer :: obj_ptr
  write(0,'(A,I0)') "Constructing ObjectFortranSafer, id = ",id
  allocate( ObjectFortranSafer::obj_ptr )
  obj_ptr%id = id
  obj = fckit_make_shared( obj_ptr )
  call obj%return()
end function

subroutine test_shared_ptr_safer( final_auto )
  logical :: final_auto
  type(fckit_shared_ptr) :: obj1
  class(ObjectFortranSafer), pointer :: obj2_ptr => null()
  type(fckit_shared_ptr) :: obj2
  type(fckit_shared_ptr) :: obj3

  write(0,'(A)') "~~~~~~~~~~~~~~ BEGIN SCOPE ~~~~~~~~~~~~~~"

  obj1 = create_ObjectFortranSafer(5)
  FCTEST_CHECK_EQUAL( obj1%owners(), 1 )
  obj2 = obj1
  FCTEST_CHECK_EQUAL( obj1%owners(), 2 )
  associate( shared_ptr => obj2%shared_ptr() )
  select type(shared_ptr)
    class is(ObjectFortranSafer)
      obj2_ptr=>shared_ptr
  end select
  end associate
  FCTEST_CHECK( associated(obj2_ptr) )
  FCTEST_CHECK_EQUAL( obj2_ptr%id, 5 )
  obj3 = obj2
  FCTEST_CHECK_EQUAL( obj1%owners(), 3 )
  obj1 = obj3
  FCTEST_CHECK_EQUAL( obj1%owners(), 3 )
  if( .not. final_auto ) then
    call obj1%final()
    call obj2%final()
    call obj3%final()
  endif

  write(0,'(A)') "~~~~~~~~~~~~~~~ END SCOPE ~~~~~~~~~~~~~~~"
  call end_scope()
end subroutine

TEST( test_shared_ptr_safer_manual )
#if 1 
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST     test_shared_ptr_safer_manual"

  call reset_counters()
  call test_shared_ptr_safer( .false. )
  FCTEST_CHECK_EQUAL( final_called , 1 )
  FCTEST_CHECK_EQUAL( final_called_after_scope , 0 )
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#endif
END_TEST

TEST( test_shared_ptr_safer_auto )
#if 1
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST     test_shared_ptr_safer_auto"

  call reset_counters()
  call test_shared_ptr_safer( .true. )
#ifdef EC_HAVE_Fortran_FINALIZATION
  FCTEST_CHECK_EQUAL( final_called, 1 )
  FCTEST_CHECK_EQUAL( final_called_after_scope, 1 )
#else
  FCTEST_CHECK_EQUAL( final_called, 0 )
  FCTEST_CHECK_EQUAL( final_called_after_scope, 0 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#endif
END_TEST

! -----------------------------------------------------------------------------

function create_ObjectFortranUnSafe(id) result(obj)
  type(fckit_shared_ptr) :: obj
  integer :: id
  class(ObjectFortranUnSafe), pointer :: obj_ptr
  write(0,'(A,I0)') "Constructing ObjectFortranUnsafe, id = ",id
  allocate( ObjectFortranUnSafe::obj_ptr )
  obj_ptr%id = id
  obj = fckit_make_shared( obj_ptr )
  call obj%return()
end function

subroutine test_shared_ptr_unsafe( final_auto )
  logical :: final_auto
  type(fckit_shared_ptr) :: obj1
  class(ObjectFortranUnSafe), pointer :: obj2_ptr => null()
  type(fckit_shared_ptr) :: obj2

  write(0,'(A)') "~~~~~~~~~~~~~~ BEGIN SCOPE ~~~~~~~~~~~~~~"

  obj1 = create_ObjectFortranUnSafe(5)
  obj2 = obj1
  associate( shared_ptr => obj2%shared_ptr() )
  select type(shared_ptr)
    class is(ObjectFortranUnSafe)
      obj2_ptr=>shared_ptr
  end select
  end associate
  FCTEST_CHECK( associated(obj2_ptr) )
  FCTEST_CHECK_EQUAL( obj2_ptr%id, 5 )
  
  if( .not. final_auto ) then
    call obj1%final()
    call obj2%final()
  endif

  write(0,'(A)') "~~~~~~~~~~~~~~~ END SCOPE ~~~~~~~~~~~~~~"
  call end_scope()
end subroutine

TEST( test_shared_ptr_unsafe_manual )
#if 1
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST    test_shared_ptr_unsafe_manual"

  call reset_counters()
  FCTEST_CHECK_EQUAL( final_called, 0 )
  call test_shared_ptr_unsafe( .false. )
#ifdef EC_HAVE_Fortran_FINALIZATION
  FCTEST_CHECK_EQUAL( final_called, 1 )
  FCTEST_CHECK_EQUAL( final_called_after_scope, 0 )
#else
  FCTEST_CHECK_EQUAL( final_called, 0 )  ! --> without finalisation this didnt work as opposed to "safer"
  FCTEST_CHECK_EQUAL( final_called_after_scope, 0 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#endif
END_TEST

TEST( test_shared_ptr_unsafe_auto )
#if 1
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST    test_shared_ptr_unsafe_auto"

  call reset_counters()
  call test_shared_ptr_unsafe( .true. )
#ifdef EC_HAVE_Fortran_FINALIZATION
  FCTEST_CHECK_EQUAL( final_called, 1 )
  FCTEST_CHECK_EQUAL( final_called_after_scope, 1 )
#else
  FCTEST_CHECK_EQUAL( final_called, 0 )
  FCTEST_CHECK_EQUAL( final_called_after_scope, 0 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#endif
END_TEST

! -----------------------------------------------------------------------------

subroutine test_shared_object( final_auto )
  logical :: final_auto
  type(ObjectCXX) :: obj1
  type(fckit_shared_object) :: obj2
  type(ObjectCXX) :: obj3

  write(0,'(A)') "~~~~~~~~~~~~~~ BEGIN SCOPE ~~~~~~~~~~~~~~~"

  obj1 = ObjectCXX(7)
  FCTEST_CHECK_EQUAL( obj1%id(), 7 )
  FCTEST_CHECK_EQUAL( obj1%owners(), 1 )
  obj2 = obj1
  FCTEST_CHECK_EQUAL( obj1%owners(), 2 )
  obj3 = obj2
  FCTEST_CHECK_EQUAL( obj1%owners(), 3 )
  FCTEST_CHECK_EQUAL( obj3%id(), 7 )

  if( .not. final_auto ) then
    call obj1%final()
    FCTEST_CHECK_EQUAL( obj2%owners(), 2 )
    call obj2%final()
    FCTEST_CHECK_EQUAL( obj3%owners(), 1 )
    call obj3%final()
  endif

  write(0,'(A)') "~~~~~~~~~~~~~~~ END SCOPE ~~~~~~~~~~~~~~~"
  call end_scope()
end subroutine

TEST( test_shared_object_manual )
#if 1
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST     test_shared_object_manual"
  call reset_counters()
  call test_shared_object( final_auto = .false. )
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 1 )
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')  
#endif
END_TEST

TEST( test_shared_object_auto )
#if 1
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST     test_shared_object_auto"
  call reset_counters()
  call test_shared_object( final_auto = .true. )
#ifdef EC_HAVE_Fortran_FINALIZATION
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 1 )
#else
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 0 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#endif
END_TEST

! -----------------------------------------------------------------------------

END_TESTSUITE

