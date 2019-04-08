! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.



#include "fckit/fctest.h"

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
logical, SAVE :: deallocate_called = .false.

! -----------------------------------------------------------------------------

! Unsafe because final is not guaranteed
type :: ObjectFortranUnsafe
  integer :: id = -1
    !! Data of Object (could be anything, like allocatable pointers)
contains
#if FCKIT_HAVE_FINAL
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
      use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t
      type(c_ptr) :: new_Object
      integer(c_int32_t), value :: i
    end function

   subroutine delete_Object(cptr) bind(c,name="delete_Object")
     use, intrinsic :: iso_c_binding, only : c_ptr
     type(c_ptr), value :: cptr
   end subroutine

   function Object__id(this) bind(c,name="Object__id")
     use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t
     type(c_ptr), value :: this
     integer(c_int32_t) :: Object__id
   end function

   function cxx_destructor_called() bind(c,name="cxx_destructor_called")
     use, intrinsic :: iso_c_binding, only : c_int32_t
     integer(c_int32_t) :: cxx_destructor_called
   end function

   function cxx_destructor_called_after_scope() bind(c,name="cxx_destructor_called_after_scope")
     use, intrinsic :: iso_c_binding, only : c_int32_t
     integer(c_int32_t) :: cxx_destructor_called_after_scope
   end function

   subroutine cxx_reset_counters() bind(c,name="cxx_reset_counters")
   end subroutine

   subroutine cxx_end_scope() bind(c,name="cxx_end_scope")
   end subroutine

end interface

type, extends(fckit_shared_object) :: ObjectCXX
contains
! public :
  procedure :: id => ObjectCXX_id

#if FCKIT_FINAL_NOT_INHERITING
  final :: ObjectCXX_final_auto
#endif
end type

interface ObjectCXX
  module procedure ObjectCXX_constructor
end interface

! -----------------------------------------------------------------------------
contains
! -----------------------------------------------------------------------------

subroutine reset_counters()
  deallocate_called = .false.
  final_called = 0
  final_called_after_scope = 0
  scope_ended = .false.
  call cxx_reset_counters()
end subroutine

subroutine end_scope()
  scope_ended = .true.
  call cxx_end_scope()
end subroutine

function ObjectCXX_constructor(id) result(this)
  type(ObjectCXX) :: this
  integer :: id
  call this%reset_c_ptr( new_Object(id) , fckit_c_deleter(delete_Object) )
  FCTEST_CHECK_EQUAL( this%owners(), 1 )
  call this%return()
end function

function ObjectCXX_id(this) result(id)
  class(ObjectCXX) :: this
  integer :: id
  id = Object__id( this%CPTR_PGIBUG_B )
end function

FCKIT_FINAL subroutine ObjectCXX_final_auto(this)
  type(ObjectCXX), intent(inout) :: this
  write(0,*) "ObjectCXX_final_auto"
#if FCKIT_FINAL_NOT_PROPAGATING
  call this%final()
#endif
  FCKIT_SUPPRESS_UNUSED( this )
end subroutine


subroutine ObjectFortranSafer_final(this)
  class(ObjectFortranSafer), intent(inout) :: this
  write(0,'(A,I0)') "ObjectFortranSafer_final id",this%id
  this%id = 0
  final_called = final_called + 1
  if( scope_ended ) final_called_after_scope = final_called_after_scope+1
end subroutine

FCKIT_FINAL subroutine ObjectFortranUnSafe_final(this)
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

function create_ObjectFortranSafer(id) result(this)
  type(fckit_shared_ptr) :: this
  integer :: id
  class(ObjectFortranSafer), pointer :: ptr
  write(0,'(A,I0)') "Constructing ObjectFortranSafer, id = ",id
  allocate( ObjectFortranSafer::ptr )
  ptr%id = id
  write(0,'(A)') "----> this = fckit_make_shared( obj_ptr )"
  !this = fckit_make_shared( ptr )
  call this%share( ptr )
  write(0,'(A)') "<---- this = fckit_make_shared( obj_ptr )"
  FCTEST_CHECK_EQUAL( this%owners(), 1 )
  call this%return()
end function

subroutine test_shared_ptr_safer( final_auto )
  logical :: final_auto
  type(fckit_shared_ptr) :: obj1
  class(ObjectFortranSafer), pointer :: obj2_ptr => null()
  type(fckit_shared_ptr) :: obj2
  type(fckit_shared_ptr) :: obj3
  class(*), pointer :: shared_ptr

  write(0,'(A)') "~~~~~~~~~~~~~~ BEGIN SCOPE ~~~~~~~~~~~~~~"

  obj1 = create_ObjectFortranSafer(5)
  FCTEST_CHECK_EQUAL( obj1%owners(), 1 )

  obj2 = obj1
  FCTEST_CHECK_EQUAL( obj1%owners(), 2 )
  shared_ptr => obj2%shared_ptr()
  select type(shared_ptr)
    class is(ObjectFortranSafer)
      obj2_ptr=>shared_ptr
  end select
  FCTEST_CHECK( associated(obj2_ptr) )
  FCTEST_CHECK_EQUAL( obj2_ptr%id, 5 )
  obj3 = obj2
  FCTEST_CHECK_EQUAL( obj1%owners(), 3 )
  obj1 = obj3
  FCTEST_CHECK_EQUAL( obj1%owners(), 3 )
  if( .not. final_auto ) then
    write(0,*) "manual final , owners = ", obj1%owners()
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
#if FCKIT_HAVE_FINAL
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

function create_ObjectFortranUnSafe(id) result(this)
  type(fckit_shared_ptr) :: this
  integer :: id
  class(ObjectFortranUnSafe), pointer :: ptr
  write(0,'(A,I0)') "Constructing ObjectFortranUnsafe, id = ",id
  allocate( ObjectFortranUnSafe::ptr )
  ptr%id = id
  call this%share( ptr )
  call this%return()
end function

subroutine test_shared_ptr_unsafe( final_auto )
  logical :: final_auto
  type(fckit_shared_ptr) :: obj1
  class(ObjectFortranUnSafe), pointer :: obj2_ptr => null()
  type(fckit_shared_ptr) :: obj2
  class(*), pointer :: shared_ptr

  write(0,'(A)') "~~~~~~~~~~~~~~ BEGIN SCOPE ~~~~~~~~~~~~~~"

  obj1 = create_ObjectFortranUnSafe(5)
  obj2 = obj1
  shared_ptr => obj2%shared_ptr()
  select type(shared_ptr)
    class is(ObjectFortranUnSafe)
      obj2_ptr=>shared_ptr
  end select
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
#if FCKIT_HAVE_FINAL
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
#if FCKIT_HAVE_FINAL
  FCTEST_CHECK_EQUAL( final_called, 1 )
  FCTEST_CHECK_EQUAL( final_called_after_scope, 1 )
#else
  FCTEST_CHECK_EQUAL( final_called, 0 )
  FCTEST_CHECK_EQUAL( final_called_after_scope, 0 )
  write(0,'(A)') "WARNING    memory leaked"
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#endif
END_TEST

! -----------------------------------------------------------------------------

subroutine test_shared_object( final_auto )

  logical :: final_auto
  type(ObjectCXX) :: obj1
  type(fckit_shared_ptr) :: obj2
  type(ObjectCXX) :: obj3

  write(0,'(A)') "~~~~~~~~~~~~~~ BEGIN SCOPE ~~~~~~~~~~~~~~~"

  obj1 = ObjectCXX(7)
  FCTEST_CHECK_EQUAL( obj1%id(), 7 )
  FCTEST_CHECK_EQUAL( obj1%owners(), 1 )

  write(0,*) "obj2 = obj1"
  obj2 = obj1
  FCTEST_CHECK_EQUAL( obj1%owners(), 2 )

  write(0,*) "obj3 = obj2"
  obj3 = obj2
  FCTEST_CHECK_EQUAL( obj1%owners(), 3 )
  FCTEST_CHECK_EQUAL( obj3%id(), 7 )

  write(0,*) "obj1 = obj3"
  obj1 = obj3

  FCTEST_CHECK_EQUAL( obj1%owners(), 3 )
  FCTEST_CHECK_EQUAL( obj2%owners(), 3 )

  write(0,*) "obj1 = ObjectCXX(4)"
  obj1 = ObjectCXX(4)
  FCTEST_CHECK_EQUAL( obj1%owners(), 1 )
  FCTEST_CHECK_EQUAL( obj2%owners(), 2 )

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
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 2 )
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
#if FCKIT_HAVE_FINAL
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 2 )
#else
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 0 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#endif
END_TEST

! -----------------------------------------------------------------------------

subroutine test_shared_object_allocatable( final_auto, deallocate_auto )

  logical :: final_auto
  logical :: deallocate_auto
  type(ObjectCXX), allocatable :: obj1

  write(0,'(A)') "~~~~~~~~~~~~~~ BEGIN SCOPE ~~~~~~~~~~~~~~~"

  allocate( obj1 )
  obj1 = ObjectCXX(7)
  FCTEST_CHECK_EQUAL( obj1%id(), 7 )
  FCTEST_CHECK_EQUAL( obj1%owners(), 1 )

  if( .not. final_auto ) then
    call obj1%final()
  endif

  if( .not. deallocate_auto ) then
    write(0,'(A)') "~~~~~~~~~~~~~~ DEALLOCATE ~~~~~~~~~~~~~~~"
    deallocate_called = .true.
    deallocate( obj1 )
  endif

  write(0,'(A)') "~~~~~~~~~~~~~~~ END SCOPE ~~~~~~~~~~~~~~~"
  call end_scope()
end subroutine

TEST( test_shared_object_allocatable_auto_auto )
#if 1
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST     test_shared_object_allocatable_auto_auto"
  call reset_counters()
  call test_shared_object_allocatable( final_auto = .true., deallocate_auto = .true. )
#if FCKIT_HAVE_FINAL
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 1 )
  FCTEST_CHECK_EQUAL( cxx_destructor_called_after_scope(), 1 )
#else
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 0 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#endif
END_TEST

TEST( test_shared_object_allocatable_auto_manual )
#if 1
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST     test_shared_object_allocatable_auto_manual"
  call reset_counters()
  call test_shared_object_allocatable( final_auto = .true., deallocate_auto = .false. )
#if FCKIT_HAVE_FINAL
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 1 )
  FCTEST_CHECK_EQUAL( cxx_destructor_called_after_scope(), 0 )
#else
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 0 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#endif
END_TEST

TEST( test_shared_object_allocatable_manual_auto )
#if 1
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST     test_shared_object_allocatable_manual_auto"
  call reset_counters()
  call test_shared_object_allocatable( final_auto = .false., deallocate_auto = .true. )
#if FCKIT_HAVE_FINAL
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 1 )
  FCTEST_CHECK_EQUAL( cxx_destructor_called_after_scope(), 0 )
#else
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 1 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#endif
END_TEST

TEST( test_shared_object_allocatable_manual_manual )
#if 1
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST     test_shared_object_allocatable_manual_manual"
  call reset_counters()
  call test_shared_object_allocatable( final_auto = .false., deallocate_auto = .false. )
#if FCKIT_HAVE_FINAL
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 1 )
  FCTEST_CHECK_EQUAL( cxx_destructor_called_after_scope(), 0 )
#else
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 1 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#endif
END_TEST

! -----------------------------------------------------------------------------

subroutine test_shared_object_allocatable_list( final_auto, deallocate_auto )

  logical :: final_auto
  logical :: deallocate_auto
  type(ObjectCXX), allocatable :: list(:)

  write(0,'(A)') "~~~~~~~~~~~~~~ BEGIN SCOPE ~~~~~~~~~~~~~~~"

  allocate( list(2) )
  list(1) = ObjectCXX(1)
  list(2) = ObjectCXX(2)
  write(0,*) "assigned"
  FCTEST_CHECK_EQUAL( list(1)%id(), 1 )
  FCTEST_CHECK_EQUAL( list(1)%owners(), 1 )
  FCTEST_CHECK_EQUAL( list(2)%id(), 2 )
  FCTEST_CHECK_EQUAL( list(2)%owners(), 1 )

  if( .not. final_auto ) then
    call list(1)%final()
    call list(2)%final()
  endif

  if( .not. deallocate_auto ) then
    write(0,'(A)') "~~~~~~~~~~~~~~ DEALLOCATE ~~~~~~~~~~~~~~~"
    deallocate_called = .true.
    deallocate( list )
  endif

  write(0,'(A)') "~~~~~~~~~~~~~~~ END SCOPE ~~~~~~~~~~~~~~~"
  call end_scope()
end subroutine

TEST( test_shared_object_allocatable_list_auto_auto )
#if 1
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST     test_shared_object_allocatable_list_auto_auto"
  call reset_counters()
  call test_shared_object_allocatable_list( final_auto = .true., deallocate_auto = .true. )
#if FCKIT_HAVE_FINAL
#if ! FCKIT_FINAL_BROKEN_FOR_ALLOCATABLE_ARRAY
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 2 )
  FCTEST_CHECK_EQUAL( cxx_destructor_called_after_scope(), 2 )
#endif
#else
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 0 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#endif
END_TEST

TEST( test_shared_object_allocatable_list_auto_manual )
#if 0
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST     test_shared_object_allocatable_list_auto_manual"
  call reset_counters()
  call test_shared_object_allocatable_list( final_auto = .true., deallocate_auto = .false. )
  write(0,'(A)') '----'

#if FCKIT_HAVE_FINAL
#if ! FCKIT_FINAL_BROKEN_FOR_ALLOCATABLE_ARRAY
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 2 )
  FCTEST_CHECK_EQUAL( cxx_destructor_called_after_scope(), 0 )
#endif
#else
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 0 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#else
#ifndef __ibmxl__
#warning test_shared_object_allocatable_list_auto_manual disabled
#endif
#endif
END_TEST

TEST( test_shared_object_allocatable_list_manual_auto )
#if 1
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST     test_shared_object_allocatable_list_manual_auto"
  call reset_counters()
  call test_shared_object_allocatable_list( final_auto = .false., deallocate_auto = .true. )
#if FCKIT_HAVE_FINAL
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 2 )
  FCTEST_CHECK_EQUAL( cxx_destructor_called_after_scope(), 0 )
#else
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 2 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#else
#ifndef __ibmxl__
#warning test_shared_object_allocatable_list_manual_auto disabled
#endif
#endif
END_TEST

TEST( test_shared_object_allocatable_list_manual_manual )
#if 0
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST     test_shared_object_allocatable_list_manual_manual"
  call reset_counters()
  call test_shared_object_allocatable_list( final_auto = .false., deallocate_auto = .false. )
#if FCKIT_HAVE_FINAL
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 2 )
  FCTEST_CHECK_EQUAL( cxx_destructor_called_after_scope(), 0 )
#else
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 2 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#else
#ifndef __ibmxl__
#warning test_shared_object_allocatable_list_manual_manual disabled
#endif
#endif
END_TEST

! -----------------------------------------------------------------------------

subroutine test_shared_object_pointer_list( final_auto, deallocate_auto )

  logical :: final_auto
  logical :: deallocate_auto
  type(ObjectCXX), pointer :: list(:)

  write(0,'(A)') "~~~~~~~~~~~~~~ BEGIN SCOPE ~~~~~~~~~~~~~~~"

  allocate( list(2) )
  write(0,*) "allocated"
  list(1) = ObjectCXX(1)
  list(2) = ObjectCXX(2)
  write(0,*) "assigned"
  FCTEST_CHECK_EQUAL( list(1)%id(), 1 )
  FCTEST_CHECK_EQUAL( list(1)%owners(), 1 )
  FCTEST_CHECK_EQUAL( list(2)%id(), 2 )
  FCTEST_CHECK_EQUAL( list(2)%owners(), 1 )

  if( .not. final_auto ) then
    call list(1)%final()
    call list(2)%final()
  endif

  if( .not. deallocate_auto ) then
    write(0,'(A)') "~~~~~~~~~~~~~~ DEALLOCATE ~~~~~~~~~~~~~~~"
    deallocate_called = .true.
    deallocate( list )
  endif

  write(0,'(A)') "~~~~~~~~~~~~~~~ END SCOPE ~~~~~~~~~~~~~~~"
  call end_scope()
  if( associated(list) ) deallocate( list )
end subroutine

TEST( test_shared_object_pointer_list_auto_auto )
#if 1
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST     test_shared_object_pointer_list_auto_auto"
  call reset_counters()
  call test_shared_object_pointer_list( final_auto = .true., deallocate_auto = .true. )
#if FCKIT_HAVE_FINAL
#if ! FCKIT_FINAL_BROKEN_FOR_ALLOCATABLE_ARRAY
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 2 )
  FCTEST_CHECK_EQUAL( cxx_destructor_called_after_scope(), 2 )
#endif
#else
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 0 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#endif
END_TEST

TEST( test_shared_object_pointer_list_auto_manual )
#if 1
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST     test_shared_object_pointer_list_auto_manual"
  call reset_counters()
  call test_shared_object_pointer_list( final_auto = .true., deallocate_auto = .false. )
#if FCKIT_HAVE_FINAL
#if ! FCKIT_FINAL_BROKEN_FOR_ALLOCATABLE_ARRAY
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 2 )
  FCTEST_CHECK_EQUAL( cxx_destructor_called_after_scope(), 0 )
#endif
#else
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 0 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#endif
END_TEST

TEST( test_shared_object_pointer_list_manual_auto )
#if 1
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST     test_shared_object_pointer_list_manual_auto"
  call reset_counters()
  call test_shared_object_pointer_list( final_auto = .false., deallocate_auto = .true. )
#if FCKIT_HAVE_FINAL
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 2 )
  FCTEST_CHECK_EQUAL( cxx_destructor_called_after_scope(), 0 )
#else
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 2 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#endif
END_TEST

TEST( test_shared_object_pointer_list_manual_manual )
#if 1
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST     test_shared_object_pointer_list_manual_manual"
  call reset_counters()
  call test_shared_object_pointer_list( final_auto = .false., deallocate_auto = .false. )
#if FCKIT_HAVE_FINAL
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 2 )
  FCTEST_CHECK_EQUAL( cxx_destructor_called_after_scope(), 0 )
#else
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 2 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#endif
END_TEST


! -----------------------------------------------------------------------------

subroutine test_shared_object_automatic_list( final_auto )

  logical :: final_auto
  type(ObjectCXX) :: list(2)

  write(0,'(A)') "~~~~~~~~~~~~~~ BEGIN SCOPE ~~~~~~~~~~~~~~~"

  list(1) = ObjectCXX(1)
  list(2) = ObjectCXX(2)
  write(0,'(A)') "assigned"
  FCTEST_CHECK_EQUAL( list(1)%id(), 1 )
  FCTEST_CHECK_EQUAL( list(1)%owners(), 1 )
  FCTEST_CHECK_EQUAL( list(2)%id(), 2 )
  FCTEST_CHECK_EQUAL( list(2)%owners(), 1 )

  if( .not. final_auto ) then
    call list(1)%final()
    call list(2)%final()
  endif

  write(0,'(A)') "~~~~~~~~~~~~~~~ END SCOPE ~~~~~~~~~~~~~~~"
  call end_scope()
end subroutine

TEST( test_shared_object_automatic_list_auto )
#if 1
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST     test_shared_object_automatic_list_auto"
  call reset_counters()
  call test_shared_object_automatic_list( final_auto = .true. )
#if FCKIT_HAVE_FINAL
#if ! FCKIT_FINAL_BROKEN_FOR_AUTOMATIC_ARRAY
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 2 )
  FCTEST_CHECK_EQUAL( cxx_destructor_called_after_scope(), 2 )
#endif
#else
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 0 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#endif
END_TEST

TEST( test_shared_object_automatic_list_manual )
#if 1
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)') "TEST     test_shared_object_automatic_list_manual"
  call reset_counters()
  call test_shared_object_automatic_list( final_auto = .false. )
#if FCKIT_HAVE_FINAL
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 2 )
  FCTEST_CHECK_EQUAL( cxx_destructor_called_after_scope(), 0 )
#else
  FCTEST_CHECK_EQUAL( cxx_destructor_called(), 2 )
#endif
  write(0,'(A)') "-------------------------------------------------------------"
  write(0,'(A)')
#endif
END_TEST

! -----------------------------------------------------------------------------

END_TESTSUITE

