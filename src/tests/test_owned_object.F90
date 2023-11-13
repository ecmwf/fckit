! (C) Copyright 2013 ECMWF.
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

! This File contains Unit Tests for testing the
! C++ / Fortran Interfaces to the State Datastructure
! @author Willem Deconinck

#include "fckit/fctest.h"

module test_Grid_module

use fckit_owned_object_module, only: fckit_owned_object
use, intrinsic :: iso_c_binding, only : c_ptr

implicit none

private :: fckit_owned_object
private :: c_ptr

public :: test_Grid
public :: test_StructuredGrid

private

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
end interface

!------------------------------------------------------------------------------
TYPE, extends(fckit_owned_object) :: test_Grid

! Purpose :
! -------
!   *test_Grid* : Object Grid specifications for Grids

! Methods :
! -------

! Author :
! ------
!   9-Oct-2014 Willem Deconinck     *ECMWF*

!------------------------------------------------------------------------------
contains

#if FCKIT_FINAL_NOT_INHERITING
  final :: test_Grid__final_auto
#endif

END TYPE test_Grid

interface test_Grid
  module procedure test_Grid__ctor_id
  module procedure test_Grid__ctor_cptr
end interface

!------------------------------------------------------------------------------

TYPE, extends(test_Grid) :: test_StructuredGrid

! Purpose :
! -------
!   *test_StructuredGrid* : Object Grid specifications for Reduced Grids

! Methods :
! -------

! Author :
! ------
!   9-Oct-2014 Willem Deconinck     *ECMWF*

!------------------------------------------------------------------------------
contains
#if FCKIT_FINAL_NOT_INHERITING
  final :: test_StructuredGrid__final_auto
#endif

END TYPE test_StructuredGrid

interface test_StructuredGrid
  module procedure test_StructuredGrid__ctor_id
end interface

!------------------------------------------------------------------------------

!========================================================
contains
!========================================================

! -----------------------------------------------------------------------------
! Destructor

#if FCKIT_FINAL_NOT_INHERITING
impure elemental subroutine test_Grid__final_auto(this)
  type(test_Grid), intent(inout) :: this
#if FCKIT_FINAL_NOT_PROPAGATING
  call this%final()
#endif
  FCKIT_SUPPRESS_UNUSED( this )
end subroutine

impure elemental subroutine test_StructuredGrid__final_auto(this)
  type(test_StructuredGrid), intent(inout) :: this
#if FCKIT_FINAL_NOT_PROPAGATING
  call this%final()
#endif
  FCKIT_SUPPRESS_UNUSED( this )
end subroutine
#endif


! -----------------------------------------------------------------------------
! Constructors

function test_Grid__ctor_id(identifier) result(this)
  use fckit_c_interop_module, only: c_str
  type(test_Grid) :: this
  integer, intent(in) :: identifier
  call this%reset_c_ptr( new_Object(identifier) )
  call this%return()
end function

function test_Grid__ctor_cptr(cptr) result(this)
  use fckit_c_interop_module, only: c_str
  type(test_Grid) :: this
  type(c_ptr), intent(in) :: cptr
  call this%reset_c_ptr( cptr )
  call this%return()
end function

! -----------------------------------------------------------------------------

function test_StructuredGrid__ctor_id(identifier) result(this)
  use fckit_c_interop_module, only: c_str
  type(test_StructuredGrid) :: this
  integer, intent(in) :: identifier
  call this%reset_c_ptr( new_Object(identifier) )
  call this%return()
end function

function test_StructuredGrid__ctor_cptr(cptr) result(this)
  use fckit_c_interop_module, only: c_str
  type(test_StructuredGrid) :: this
  type(c_ptr), intent(in) :: cptr
  call this%reset_c_ptr( cptr )
  call this%return()
end function

! ----------------------------------------------------------------------------------------

end module test_grid_module


module fcta_FunctionSpace_fxt
use, intrinsic :: iso_c_binding
implicit none



contains

end module

! -----------------------------------------------------------------------------

TESTSUITE_WITH_FIXTURE(fcta_FunctionSpace,fcta_FunctionSpace_fxt)

! -----------------------------------------------------------------------------

TESTSUITE_INIT
  ! call atlas_library%initialise()
END_TESTSUITE_INIT

! -----------------------------------------------------------------------------

TESTSUITE_FINALIZE
  ! call atlas_library%finalise()
END_TESTSUITE_FINALIZE

! -----------------------------------------------------------------------------


TEST( test_nodescolumns )
use test_grid_module
#if 1
type(test_Grid) :: grid

grid = test_StructuredGrid(24)

FCTEST_CHECK_EQUAL( grid%owners(), 1 )
call grid%final()
#else
#warning test test_nodescolumns disabled
#endif
END_TEST

! -----------------------------------------------------------------------------



! -----------------------------------------------------------------------------

END_TESTSUITE

