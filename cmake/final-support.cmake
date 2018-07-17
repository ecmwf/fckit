
macro( check_final_support )

set( DEBUG_FINAL_SUPPORT FALSE  )
macro( debug_test case )
  if( DEBUG_FINAL_SUPPORT )
    file( WRITE ${CMAKE_CURRENT_BINARY_DIR}/fckit-test-${case}.F90 ${FINAL_SUPPORT_SOURCE} )
    ecbuild_add_executable(
      TARGET  fckit-test-${case}
      SOURCES ${CMAKE_CURRENT_BINARY_DIR}/fckit-test-${case}.F90
      DEFINITIONS ${case}
    )
  endif()
endmacro()

macro( check_final_support_case case )

  if( NOT DEFINED FCKIT_${case} )
    file( WRITE ${CMAKE_CURRENT_BINARY_DIR}/fckit-test-${case}.F90 ${FINAL_SUPPORT_SOURCE} )

    try_compile( ${case}_compiled
                 ${CMAKE_CURRENT_BINARY_DIR}
                 ${CMAKE_CURRENT_BINARY_DIR}/fckit-test-${case}.F90
                 COMPILE_DEFINITIONS -D${case}
                 LINK_LIBRARIES "${CMAKE_EXE_LINKER_FLAGS}"
                 OUTPUT_VARIABLE FCKIT_${case}_compile_output
                 COPY_FILE ${CMAKE_CURRENT_BINARY_DIR}/${case}.bin )

    execute_process( COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${case}.bin
                     WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                     RESULT_VARIABLE _run_res
                     OUTPUT_VARIABLE FCKIT_${case} ERROR_VARIABLE _run_err )


    string( STRIP ${FCKIT_${case}} FCKIT_${case} )
    set( FCKIT_${case} ${FCKIT_${case}} CACHE STRING "" )
    debug_test( ${case} )
  endif()

endmacro()

set( FINAL_SUPPORT_SOURCE
"
#ifdef FINAL_FUNCTION_RESULT
#define TEST 1
#endif

#ifdef FINAL_UNINITIALIZED_LOCAL
#define TEST 1
#endif

#ifdef FINAL_UNINITIALIZED_INTENT_OUT
#define TEST 2
#endif

#ifdef FINAL_UNINITIALIZED_INTENT_INOUT
#define TEST 3
#endif

#ifdef FINAL_NOT_PROPAGATING
#define TEST 6
#endif

#ifdef FINAL_NOT_INHERITING
#define TEST 7
#endif

#ifdef FINAL_BROKEN_FOR_ALLOCATABLE_ARRAY
#define TEST 8
#endif

#ifdef FINAL_BROKEN_FOR_AUTOMATIC_ARRAY
#define TEST 9
#endif

#ifndef TEST
#define OUTPUT
#endif

module final_support_module
implicit none
public
integer, parameter :: output_unit = 6

type :: Object
  logical, public :: return = .false.
  logical, public :: initialized = .false.
  logical, public :: finalized = .false.
contains
  procedure, public :: copy => copy_f
  generic, public :: assignment(=) => copy
  final :: destructor
endtype

interface Object
  module procedure construct_Object
end interface

type, extends(Object) :: ObjectDerivedWithFinal
contains
  final :: destructor_ObjectDerivedWithFinal
endtype

interface ObjectDerivedWithFinal
  module procedure construct_ObjectDerivedWithFinal
end interface

type, extends(Object) :: ObjectDerivedWithoutFinal
contains
endtype

interface ObjectDerivedWithoutFinal
  module procedure construct_ObjectDerivedWithoutFinal
end interface


integer :: final_uninitialized = 0
integer :: final_return        = 0
integer :: final_initialized   = 0
integer :: final_base         = 0
integer :: final_derived       = 0
integer :: indent=0

contains

subroutine reset()
  final_uninitialized = 0
  final_return        = 0
  final_initialized   = 0
  final_base          = 0
  final_derived       = 0
end subroutine

subroutine write_indented( string )
  character(len=*) :: string
  integer :: i
#ifdef OUTPUT
  do i=1,indent
    write(0,'(A)',advance='no') '  '
  enddo
  write(0,'(A)') string
#endif
end subroutine
subroutine write_counters()
#ifdef OUTPUT
  write(0,*) ''
  write(0,*) 'final_uninitialized: ',final_uninitialized
  write(0,*) 'final_initialized:   ',final_initialized
  write(0,*) 'final_return:        ',final_return
  write(0,*) 'final_base:          ',final_base
  write(0,*) 'final_derived:       ',final_derived
#endif
end subroutine

function construct_Object() result(this)
  type(Object) :: this
  this%initialized = .true.
  this%return = .true.
end function

function construct_ObjectDerivedWithFinal() result(this)
  type(ObjectDerivedWithFinal) :: this
  this%initialized = .true.
  this%return = .true.
end function

function construct_ObjectDerivedWithoutFinal() result(this)
  type(ObjectDerivedWithoutFinal) :: this
  this%initialized = .true.
  this%return = .true.
end function

subroutine destructor_ObjectDerivedWithFinal(this)
  type(ObjectDerivedWithFinal) :: this
  call write_indented( 'final( derived )' )
  final_derived = final_derived + 1
  associate( unused => this )
  end associate
end subroutine

subroutine copy_f(this,obj_in)
  class(Object), intent(inout) :: this
  class(Object), target, intent(in) :: obj_in
#if 1
  if( obj_in%return ) then
     if( .not. this%initialized ) then
        call write_indented( 'copy uninitialized from rvalue' )
     else
        call write_indented( 'copy initialized from rvalue' )
     endif
  else if ( obj_in%initialized ) then
     if( .not. this%initialized ) then
        call write_indented( 'copy uninitialized from already existing initialized' )
     else
        call write_indented( 'copy initialized from already existing initialized' )
     endif
  endif
#endif
  this%initialized = obj_in%initialized
  this%return = .false.
end subroutine

impure elemental subroutine destructor(this)
  type(Object), intent(inout) :: this
  final_base = final_base + 1

  if( .not. this%initialized ) then
    call write_indented( 'final( uninitialized )' )
    final_uninitialized = final_uninitialized+1
  else
    if( this%return ) then
      call write_indented( 'final( returned )' )
      final_return = final_return+1
    else
      call write_indented( 'final( initialized )' )
      final_initialized = final_initialized+1
    endif
  endif
end subroutine


subroutine create_obj_out(obj)
  implicit none
  type(Object), intent(out) :: obj
  call write_indented( 'obj = Object()' )
  indent = indent+1
  obj = Object()
  indent = indent-1
end subroutine

subroutine create_obj_inout(obj)
  implicit none
  type(Object), intent(inout) :: obj
  call write_indented( 'obj = Object()' )
  indent = indent+1
  obj = Object()
  indent = indent-1
end subroutine

subroutine test1
  implicit none
  type(Object) :: obj
  call write_indented( 'obj = Object()' )
  indent = indent+1
  obj = Object()
  indent = indent-1
end subroutine

subroutine test2
  implicit none
  type(Object) :: obj
  call write_indented( 'subroutine create_obj_out(obj)' )
  indent = indent+1
  call create_obj_out(obj)
  indent = indent-1
  call write_indented( 'end subroutine create_obj_out(obj)' )
end subroutine

subroutine test3
  implicit none
  type(Object) :: obj
  call write_indented( 'subroutine create_obj_inout(obj)' )
  indent = indent+1
  call create_obj_inout(obj)
  indent = indent-1
  call write_indented( 'end subroutine create_obj_inout(obj)' )
end subroutine

subroutine test4
  implicit none
  type(Object) :: obj1, obj2
  call write_indented( 'subroutine create_obj_inout(obj1)' )
  indent = indent+1
  call create_obj_inout(obj1)
  indent = indent-1
  call write_indented( 'end subroutine create_obj_inout(obj)' )
  call write_indented( 'obj2 = obj1' )
  indent = indent+1
  obj2 = obj1
  indent = indent-1
end subroutine

subroutine test5
  implicit none
  type(Object) :: obj1, obj2
  call write_indented( 'subroutine create_obj_inout(obj1)' )
  indent = indent+1
  call create_obj_inout(obj1)
  indent = indent-1
  call write_indented( 'end subroutine create_obj_inout(obj)' )
  call write_indented( 'obj2 = obj1' )
  indent = indent+1
  obj2 = obj1
  indent = indent-1
  call write_indented( 'obj2 = obj1' )
  indent = indent+1
  obj1 = obj2
  indent = indent-1
end subroutine

subroutine test6
  implicit none
  type(ObjectDerivedWithFinal) :: obj
  indent = indent+1
  obj = ObjectDerivedWithFinal()
  indent = indent-1
  call write_indented('--- scope end ---')
end subroutine

subroutine test7
  implicit none
  type(ObjectDerivedWithoutFinal) :: obj
  indent = indent+1
  obj = ObjectDerivedWithoutFinal()
  indent = indent-1
  call write_indented('--- scope end ---')
end subroutine

subroutine test8
  implicit none
  type(Object), allocatable :: list(:)
  allocate( list(2) )
  call write_indented('list(1) = Object()')
  indent=indent+1
  list(1) = Object()
  indent=indent-1
  call write_indented('list(2) = Object()')
  indent=indent+1
  list(2) = Object()
  indent=indent-1
  call write_indented('--- deallocate ---')
  deallocate( list )
  call write_indented('--- scope end ---')
end subroutine

subroutine test9
  implicit none
  type(Object) :: list(2)
  call write_indented('list(1) = Object()')
  indent=indent+1
  list(1) = Object()
  indent=indent-1
  call write_indented('list(2) = Object()')
  indent=indent+1
  list(2) = Object()
  indent=indent-1
  call write_indented('--- scope end ---')
end subroutine

subroutine run_test(i)
  integer, intent(in) :: i
  character(len=1) :: test_number
  write(test_number,'(I0)') i
#ifndef TEST
#define COMPARE_TEST(x) (x == i)
#else
#define COMPARE_TEST(x) (x == TEST)
#endif
#ifdef OUTPUT
  write(0,'(A)') '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
#endif
  call write_indented( 'subroutine test'//test_number )
  indent = indent+1
  call reset
  if( COMPARE_TEST(1) ) call test1
  if( COMPARE_TEST(2) ) call test2
  if( COMPARE_TEST(3) ) call test3
  if( COMPARE_TEST(4) ) call test4
  if( COMPARE_TEST(5) ) call test5
  if( COMPARE_TEST(6) ) call test6
  if( COMPARE_TEST(7) ) call test7
  if( COMPARE_TEST(8) ) call test8
  if( COMPARE_TEST(9) ) call test9
  indent = indent-1
  call write_indented( 'end subroutine test'//test_number )
  call write_counters()
end subroutine


end module


program final_support
  use final_support_module
  implicit none

  call run_test(1)
  call write_indented( 'test1 summary:' )
  if( final_return > 0 ) then
    call write_indented( 'rvalue Object() was finalised' )
  endif
  if( final_uninitialized > 0 ) then
    call write_indented( 'Locally scoped object is finalised before assignment' )
  endif
  if( final_return == 0 .and. final_uninitialized == 0 ) then
    call write_indented( 'Behaviour of GNU 6.3.0' )
  endif
  if( final_return == 0 .and. final_uninitialized == 1 ) then
    call write_indented( 'Behaviour of PGI 17.10' )
  endif
  if( final_return == 1 .and. final_uninitialized == 0 ) then
    call write_indented( 'Behaviour of Cray 8.6.2' )
    call write_indented( 'Behaviour of Intel 17-18' )
  endif
#ifdef FINAL_FUNCTION_RESULT
  write(output_unit,'(I0)',advance='no') final_return
#endif
#ifdef FINAL_UNINITIALIZED_LOCAL
  write(output_unit,'(I0)',advance='no') final_uninitialized
#endif

  call run_test(2)
  call write_indented( 'test2 summary:' )
  if( final_uninitialized > 0 ) then
    call write_indented( 'object with intent OUT is finalised before assignment' )
  endif
  if( final_uninitialized == 1 ) then
    call write_indented( 'Behaviour of GNU 6.3.0' )
    call write_indented( 'Behaviour of Intel 17-18' )
  endif
  if( final_uninitialized == 0 ) then
    call write_indented( 'Behaviour of Cray 8.6.2' )
    call write_indented( 'Behaviour of PGI 17.10' )
  endif
#ifdef FINAL_UNINITIALIZED_INTENT_OUT
  write(output_unit,'(I0)',advance='no') final_uninitialized
#endif


  call run_test(3)
  call write_indented( 'test3 summary:' )
  if( final_uninitialized > 0 ) then
    call write_indented('object with intent INOUT is finalised before assignment')
  endif
  if( final_uninitialized == 0 ) then
    call write_indented( 'Behaviour of GNU 6.3.0' )
    call write_indented( 'Behaviour of Cray 8.6.2' )
    call write_indented( 'Behaviour of Intel 17-18' )
    call write_indented( 'Behaviour of PGI 17.1' )
  endif
#ifdef FINAL_UNINITIALIZED_INTENT_INOUT
  write(output_unit,'(I0)',advance='no') final_uninitialized
#endif

  call run_test(4)

  call write_indented( 'test4 summary:' )
  if( final_uninitialized == 0 .and. final_initialized == 2 ) then
    call write_indented( 'Behaviour of GNU 6.3.0' )
  endif

  call run_test(5)

  call write_indented( 'test5 summary:' )
  if( final_uninitialized == 0 .and. final_initialized == 2 ) then
    call write_indented( 'Behaviour of GNU 6.3.0' )
  endif

  call run_test(6)
#ifdef FINAL_NOT_PROPAGATING
  if( final_derived > 0 .and. final_initialized == 0 ) then
    write(output_unit,'(I0)',advance='no') 1
  else
    write(output_unit,'(I0)',advance='no') 0
  endif
#endif

  call run_test(7)
#ifdef FINAL_NOT_INHERITING
  if( final_initialized == 0 ) then
    write(output_unit,'(I0)',advance='no') 1
  else
    write(output_unit,'(I0)',advance='no') 0
  endif
#endif

  call run_test(8)
#ifdef FINAL_BROKEN_FOR_ALLOCATABLE_ARRAY
  if( final_initialized == 0 ) then
    write(output_unit,'(I0)',advance='no') 1
  else
    write(output_unit,'(I0)',advance='no') 0
  endif
#endif

  call run_test(9)
#ifdef FINAL_BROKEN_FOR_AUTOMATIC_ARRAY
  if( final_initialized == 0 ) then
    write(output_unit,'(I0)',advance='no') 1
  else
    write(output_unit,'(I0)',advance='no') 0
  endif
#endif

end program
" )

list( APPEND cases
  FINAL_FUNCTION_RESULT
  FINAL_UNINITIALIZED_LOCAL
  FINAL_UNINITIALIZED_INTENT_OUT
  FINAL_UNINITIALIZED_INTENT_INOUT
  FINAL_NOT_PROPAGATING
  FINAL_NOT_INHERITING
  FINAL_BROKEN_FOR_ALLOCATABLE_ARRAY
  FINAL_BROKEN_FOR_AUTOMATIC_ARRAY
)
foreach( case ${cases})
  check_final_support_case( ${case} )
endforeach()

file( WRITE ${CMAKE_CURRENT_BINARY_DIR}/fckit-final-support.F90 ${FINAL_SUPPORT_SOURCE} )
ecbuild_add_executable(
  TARGET  fckit-final-support
  SOURCES ${CMAKE_CURRENT_BINARY_DIR}/fckit-final-support.F90
)


endmacro()
