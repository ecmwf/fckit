
macro( check_final_support )

set(FINAL_FUNCTION_RESULT_SOURCE
"
module test1_mod
implicit none
public

type :: Object
  logical, private :: return = .false.
  logical, private :: initialized = .false.
  logical, private :: finalized = .false.
contains
  procedure, public :: copy => copy_f
  generic, public :: assignment(=) => copy
  final :: destructor
endtype

interface Object
  module procedure construct_Object
end interface


integer :: final_uninitialized = 0
integer :: final_return        = 0
integer :: final_initialized   = 0
integer :: final_total         = 0

contains

function construct_Object() result(this)
  type(Object) :: this
  this%initialized = .true.
  this%return = .true.
end function

subroutine copy_f(this,obj_in)
  class(Object), intent(inout) :: this
  class(Object), target, intent(in) :: obj_in
  this%initialized = obj_in%initialized
  this%return = .false.
end subroutine

subroutine destructor(this)
  type(Object), intent(inout) :: this
  final_total = final_total + 1
  
  if( .not. this%initialized ) then
    final_uninitialized = final_uninitialized+1    
  else
    if( this%return ) then
      final_return = final_return+1
    else
      final_initialized = final_initialized+1
    endif
  endif
end subroutine

end module

subroutine return_test
  use test1_mod
  implicit none
  type(Object) :: obj
  obj = Object()
end subroutine

program test1
  use test1_mod
  implicit none
  final_return = 0
  call return_test
  write(0,'(I0)')final_return
end program
")
ecbuild_check_fortran_source_return( 
  ${FINAL_FUNCTION_RESULT_SOURCE}
  VAR FINAL_FUNCTION_RESULT
  OUTPUT result1
)
string( STRIP ${result1} Fortran_FINAL_FUNCTION_RESULT )

###############################################################

set(FINAL_UNINITIALIZED_LHS_SOURCE
"
module test2_mod
implicit none
public

type :: Object
  logical, private :: return = .false.
  logical, private :: initialized = .false.
  logical, private :: finalized = .false.
contains
  procedure, public :: copy => copy_f
  generic, public :: assignment(=) => copy
  final :: destructor
endtype

interface Object
  module procedure construct_Object
end interface


integer :: final_uninitialized = 0
integer :: final_return        = 0
integer :: final_initialized   = 0
integer :: final_total         = 0

contains

function construct_Object() result(this)
  type(Object) :: this
  this%initialized = .true.
  this%return = .true.
end function

subroutine copy_f(this,obj_in)
  class(Object), intent(inout) :: this
  class(Object), target, intent(in) :: obj_in
  this%initialized = obj_in%initialized
  this%return = .false.
end subroutine

subroutine destructor(this)
  type(Object), intent(inout) :: this
  final_total = final_total + 1
  
  if( .not. this%initialized ) then
    final_uninitialized = final_uninitialized+1    
  else
    if( this%return ) then
      final_return = final_return+1
    else
      final_initialized = final_initialized+1
    endif
  endif
end subroutine

end module

subroutine return_test
  use test2_mod
  implicit none
  type(Object) :: obj
  obj = Object()
end subroutine

program test2
  use test2_mod
  implicit none
  final_uninitialized = 0
  call return_test
  write(0,'(I0)')final_uninitialized
end program
")
ecbuild_check_fortran_source_return( 
  ${FINAL_UNINITIALIZED_LHS_SOURCE}
  VAR FINAL_UNINITIALIZED_LHS
  OUTPUT result2
)
string( STRIP ${result2} Fortran_FINAL_UNINITIALIZED_LHS )


#####################################################################

set(FINAL_UNINITIALIZED_INTENT_OUT_SOURCE
"
module test3_mod
implicit none
public

type :: Object
  logical, private :: return = .false.
  logical, private :: initialized = .false.
  logical, private :: finalized = .false.
contains
  procedure, public :: copy => copy_f
  generic, public :: assignment(=) => copy
  final :: destructor
endtype

interface Object
  module procedure construct_Object
end interface


integer :: final_uninitialized = 0
integer :: final_return        = 0
integer :: final_initialized   = 0
integer :: final_total         = 0

contains

function construct_Object() result(this)
  type(Object) :: this
  this%initialized = .true.
  this%return = .true.
end function

subroutine copy_f(this,obj_in)
  class(Object), intent(inout) :: this
  class(Object), target, intent(in) :: obj_in
  this%initialized = obj_in%initialized
  this%return = .false.
end subroutine

subroutine destructor(this)
  type(Object), intent(inout) :: this
  final_total = final_total + 1
  
  if( .not. this%initialized ) then
    final_uninitialized = final_uninitialized+1    
  else
    if( this%return ) then
      final_return = final_return+1
    else
      final_initialized = final_initialized+1
    endif
  endif
end subroutine

subroutine create_obj(obj)
implicit none
type(Object), intent(out) :: obj
obj = Object()
end subroutine

end module

subroutine return_test
  use test3_mod
  implicit none
  type(Object) :: obj
  call create_obj(obj)
end subroutine

program test3
  use test3_mod
  implicit none
  call return_test
  write(0,'(I0)')final_uninitialized
end program
")
ecbuild_check_fortran_source_return( 
  ${FINAL_UNINITIALIZED_INTENT_OUT_SOURCE}
  VAR FINAL_UNINITIALIZED_INTENT_OUT
  OUTPUT result3
)
string( STRIP ${result3} Fortran_FINAL_UNINITIALIZED_INTENT_OUT )

#######################################################################################

set(FINAL_UNINITIALIZED_INTENT_INOUT_SOURCE
"
module test4_mod
implicit none
public

type :: Object
  logical, private :: return = .false.
  logical, private :: initialized = .false.
  logical, private :: finalized = .false.
contains
  procedure, public :: copy => copy_f
  generic, public :: assignment(=) => copy
  final :: destructor
endtype

interface Object
  module procedure construct_Object
end interface


integer :: final_uninitialized = 0
integer :: final_return        = 0
integer :: final_initialized   = 0
integer :: final_total         = 0

contains

function construct_Object() result(this)
  type(Object) :: this
  this%initialized = .true.
  this%return = .true.
end function

subroutine copy_f(this,obj_in)
  class(Object), intent(inout) :: this
  class(Object), target, intent(in) :: obj_in
  this%initialized = obj_in%initialized
  this%return = .false.
end subroutine

subroutine destructor(this)
  type(Object), intent(inout) :: this
  final_total = final_total + 1
  
  if( .not. this%initialized ) then
    final_uninitialized = final_uninitialized+1    
  else
    if( this%return ) then
      final_return = final_return+1
    else
      final_initialized = final_initialized+1
    endif
  endif
end subroutine

subroutine create_obj(obj)
implicit none
type(Object), intent(inout) :: obj
obj = Object()
end subroutine

end module

subroutine return_test
  use test4_mod
  implicit none
  type(Object) :: obj
  call create_obj(obj)
end subroutine

program test4
  use test4_mod
  implicit none
  call return_test
  write(0,'(I0)')final_uninitialized
end program
")
ecbuild_check_fortran_source_return( 
  ${FINAL_UNINITIALIZED_INTENT_INOUT_SOURCE}
  VAR FINAL_UNINITIALIZED_INTENT_INOUT
  OUTPUT result4
)
string( STRIP ${result4} Fortran_FINAL_UNINITIALIZED_INTENT_INOUT )

endmacro()
