! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#include "fckit/fckit.h"

module fckit_configuration_module
  !! author: Willem Deconinck
  !!
  !! Module providing the [[fckit_configuration_module:fckit_configuration(type)]] type
  !!
  !! The [[fckit_configuration_module:fckit_configuration(type)]] type can be used to
  !! encapsulate name-value configurations, including nesting of subconfigurations.
  !!
  !! The [[fckit_configuration_module:fckit_YAMLConfiguration(interface)]] constructor
  !! for [[fckit_configuration_module:fckit_configuration(type)]] can create the
  !! configuration from a JSON file

use, intrinsic :: iso_c_binding, only : c_ptr, c_int32_t, c_int64_t, c_float, c_double, c_size_t, c_char
use fckit_shared_object_module, only : fckit_shared_object, fckit_c_deleter, fckit_c_nodeleter
use fckit_pathname_module, only : fckit_pathname
use fckit_buffer_module, only : fckit_buffer

implicit none

public  :: fckit_configuration
public  :: fckit_YAMLConfiguration
public  :: deallocate_fckit_configuration

private

#include "fckit_configuration.inc"

!----------------------------------------------------------------------------
TYPE, extends(fckit_shared_object) :: fckit_configuration
  !! Name-Value configuration type
  !!
  !! Names are strings, and values can be
  !!
  !! - basic types int32, int64, real32, real64, string, or arrays of these.
  !! - arrays of basic types
  !! - Subconfiguration, or arrays of subconfigurations
  !!
  !!#### Example construction
  !!
  !! From JSON or YAML file (all MPI tasks read)
  !!
  !!```fortran
  !! type(fckit_configuration) :: config
  !! config = fckit_YAMLConfiguration( fckit_pathname("filepath" )
  !!```
  !!
  !! From JSON or YAML file (only one MPI task reads, and broadcasts)
  !!
  !!```fortran
  !! type(fckit_configuration) :: config
  !! type(fckit_mpi_comm)      :: comm
  !! comm = fckit_mpi_comm("world")
  !! config = fckit_YAMLConfiguration( comm%broadcast_file("filepath", root=0) )
  !!```

contains

#if FCKIT_FINAL_NOT_INHERITING
  final :: fckit_configuration__final_auto
#endif

  procedure, public :: has
    !! Function that returns whether a name is contained in the configuration
    !!
    !!#### Example usage:
    !!
    !!```fortran
    !! if( .not. fckit_configuration%has('levels') ) call abort()
    !!```

  procedure, private :: set_config
  procedure, private :: set_config_list
  procedure, private :: set_logical
  procedure, private :: set_int32
  procedure, private :: set_int64
  procedure, private :: set_real32
  procedure, private :: set_real64
  procedure, private :: set_string
  procedure, private :: set_array_int32
  procedure, private :: set_array_int64
  procedure, private :: set_array_real32
  procedure, private :: set_array_real64

  !----------------------------------------------------------------------------
  !> Subroutine to set a name-value configuration
  !!
  !! Name is a string, and value any of the basic types, or subconfigurations
  !!
  !!#### Example usage
  !!
  !!```fortran
  !! type(fckit_configuration) :: config
  !! config = fckit_configuration()
  !! call config%set('grid','O1280')
  !! call config%set('levels',137)
  !!```
  !! Or with subconfiguration:
  !!
  !!```fortran
  !! type(fckit_configuration) :: config
  !! type(fckit_configuration) :: grid_config
  !!
  !! grid_config = fckit_configuration()
  !! call grid_config%set('type','reduced_gaussian')
  !! call grid_config%set('pl',[20,24,28,32,32,28,24,20])
  !! call grid_config%set('levels',137)
  !!
  !! config = fckit_configuration()
  !! call config%set('grid',grid_config)
  !!```
  generic, public :: set => &
    set_config, &
    set_config_list, &
    set_logical, &
    set_int32, &
    set_int64, &
    set_real32, &
    set_real64, &
    set_string, &
    set_array_int32, &
    set_array_int64, &
    set_array_real32, &
    set_array_real64

  procedure, private :: get_config
  procedure, private :: get_config_list
  procedure, private :: get_int32
  procedure, private :: get_int64
  procedure, private :: get_logical
  procedure, private :: get_real32
  procedure, private :: get_real64
  procedure, private :: get_string
  procedure, private :: get_array_int32
  procedure, private :: get_array_int64
  procedure, private :: get_array_real32
  procedure, private :: get_array_real64

  !----------------------------------------------------------------------------
  !> Function that gets a name-value configuration
  !!
  !! Name is a string, and value any of the basic types, or subconfigurations
  !!
  !! @Note
  !! This is a function that returns a logical which is ```.true.``` if the name is found
  !! in the configuration, and ```.false.``` otherwise.
  !! @Endnote
  !!
  !!#### Example usage
  !!
  !!```fortran
  !! type(fckit_configuration), intent(in) :: config
  !! character(kind=c_char,len=:), allocatable :: grid_id
  !! integer(c_int32_t) :: levels
  !! if( .not. config%get('grid',grid_id) ) then
  !!   grid_id = 'O1280' ! Default if not found
  !! endif
  !! if( .not. config%get('levels',levels) ) then
  !!   levels = 137 ! Default if not found
  !! endif
  !!```
  !! Or with subconfiguration:
  !!
  !!```fortran
  !! type(fckit_configuration), intent(in) :: config
  !! type(fckit_configuration) :: grid_config
  !! character(kind=c_char,len=:), allocatable :: grid_type
  !! integer(c_int32_t) :: grid_levels
  !! integer, allocatable :: grid_pl(:)
  !!
  !! if( .not. config%get('grid',grid_config) ) then
  !!    ! You could abort, or create a default grid_config:
  !!    grid_config = fckit_configuration()
  !!    call grid_config%set('type','reduced_gaussian')
  !!    call grid_config%set('pl',[20,24,28,32,32,28,24,20])
  !!    call grid_config%set('levels',137)
  !! endif
  !! if( .not. grid_config%get('type',grid_type) ) call abort()
  !! if( .not. grid_config%get('pl',grid_pl) )     call abort()
  !! if( .not. grid_config%get('levels',levels) )  call abort()
  !!```
  generic, public :: get => &
    get_config, &
    get_config_list, &
    get_int32, &
    get_int64, &
    get_logical, &
    get_real32, &
    get_real64, &
    get_string, &
    get_array_int32, &
    get_array_int64, &
    get_array_real32, &
    get_array_real64

    procedure, private :: get_config_or_die
    procedure, private :: get_config_list_or_die
    procedure, private :: get_int32_or_die
    procedure, private :: get_int64_or_die
    procedure, private :: get_logical_or_die
    procedure, private :: get_real32_or_die
    procedure, private :: get_real64_or_die
    procedure, private :: get_string_or_die
    procedure, private :: get_array_int32_or_die
    procedure, private :: get_array_int64_or_die
    procedure, private :: get_array_real32_or_die
    procedure, private :: get_array_real64_or_die

    !----------------------------------------------------------------------------
    !> Subroutine that gets a name-value configuration, and throws exception
    !! when not found
    !!
    !! Name is a string, and value any of the basic types, or subconfigurations
    !!
    !!#### Example usage
    !!
    !!```fortran
    !! type(fckit_configuration), intent(in) :: config
    !! character(kind=c_char,len=:), allocatable :: grid_id
    !! integer(c_int32_t) :: levels
    !! call config%get_or_die('grid',grid_id) )
    !! call config%get_or_die('levels',levels) )
    !!```
    !! Or with subconfiguration:
    !!
    !!```fortran
    !! type(fckit_configuration), intent(in) :: config
    !! type(fckit_configuration) :: grid_config
    !! character(kind=c_char,len=:), allocatable :: grid_type
    !! integer(c_int32_t) :: grid_levels
    !! integer, allocatable :: grid_pl(:)
    !!
    !! call config%get_or_die('grid',grid_config)
    !! call grid_config%get_or_die('type',grid_type) )
    !! call grid_config%get_or_die('pl',grid_pl) )
    !! call grid_config%get_or_die('levels',levels) )
    !!```
    generic :: get_or_die => &
      get_config_or_die, &
      get_config_list_or_die, &
      get_int32_or_die, &
      get_int64_or_die, &
      get_logical_or_die, &
      get_real32_or_die, &
      get_real64_or_die, &
      get_string_or_die, &
      get_array_int32_or_die, &
      get_array_int64_or_die, &
      get_array_real32_or_die, &
      get_array_real64_or_die

  procedure :: json
    !! Return a json string corresponding to this configuration
    !!
    !!#### Example usage
    !!
    !!```fortran
    !! type(fckit_configuration), intent(in) :: config
    !! character(kind=c_char,len=:), allocatable :: json_str
    !! json_str = config%json()
    !!```

END TYPE fckit_configuration

!------------------------------------------------------------------------------

interface fckit_configuration
  module procedure ctor
  module procedure ctor_from_cptr
end interface

interface fckit_YAMLConfiguration
  module procedure ctor_from_jsonfile
  module procedure ctor_from_jsonstr
  module procedure ctor_from_buffer
end interface

!------------------------------------------------------------------------------

private :: c_ptr, c_int32_t, c_int64_t, c_float, c_double, c_size_t, c_char
private :: fckit_shared_object
private :: fckit_c_deleter
private :: fckit_c_nodeleter
private :: fckit_pathname
private :: fckit_buffer

!========================================================
contains
!========================================================

subroutine throw_configuration_not_found( name )
  use fckit_c_interop_module, only : c_str
  character(kind=c_char,len=*), intent(in) :: name
  call c_fckit_throw_configuration_not_found(c_str(name))
end subroutine

! -----------------------------------------------------------------------------
! Config routines

subroutine deallocate_fckit_configuration( array )
  type(fckit_configuration), allocatable, intent(inout) :: array(:)
  integer(c_int32_t) :: j
  if( allocated(array) ) then
    do j=1,size(array)
#if FCKIT_FINAL_DEBUGGING
    write(0,'(A,I0,A)') "     + call array(",j,")%final()"
#endif
      call array(j)%final()
    enddo
#if FCKIT_FINAL_DEBUGGING
    write(0,*) "    + deallocate(array)"
#endif
    deallocate(array)
  endif
end subroutine

FCKIT_FINAL subroutine fckit_configuration__final_auto(this)
  type(fckit_configuration), intent(inout) :: this
#if FCKIT_FINAL_DEBUGGING
  write(0,*) "fckit_configuration__final_auto"
#endif
#if FCKIT_FINAL_NOT_PROPAGATING
  call this%final()
#endif
  FCKIT_SUPPRESS_UNUSED( this )
end subroutine

function ctor() result(this)
  type(fckit_Configuration) :: this
  call this%reset_c_ptr( c_fckit_configuration_new(), &
    & fckit_c_deleter(c_fckit_configuration_delete) )
  call this%return()
end function

function ctor_from_cptr(cptr,own) result(this)
  type(c_ptr), value :: cptr
  type(fckit_Configuration) :: this
  logical, optional :: own
  logical :: opt_own
  opt_own = .false.
  if( present(own) ) opt_own = own
  if( opt_own ) then
    call this%reset_c_ptr( cptr, fckit_c_deleter(c_fckit_configuration_delete) )
  else
    call this%reset_c_ptr( cptr, fckit_c_nodeleter() )
  endif
  call this%return()
end function


function ctor_from_jsonstr(json) result(this)
  use fckit_c_interop_module, only : c_str
  type(fckit_Configuration) :: this
  character(kind=c_char,len=*), intent(in) :: json
  call this%reset_c_ptr( c_fckit_configuration_new_from_json(c_str(json)), &
    & fckit_c_deleter(c_fckit_configuration_delete) )
  call this%return()
end function

function ctor_from_jsonfile(path) result(this)
  use fckit_c_interop_module, only : c_str
  type(fckit_Configuration) :: this
  type(fckit_pathname), intent(in) :: path
  call this%reset_c_ptr( c_fckit_configuration_new_from_file(c_str(path%str())), &
    & fckit_c_deleter(c_fckit_configuration_delete) )
  call this%return()
end function

function ctor_from_buffer(buffer) result(this)
  use fckit_c_interop_module, only : c_str
  type(fckit_Configuration) :: this
  type(fckit_buffer), intent(in) :: buffer
  call this%reset_c_ptr( c_fckit_configuration_new_from_buffer(buffer%CPTR_PGIBUG_B), &
    & fckit_c_deleter(c_fckit_configuration_delete) )
  call buffer%consumed() ! If buffer was constructed inline, this will delete the buffer
  call this%return()
end function


function has(this, name) result(value)
  use fckit_c_interop_module, only : c_str
  class(fckit_Configuration), intent(inout) :: this
  character(kind=c_char,len=*), intent(in) :: name
  logical :: value
  integer(c_int32_t) :: value_int
  value_int =  c_fckit_configuration_has(this%CPTR_PGIBUG_B, c_str(name) )
  if( value_int == 1 ) then
    value = .True.
  else
    value = .False.
  end if
end function

subroutine set_config(this, name, value)
  use fckit_c_interop_module, only : c_str
  class(fckit_Configuration), intent(inout) :: this
  character(kind=c_char,len=*), intent(in) :: name
  class(fckit_Configuration), intent(in) :: value
  call c_fckit_configuration_set_config(this%CPTR_PGIBUG_B, c_str(name), value%CPTR_PGIBUG_B )
end subroutine

subroutine set_config_list(this, name, value)
  use, intrinsic :: iso_c_binding, only : c_loc
  use fckit_c_interop_module, only : c_str
  class(fckit_Configuration), intent(inout) :: this
  character(kind=c_char,len=*), intent(in) :: name
  type(fckit_Configuration), intent(in) :: value(:) !PGI (17.7) compiler bug when "type" replaced with "class"
  type(c_ptr), target :: value_cptrs(size(value))
  integer(c_int32_t) :: j
  if( size(value) > 0 ) then
    do j=1,size(value)
      value_cptrs(j) = value(j)%CPTR_PGIBUG_B
    enddo
    call c_fckit_configuration_set_config_list(this%CPTR_PGIBUG_B, c_str(name),  &
      c_loc(value_cptrs(1)), size(value_cptrs,kind=c_size_t) )
  endif
end subroutine


subroutine set_logical(this, name, value)
  use fckit_c_interop_module, only : c_str
  class(fckit_Configuration), intent(inout) :: this
  character(kind=c_char,len=*), intent(in) :: name
  logical, intent(in) :: value
  integer(c_int32_t) :: value_int
  if( value ) then
    value_int = 1
  else
    value_int = 0
  end if
  call c_fckit_configuration_set_int32(this%CPTR_PGIBUG_B, c_str(name), value_int )
end subroutine

subroutine set_int32(this, name, value)
  use fckit_c_interop_module, only : c_str
  class(fckit_Configuration), intent(inout) :: this
  character(kind=c_char,len=*), intent(in) :: name
  integer(c_int32_t), intent(in) :: value
  call c_fckit_configuration_set_int32(this%CPTR_PGIBUG_B, c_str(name), value)
end subroutine

subroutine set_int64(this, name, value)
  use fckit_c_interop_module, only : c_str
  class(fckit_Configuration), intent(inout) :: this
  character(kind=c_char,len=*), intent(in) :: name
  integer(c_int64_t), intent(in) :: value
  call c_fckit_configuration_set_int64(this%CPTR_PGIBUG_B, c_str(name), value)
end subroutine

subroutine set_real32(this, name, value)
  use fckit_c_interop_module, only : c_str
  class(fckit_Configuration), intent(inout) :: this
  character(kind=c_char,len=*), intent(in) :: name
  real(c_float), intent(in) :: value
  call c_fckit_configuration_set_float(this%CPTR_PGIBUG_B, c_str(name) ,value)
end subroutine

subroutine set_real64(this, name, value)
  use fckit_c_interop_module, only : c_str
  class(fckit_Configuration), intent(inout) :: this
  character(kind=c_char,len=*), intent(in) :: name
  real(c_double), intent(in) :: value
  call c_fckit_configuration_set_double(this%CPTR_PGIBUG_B, c_str(name) ,value)
end subroutine

subroutine set_string(this, name, value)
  use fckit_c_interop_module, only : c_str
  class(fckit_Configuration), intent(inout) :: this
  character(kind=c_char,len=*), intent(in) :: name
  character(kind=c_char,len=*), intent(in) :: value
  call c_fckit_configuration_set_string(this%CPTR_PGIBUG_B, c_str(name) , c_str(value) )
end subroutine

subroutine set_array_int32(this, name, value)
  use fckit_c_interop_module, only : c_str
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  integer(c_int32_t), intent(in) :: value(:)
  call c_fckit_configuration_set_array_int32(this%CPTR_PGIBUG_B, c_str(name), value, size(value,kind=c_size_t) )
end subroutine

subroutine set_array_int64(this, name, value)
  use fckit_c_interop_module, only : c_str
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  integer(c_int64_t), intent(in) :: value(:)
  call c_fckit_configuration_set_array_int64(this%CPTR_PGIBUG_B, c_str(name), value, size(value,kind=c_size_t) )
end subroutine

subroutine set_array_real32(this, name, value)
  use fckit_c_interop_module, only : c_str
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  real(c_float), intent(in) :: value(:)
  call c_fckit_configuration_set_array_float(this%CPTR_PGIBUG_B, c_str(name), value, size(value,kind=c_size_t) )
end subroutine

subroutine set_array_real64(this, name, value)
  use fckit_c_interop_module, only : c_str
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  real(c_double), intent(in) :: value(:)
  call c_fckit_configuration_set_array_double(this%CPTR_PGIBUG_B, c_str(name), value, size(value,kind=c_size_t) )
end subroutine

function get_config(this, name, value) result(found)
  use fckit_c_interop_module, only : c_str
  logical :: found
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  class(fckit_Configuration), intent(inout) :: value
  integer(c_int32_t) :: found_int
  value = fckit_Configuration()
  found_int = c_fckit_configuration_get_config(this%CPTR_PGIBUG_B, &
    c_str(name), value%CPTR_PGIBUG_B )
  found = .False.
  if (found_int == 1) then
    found = .True.
  endif
end function

subroutine get_config_or_die(this,name,value)
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  class(fckit_Configuration), intent(inout) :: value
  if( .not. this%get(name,value) ) call throw_configuration_not_found(name)
end subroutine

function get_config_list(this, name, value) result(found)
  use, intrinsic :: iso_c_binding, only : c_f_pointer, c_null_ptr
  use fckit_c_interop_module, only : c_str, c_ptr_free
  logical :: found
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  type(fckit_Configuration), allocatable, intent(inout) :: value(:)
  type(c_ptr) :: value_list_cptr
  type(c_ptr), pointer :: value_cptrs(:)
  integer(c_size_t) :: value_list_size
  integer(c_int32_t) :: found_int
  integer(c_int32_t) :: j
  call deallocate_fckit_configuration(value)
  value_list_cptr = c_null_ptr
  found_int = c_fckit_configuration_get_config_list(this%CPTR_PGIBUG_B, c_str(name), &
    & value_list_cptr, value_list_size)
  found = .False.
  if( found_int == 1 ) then
    found = .true.
    call c_f_pointer(value_list_cptr,value_cptrs,(/value_list_size/))
    allocate(value(value_list_size))
    do j=1,value_list_size
      call value(j)%reset_c_ptr( value_cptrs(j), fckit_c_deleter(c_fckit_configuration_delete) )
    enddo
  endif
end function

subroutine get_config_list_or_die(this,name,value)
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  type(fckit_Configuration), allocatable, intent(inout) :: value(:)
  if( .not. this%get(name,value) ) call throw_configuration_not_found(name)
end subroutine

function get_logical(this, name, value) result(found)
  use fckit_c_interop_module, only : c_str
  logical :: found
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  logical, intent(inout) :: value
  integer(c_int32_t) :: value_int
  integer(c_int32_t) :: found_int
  found_int = c_fckit_configuration_get_int32(this%CPTR_PGIBUG_B,c_str(name), value_int )
  found = .False.
  if (found_int == 1) found = .True.
  if (found) then
    if (value_int > 0) then
      value = .True.
    else
      value = .False.
    end if
  endif
end function

subroutine get_logical_or_die(this,name,value)
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  logical, intent(inout) :: value
  if( .not. this%get(name,value) ) call throw_configuration_not_found(name)
end subroutine

function get_int32(this, name, value) result(found)
  use fckit_c_interop_module, only : c_str
  logical :: found
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  integer(c_int32_t), intent(inout) :: value
  integer(c_int32_t) :: found_int
  found_int = c_fckit_configuration_get_int32(this%CPTR_PGIBUG_B, c_str(name), value )
  found = .False.
  if (found_int == 1) found = .True.
end function

subroutine get_int32_or_die(this,name,value)
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  integer(c_int32_t), intent(inout) :: value
  if( .not. this%get(name,value) ) call throw_configuration_not_found(name)
end subroutine

function get_int64(this, name, value) result(found)
  use fckit_c_interop_module, only : c_str
  logical :: found
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  integer(c_int64_t), intent(inout) :: value
  integer(c_int32_t) :: found_int
  found_int = c_fckit_configuration_get_int64(this%CPTR_PGIBUG_B, c_str(name), value )
  found = .False.
  if (found_int == 1) found = .True.
end function

subroutine get_int64_or_die(this,name,value)
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  integer(c_int64_t), intent(inout) :: value
  if( .not. this%get(name,value) ) call throw_configuration_not_found(name)
end subroutine

function get_real32(this, name, value) result(found)
  use fckit_c_interop_module, only : c_str
  logical :: found
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  real(c_float), intent(inout) :: value
  integer(c_int32_t) :: found_int
  found_int = c_fckit_configuration_get_float(this%CPTR_PGIBUG_B, c_str(name), value )
  found = .False.
  if (found_int == 1) found = .True.
end function

subroutine get_real32_or_die(this,name,value)
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  real(c_float), intent(inout) :: value
  if( .not. this%get(name,value) ) call throw_configuration_not_found(name)
end subroutine

function get_real64(this, name, value) result(found)
  use fckit_c_interop_module, only : c_str
  logical :: found
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  real(c_double), intent(inout) :: value
  integer(c_int32_t) :: found_int
  found_int = c_fckit_configuration_get_double(this%CPTR_PGIBUG_B, c_str(name), value )
  found = .False.
  if (found_int == 1) found = .True.
end function

subroutine get_real64_or_die(this,name,value)
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  real(c_double), intent(inout) :: value
  if( .not. this%get(name,value) ) call throw_configuration_not_found(name)
end subroutine

function get_string(this, name, value) result(found)
  use fckit_c_interop_module, only : c_str, c_ptr_to_string, c_ptr_free
  logical :: found
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  character(kind=c_char,len=:), allocatable, intent(inout) :: value
  type(c_ptr) :: value_cptr
  integer(c_int32_t) :: found_int
  integer(c_size_t) :: value_size
  found_int = c_fckit_configuration_get_string(this%CPTR_PGIBUG_B,c_str(name),value_cptr,value_size)
  if( found_int == 1 ) then
    if( allocated(value) ) deallocate(value)
    allocate(character(kind=c_char,len=value_size) :: value )
    value = c_ptr_to_string(value_cptr)
    call c_ptr_free(value_cptr)
  endif
  found = .False.
  if (found_int == 1) found = .True.
end function

subroutine get_string_or_die(this,name,value)
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  character(kind=c_char,len=:), allocatable, intent(inout) :: value
  if( .not. this%get(name,value) ) call throw_configuration_not_found(name)
end subroutine

function get_array_int32(this, name, value) result(found)
  use, intrinsic :: iso_c_binding, only : c_f_pointer
  use fckit_c_interop_module, only : c_str, c_ptr_free
  logical :: found
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  integer(c_int32_t), allocatable, intent(inout) :: value(:)
  type(c_ptr) :: value_cptr
  integer(c_int32_t), pointer :: value_fptr(:)
  integer(c_size_t) :: value_size
  integer(c_int32_t) :: found_int
  found_int = c_fckit_configuration_get_array_int32(this%CPTR_PGIBUG_B, c_str(name), &
   & value_cptr, value_size )
  if (found_int ==1 ) then
    call c_f_pointer(value_cptr,value_fptr,(/value_size/))
    if( allocated(value) ) deallocate(value)
    allocate(value(value_size))
    value(:) = value_fptr(:)
    call c_ptr_free(value_cptr)
  endif
  found = .False.
  if (found_int == 1) found = .True.
end function

subroutine get_array_int32_or_die(this,name,value)
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  integer(c_int32_t), allocatable, intent(inout) :: value(:)
  if( .not. this%get(name,value) ) call throw_configuration_not_found(name)
end subroutine

function get_array_int64(this, name, value) result(found)
  use, intrinsic :: iso_c_binding, only : c_f_pointer
  use fckit_c_interop_module, only : c_str, c_ptr_free
  logical :: found
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  integer(c_int64_t), allocatable, intent(inout) :: value(:)
  type(c_ptr) :: value_cptr
  integer(c_int64_t), pointer :: value_fptr(:)
  integer(c_size_t) :: value_size
  integer(c_int32_t) :: found_int
  found_int = c_fckit_configuration_get_array_int64(this%CPTR_PGIBUG_B, c_str(name), &
   & value_cptr, value_size )
  if (found_int == 1) then
    call c_f_pointer(value_cptr,value_fptr,(/value_size/))
    if( allocated(value) ) deallocate(value)
    allocate(value(value_size))
    value(:) = value_fptr(:)
    call c_ptr_free(value_cptr)
  endif
  found = .False.
  if (found_int == 1) found = .True.
end function

subroutine get_array_int64_or_die(this,name,value)
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  integer(c_int64_t), allocatable, intent(inout) :: value(:)
  if( .not. this%get(name,value) ) call throw_configuration_not_found(name)
end subroutine

function get_array_real32(this, name, value) result(found)
  use, intrinsic :: iso_c_binding, only : c_f_pointer
  use fckit_c_interop_module, only : c_str, c_ptr_free
  logical :: found
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  real(c_float), allocatable, intent(inout) :: value(:)
  type(c_ptr) :: value_cptr
  real(c_float), pointer :: value_fptr(:)
  integer(c_size_t) :: value_size
  integer(c_int32_t) :: found_int
  found_int = c_fckit_configuration_get_array_float(this%CPTR_PGIBUG_B, c_str(name), &
   & value_cptr, value_size )
  if (found_int == 1 ) then
    call c_f_pointer(value_cptr,value_fptr,(/value_size/))
    if( allocated(value) ) deallocate(value)
    allocate(value(value_size))
    value(:) = value_fptr(:)
    call c_ptr_free(value_cptr)
  endif
  found = .False.
  if (found_int == 1) found = .True.
end function

subroutine get_array_real32_or_die(this,name,value)
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  real(c_float), allocatable, intent(inout) :: value(:)
  if( .not. this%get(name,value) ) call throw_configuration_not_found(name)
end subroutine

function get_array_real64(this, name, value) result(found)
  use, intrinsic :: iso_c_binding, only : c_f_pointer
  use fckit_c_interop_module, only : c_str, c_ptr_free
  logical :: found
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  real(c_double), allocatable, intent(inout) :: value(:)
  type(c_ptr) :: value_cptr
  real(c_double), pointer :: value_fptr(:)
  integer(c_size_t) :: value_size
  integer(c_int32_t) :: found_int
  found_int = c_fckit_configuration_get_array_double(this%CPTR_PGIBUG_B, c_str(name), &
   & value_cptr, value_size )
  if (found_int == 1) then
    call c_f_pointer(value_cptr,value_fptr,(/value_size/))
    if( allocated(value) ) deallocate(value)
    allocate(value(value_size))
    value(:) = value_fptr(:)
    call c_ptr_free(value_cptr)
  endif
  found = .False.
  if (found_int == 1) found = .True.
end function

subroutine get_array_real64_or_die(this,name,value)
  class(fckit_Configuration), intent(in) :: this
  character(kind=c_char,len=*), intent(in) :: name
  real(c_double), allocatable, intent(inout) :: value(:)
  if( .not. this%get(name,value) ) call throw_configuration_not_found(name)
end subroutine

function json(this) result(jsonstr)
  use fckit_c_interop_module, only : c_str, c_ptr_to_string, c_ptr_free
  character(kind=c_char,len=:), allocatable :: jsonstr
  class(fckit_Configuration), intent(in) :: this
  type(c_ptr) :: json_cptr
  integer(c_size_t) :: json_size
  call c_fckit_configuration_json(this%CPTR_PGIBUG_B,json_cptr,json_size)
  allocate(character(kind=c_char,len=json_size) :: jsonstr )
  jsonstr = c_ptr_to_string(json_cptr)
  call c_ptr_free(json_cptr)
end function

end module fckit_configuration_module

