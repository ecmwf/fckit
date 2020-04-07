! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#ifndef FORD
interface

!-------------------------------------------------------------------------------
! void c_fckit_throw_configuration_not_found (const char* name)
!-------------------------------------------------------------------------------
subroutine c_fckit_throw_configuration_not_found( name ) bind(C,name="c_fckit_t&
  &hrow_configuration_not_found")
    use iso_c_binding, only: c_char
    character(c_char), dimension(*) :: name
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Configuration* c_fckit_configuration_new ()
!-------------------------------------------------------------------------------
function c_fckit_configuration_new() bind(C,name="c_fckit_configuration_new")
    use iso_c_binding, only: c_ptr
    type(c_ptr) :: c_fckit_configuration_new
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Configuration* c_fckit_configuration_new_from_json (const char* json)
!-------------------------------------------------------------------------------
function c_fckit_configuration_new_from_json( json ) bind(C,name="c_fckit_confi&
  &guration_new_from_json")
    use iso_c_binding, only: c_ptr, c_char
    type(c_ptr) :: c_fckit_configuration_new_from_json
    character(c_char), dimension(*) :: json
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Configuration* c_fckit_configuration_new_from_file (const char* path)
!-------------------------------------------------------------------------------
function c_fckit_configuration_new_from_file( path ) bind(C,name="c_fckit_confi&
  &guration_new_from_file")
    use iso_c_binding, only: c_ptr, c_char
    type(c_ptr) :: c_fckit_configuration_new_from_file
    character(c_char), dimension(*) :: path
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Configuration* c_fckit_configuration_new_from_buffer (eckit::Buffer* buffer)
!-------------------------------------------------------------------------------
function c_fckit_configuration_new_from_buffer( buffer ) bind(C,name="c_fckit_c&
  &onfiguration_new_from_buffer")
    use iso_c_binding, only: c_ptr
    type(c_ptr) :: c_fckit_configuration_new_from_buffer
    type(c_ptr), value :: buffer
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_delete (Configuration* This)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_delete( This ) bind(C,name="c_fckit_configurat&
  &ion_delete")
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: This
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_set_config (Configuration* This, const char* name,
!   const Configuration* value)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_config( This, name, value ) bind(C,name="c&
  &_fckit_configuration_set_config")
    use iso_c_binding, only: c_ptr, c_char
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    type(c_ptr), value :: value
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_set_config_list (Configuration* This, const char* n
!   ame, const Configuration* value[], size_t size)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_config_list( This, name, value, size ) bin&
  &d(C,name="c_fckit_configuration_set_config_list")
    use iso_c_binding, only: c_int32_t, c_size_t, c_ptr, c_char
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    type(c_ptr), value :: value
    integer(c_size_t), value :: size
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_set_bool (Configuration* This, const char* name,
!                                       int32 value)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_bool( This, name, value ) bind(C,name="c_&
 &fckit_configuration_set_bool")
   use iso_c_binding, only: c_int32_t, c_ptr, c_char
   type(c_ptr), value :: This
   character(c_char), dimension(*) :: name
   integer(c_int32_t), value :: value
end subroutine
!-------------------------------------------------------------------------------

                                               !-------------------------------------------------------------------------------
! void c_fckit_configuration_set_int32 (Configuration* This, const char* name,
!                                       int32 value)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_int32( This, name, value ) bind(C,name="c_&
  &fckit_configuration_set_int32")
    use iso_c_binding, only: c_int32_t, c_ptr, c_char
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    integer(c_int32_t), value :: value
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_set_int64 (Configuration* This, const char* name,
!                                       int64 value)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_int64( This, name, value ) bind(C,name="c_&
  &fckit_configuration_set_int64")
    use iso_c_binding, only: c_ptr, c_int64_t, c_char
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    integer(c_int64_t), value :: value
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_set_float (Configuration* This, const char* name, f
!   loat value)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_float( This, name, value ) bind(C,name="c_&
  &fckit_configuration_set_float")
    use iso_c_binding, only: c_ptr, c_char, c_float
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    real(c_float), value :: value
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_set_double (Configuration* This, const char* name,
!   double value)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_double( This, name, value ) bind(C,name="c&
  &_fckit_configuration_set_double")
    use iso_c_binding, only: c_ptr, c_char, c_double
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    real(c_double), value :: value
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_set_string (Configuration* This, const char* name,
!   const char* value)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_string( This, name, value ) bind(C,name="c&
  &_fckit_configuration_set_string")
    use iso_c_binding, only: c_ptr, c_char
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    character(c_char), dimension(*) :: value
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_set_array_string (Configuration* This, const char* n
!   ame, const char* value, size_t size)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_array_string( This, name, value, length, size ) bin&
  &d(C,name="c_fckit_configuration_set_array_string")
    use iso_c_binding, only: c_int32_t, c_size_t, c_ptr, c_char
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    character(c_char), dimension(*) :: value
    integer(c_size_t), value :: length
    integer(c_size_t), value :: size
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_set_array_int32 (Configuration* This, const char* n
!   ame, int32 value[], size_t size)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_array_int32( This, name, value, size ) bin&
  &d(C,name="c_fckit_configuration_set_array_int32")
    use iso_c_binding, only: c_int32_t, c_size_t, c_ptr, c_char
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    integer(c_int32_t), dimension(*) :: value
    integer(c_size_t), value :: size
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_set_array_int64 (Configuration* This, const char* n
!   ame, int64 value[], size_t size)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_array_int64( This, name, value, size ) bin&
  &d(C,name="c_fckit_configuration_set_array_int64")
    use iso_c_binding, only: c_size_t, c_ptr, c_int64_t, c_char
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    integer(c_int64_t), dimension(*) :: value
    integer(c_size_t), value :: size
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_set_array_float (Configuration* This, const char* n
!   ame, float value[], size_t size)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_array_float( This, name, value, size ) bin&
  &d(C,name="c_fckit_configuration_set_array_float")
    use iso_c_binding, only: c_size_t, c_ptr, c_char, c_float
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    real(c_float), dimension(*) :: value
    integer(c_size_t), value :: size
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_set_array_double (Configuration* This, const char*
!   name, double value[], size_t size)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_array_double( This, name, value, size ) bi&
  &nd(C,name="c_fckit_configuration_set_array_double")
    use iso_c_binding, only: c_size_t, c_ptr, c_char, c_double
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    real(c_double), dimension(*) :: value
    integer(c_size_t), value :: size
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int32 c_fckit_configuration_get_config (const Configuration* This, const char*
!   name, LocalConfiguration* value)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_config( This, name, value ) bind(C,name="c_f&
  &ckit_configuration_get_config")
    use iso_c_binding, only: c_char, c_ptr, c_int32_t
    integer(c_int32_t) :: c_fckit_configuration_get_config
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    type(c_ptr), value :: value
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int32 c_fckit_configuration_get_config_list (const Configuration* This, const
!   char* name, LocalConfiguration** &value, size_t &size)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_config_list( This, name, value, size ) bind(&
  &C,name="c_fckit_configuration_get_config_list")
    use iso_c_binding, only: c_char, c_ptr, c_int32_t, c_size_t
    integer(c_int32_t) :: c_fckit_configuration_get_config_list
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    type(c_ptr) :: value
    integer(c_size_t) :: size
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int32 c_fckit_configuration_get_bool (const Configuration* This, const char*
!   name, int32& value)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_bool( This, name, value ) bind(C,name="c_fc&
  &kit_configuration_get_bool")
    use iso_c_binding, only: c_char, c_ptr, c_int32_t
    integer(c_int32_t) :: c_fckit_configuration_get_bool
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    integer(c_int32_t) :: value
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int32 c_fckit_configuration_get_int32 (const Configuration* This, const char*
!   name, int32& value)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_int32( This, name, value ) bind(C,name="c_fc&
  &kit_configuration_get_int32")
    use iso_c_binding, only: c_char, c_ptr, c_int32_t
    integer(c_int32_t) :: c_fckit_configuration_get_int32
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    integer(c_int32_t) :: value
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int32 c_fckit_configuration_get_int64 (const Configuration* This, const char*
!   name, int64& value)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_int64( This, name, value ) bind(C,name="c_fc&
  &kit_configuration_get_int64")
    use iso_c_binding, only: c_char, c_ptr, c_int64_t, c_int32_t
    integer(c_int32_t) :: c_fckit_configuration_get_int64
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    integer(c_int64_t) :: value
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int32 c_fckit_configuration_get_float (const Configuration* This, const char*
!   name, float& value)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_float( This, name, value ) bind(C,name="c_fc&
  &kit_configuration_get_float")
    use iso_c_binding, only: c_char, c_ptr, c_int32_t, c_float
    integer(c_int32_t) :: c_fckit_configuration_get_float
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    real(c_float) :: value
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int32 c_fckit_configuration_get_double (const Configuration* This, const char*
!   name, double& value)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_double( This, name, value ) bind(C,name="c_f&
  &ckit_configuration_get_double")
    use iso_c_binding, only: c_char, c_ptr, c_int32_t, c_double
    integer(c_int32_t) :: c_fckit_configuration_get_double
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    real(c_double) :: value
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int32 c_fckit_configuration_get_string( const Configuration* This, const char*
!   name, char* &value, size_t &size)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_string( This, name, value, size ) bind(C,nam&
  &e="c_fckit_configuration_get_string")
    use iso_c_binding, only: c_char, c_ptr, c_int32_t, c_size_t
    integer(c_int32_t) :: c_fckit_configuration_get_string
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    type(c_ptr) :: value
    integer(c_size_t) :: size
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int32 c_fckit_configuration_get_string_element( const Configuration* This, const char*
!   name, size_t index, char* &value, size_t &size)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_string_element( This, name, index, value, size ) bind(C,nam&
  &e="c_fckit_configuration_get_string_element")
    use iso_c_binding, only: c_char, c_ptr, c_int32_t, c_size_t
    integer(c_int32_t) :: c_fckit_configuration_get_string_element
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    integer(c_size_t) :: index
    type(c_ptr) :: value
    integer(c_size_t) :: size
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int32 c_fckit_configuration_get_array_int32 (const Configuration* This, const
!   char* name, int32* &value, size_t& size)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_array_int32( This, name, value, size ) bind(&
  &C,name="c_fckit_configuration_get_array_int32")
    use iso_c_binding, only: c_char, c_ptr, c_int32_t, c_size_t
    integer(c_int32_t) :: c_fckit_configuration_get_array_int32
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    type(c_ptr) :: value
    integer(c_size_t) :: size
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int64 c_fckit_configuration_get_array_int64 (const Configuration* This, const
!   char* name, int64* &value, size_t& size)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_array_int64( This, name, value, size ) bind(C&
  &,name="c_fckit_configuration_get_array_int64")
    use iso_c_binding, only: c_char, c_ptr, c_int32_t, c_size_t
    integer(c_int32_t) :: c_fckit_configuration_get_array_int64
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    type(c_ptr) :: value
    integer(c_size_t) :: size
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int32 c_fckit_configuration_get_array_float (const Configuration* This, const ch
!   ar* name, float* &value, size_t& size)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_array_float( This, name, value, size ) bind(&
  &C,name="c_fckit_configuration_get_array_float")
    use iso_c_binding, only: c_char, c_ptr, c_int32_t, c_size_t
    integer(c_int32_t) :: c_fckit_configuration_get_array_float
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    type(c_ptr) :: value
    integer(c_size_t) :: size
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int32 c_fckit_configuration_get_array_double (const Configuration* This, const
!   char* name, double* &value, size_t& size)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_array_double( This, name, value, size ) bind&
  &(C,name="c_fckit_configuration_get_array_double")
    use iso_c_binding, only: c_char, c_ptr, c_int32_t, c_size_t
    integer(c_int32_t) :: c_fckit_configuration_get_array_double
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    type(c_ptr) :: value
    integer(c_size_t) :: size
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int32 c_fckit_configuration_get_array_string (const Configuration* This, const c
!   har* name, char* &value, size_t& size, size_t* &offsets, size_t& numelem)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_array_string( This, name, value, size, offsets, numelem) bind&
  &(C,name="c_fckit_configuration_get_array_string")
    use iso_c_binding, only: c_char, c_ptr, c_int32_t, c_size_t
    integer(c_int32_t) :: c_fckit_configuration_get_array_string
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    type(c_ptr) :: value
    integer(c_size_t) :: size
    type(c_ptr) :: offsets
    integer(c_size_t) :: numelem
end function
!----------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int32 c_fckit_configuration_has (const Configuration* This, const char *name)
!-------------------------------------------------------------------------------
function c_fckit_configuration_has( This, name ) bind(C,name="c_fckit_configura&
  &tion_has")
    use iso_c_binding, only: c_char, c_ptr, c_int32_t
    integer(c_int32_t) :: c_fckit_configuration_has
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int32 c_fckit_configuration_get_size (const Configuration* This, const char *name)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_size( This, name ) bind(C,name="c_fckit_configura&
  &tion_get_size")
    use iso_c_binding, only: c_char, c_ptr, c_int32_t
    integer(c_int32_t) :: c_fckit_configuration_get_size
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_json(const Configuration* This, char* &json, int &s
!   ize)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_json( This, json, size ) bind(C,name="c_fckit_&
  &configuration_json")
    use iso_c_binding, only: c_ptr, c_size_t
    type(c_ptr), value :: This
    type(c_ptr) :: json
    integer(c_size_t) :: size
end subroutine
!-------------------------------------------------------------------------------

end interface
#endif
