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
! const Configuration* c_fckit_configuration_new_from_file (const char* path)
!-------------------------------------------------------------------------------
function c_fckit_configuration_new_from_file( path ) bind(C,name="c_fckit_confi&
  &guration_new_from_file")
    use iso_c_binding, only: c_ptr, c_char
    type(c_ptr) :: c_fckit_configuration_new_from_file
    character(c_char), dimension(*) :: path
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
!   ame, const Configuration* value[], int size)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_config_list( This, name, value, size ) bin&
  &d(C,name="c_fckit_configuration_set_config_list")
    use iso_c_binding, only: c_int, c_ptr, c_char
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    type(c_ptr), value :: value
    integer(c_int), value :: size
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_set_int (Configuration* This, const char* name, int
!    value)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_int( This, name, value ) bind(C,name="c_fc&
  &kit_configuration_set_int")
    use iso_c_binding, only: c_int, c_ptr, c_char
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    integer(c_int), value :: value
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_set_long (Configuration* This, const char* name, lo
!   ng value)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_long( This, name, value ) bind(C,name="c_f&
  &ckit_configuration_set_long")
    use iso_c_binding, only: c_ptr, c_long, c_char
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    integer(c_long), value :: value
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
! void c_fckit_configuration_set_array_int (Configuration* This, const char* nam
!   e, int value[], int size)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_array_int( This, name, value, size ) bind(&
  &C,name="c_fckit_configuration_set_array_int")
    use iso_c_binding, only: c_int, c_ptr, c_char
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    integer(c_int), dimension(*) :: value
    integer(c_int), value :: size
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_set_array_long (Configuration* This, const char* na
!   me, long value[], int size)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_array_long( This, name, value, size ) bind&
  &(C,name="c_fckit_configuration_set_array_long")
    use iso_c_binding, only: c_int, c_ptr, c_long, c_char
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    integer(c_long), dimension(*) :: value
    integer(c_int), value :: size
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_set_array_float (Configuration* This, const char* n
!   ame, float value[], int size)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_array_float( This, name, value, size ) bin&
  &d(C,name="c_fckit_configuration_set_array_float")
    use iso_c_binding, only: c_int, c_ptr, c_char, c_float
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    real(c_float), dimension(*) :: value
    integer(c_int), value :: size
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! void c_fckit_configuration_set_array_double (Configuration* This, const char* 
!   name, double value[], int size)
!-------------------------------------------------------------------------------
subroutine c_fckit_configuration_set_array_double( This, name, value, size ) bi&
  &nd(C,name="c_fckit_configuration_set_array_double")
    use iso_c_binding, only: c_int, c_ptr, c_char, c_double
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    real(c_double), dimension(*) :: value
    integer(c_int), value :: size
end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int c_fckit_configuration_get_config (const Configuration* This, const char* n
!   ame, LocalConfiguration* value)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_config( This, name, value ) bind(C,name="c_f&
  &ckit_configuration_get_config")
    use iso_c_binding, only: c_char, c_ptr, c_int
    integer(c_int) :: c_fckit_configuration_get_config
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    type(c_ptr), value :: value
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int c_fckit_configuration_get_config_list (const Configuration* This, const ch
!   ar* name, LocalConfiguration** &value, int &size)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_config_list( This, name, value, size ) bind(&
  &C,name="c_fckit_configuration_get_config_list")
    use iso_c_binding, only: c_char, c_ptr, c_int
    integer(c_int) :: c_fckit_configuration_get_config_list
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    type(c_ptr) :: value
    integer(c_int) :: size
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int c_fckit_configuration_get_int (const Configuration* This, const char* name
!   , int& value)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_int( This, name, value ) bind(C,name="c_fcki&
  &t_configuration_get_int")
    use iso_c_binding, only: c_char, c_ptr, c_int
    integer(c_int) :: c_fckit_configuration_get_int
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    integer(c_int) :: value
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int c_fckit_configuration_get_long (const Configuration* This, const char* nam
!   e, long& value)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_long( This, name, value ) bind(C,name="c_fck&
  &it_configuration_get_long")
    use iso_c_binding, only: c_char, c_ptr, c_long, c_int
    integer(c_int) :: c_fckit_configuration_get_long
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    integer(c_long) :: value
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int c_fckit_configuration_get_float (const Configuration* This, const char* na
!   me, float& value)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_float( This, name, value ) bind(C,name="c_fc&
  &kit_configuration_get_float")
    use iso_c_binding, only: c_char, c_ptr, c_int, c_float
    integer(c_int) :: c_fckit_configuration_get_float
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    real(c_float) :: value
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int c_fckit_configuration_get_double (const Configuration* This, const char* n
!   ame, double& value)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_double( This, name, value ) bind(C,name="c_f&
  &ckit_configuration_get_double")
    use iso_c_binding, only: c_char, c_ptr, c_int, c_double
    integer(c_int) :: c_fckit_configuration_get_double
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    real(c_double) :: value
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int c_fckit_configuration_get_string( const Configuration* This, const char* n
!   ame, char* &value, int &size)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_string( This, name, value, size ) bind(C,nam&
  &e="c_fckit_configuration_get_string")
    use iso_c_binding, only: c_char, c_ptr, c_int
    integer(c_int) :: c_fckit_configuration_get_string
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    type(c_ptr) :: value
    integer(c_int) :: size
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int c_fckit_configuration_get_array_int (const Configuration* This, const char
!   * name, int* &value, int& size)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_array_int( This, name, value, size ) bind(C,&
  &name="c_fckit_configuration_get_array_int")
    use iso_c_binding, only: c_char, c_ptr, c_int
    integer(c_int) :: c_fckit_configuration_get_array_int
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    type(c_ptr) :: value
    integer(c_int) :: size
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int c_fckit_configuration_get_array_long (const Configuration* This, const cha
!   r* name, long* &value, int& size)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_array_long( This, name, value, size ) bind(C&
  &,name="c_fckit_configuration_get_array_long")
    use iso_c_binding, only: c_char, c_ptr, c_int
    integer(c_int) :: c_fckit_configuration_get_array_long
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    type(c_ptr) :: value
    integer(c_int) :: size
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int c_fckit_configuration_get_array_float (const Configuration* This, const ch
!   ar* name, float* &value, int& size)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_array_float( This, name, value, size ) bind(&
  &C,name="c_fckit_configuration_get_array_float")
    use iso_c_binding, only: c_char, c_ptr, c_int
    integer(c_int) :: c_fckit_configuration_get_array_float
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    type(c_ptr) :: value
    integer(c_int) :: size
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int c_fckit_configuration_get_array_double (const Configuration* This, const c
!   har* name, double* &value, int& size)
!-------------------------------------------------------------------------------
function c_fckit_configuration_get_array_double( This, name, value, size ) bind&
  &(C,name="c_fckit_configuration_get_array_double")
    use iso_c_binding, only: c_char, c_ptr, c_int
    integer(c_int) :: c_fckit_configuration_get_array_double
    type(c_ptr), value :: This
    character(c_char), dimension(*) :: name
    type(c_ptr) :: value
    integer(c_int) :: size
end function
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! int c_fckit_configuration_has (const Configuration* This, const char *name)
!-------------------------------------------------------------------------------
function c_fckit_configuration_has( This, name ) bind(C,name="c_fckit_configura&
  &tion_has")
    use iso_c_binding, only: c_char, c_ptr, c_int
    integer(c_int) :: c_fckit_configuration_has
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
    use iso_c_binding, only: c_ptr, c_int
    type(c_ptr), value :: This
    type(c_ptr) :: json
    integer(c_int) :: size
end subroutine
!-------------------------------------------------------------------------------

end interface
#endif