#ifndef FORD


interface



! ======== general


!-------------------------------------------------------------------------------
! FckitMap* c_fckit_map_new ()
!-------------------------------------------------------------------------------
function c_fckit_map_new() bind(C,name="c_fckit_map_new")
    use iso_c_binding, only: c_ptr
    type(c_ptr) :: c_fckit_map_new
end function
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_map_delete (FckitMap* This)
!-------------------------------------------------------------------------------
subroutine c_fckit_map_delete( this ) bind(C,name="c_fckit_map_delete")
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: this
end subroutine
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_map_has (FckitMap* this, const char* name)
!-------------------------------------------------------------------------------
function c_fckit_map_has( this, name ) bind(C,name="c_fckit_map_has")
  use iso_c_binding, only : c_ptr, c_char, c_bool  
  type(c_ptr), intent(in), value :: this
  character(c_char)            :: name
  logical(c_bool)       :: c_fckit_map_has
end function
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_map_size (FckitMap* this)
!-------------------------------------------------------------------------------
function c_fckit_map_size( this ) bind(C,name="c_fckit_map_size")
  use iso_c_binding, only : c_ptr, c_int  
  type(c_ptr), intent(in), value :: this
  integer(c_int) :: c_fckit_map_size
end function
!-------------------------------------------------------------------------------



! ======== setters


!-------------------------------------------------------------------------------
! void c_fckit_map_insert_int32(FckitMap* this, const char* name, int value)
!-------------------------------------------------------------------------------
subroutine c_fckit_map_insert_int32( this, name, value) &
  & bind(C,name="c_fckit_map_insert_int32")
  use iso_c_binding, only : c_ptr, c_int, c_char
  type(c_ptr), intent(in), value    :: this  
  character(c_char)                 :: name
  integer(c_int), intent(in), value :: value 
end subroutine
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_map_insert_int64(FckitMap* this, const char* name, int64 value)
!-------------------------------------------------------------------------------
subroutine c_fckit_map_insert_int64( this, name, value) &
  & bind(C,name="c_fckit_map_insert_int64")
  use iso_c_binding, only : c_ptr, c_int64_t, c_char
  type(c_ptr), intent(in), value    :: this  
  character(c_char)                 :: name
  integer(c_int64_t), intent(in), value :: value 
end subroutine
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_map_insert_real32(FckitMap* this, const char* name, float value)
!-------------------------------------------------------------------------------
subroutine c_fckit_map_insert_real32( this, name, value) &
  & bind(C,name="c_fckit_map_insert_real32")
  use iso_c_binding, only : c_ptr, c_float, c_char
  type(c_ptr), intent(in), value   :: this  
  character(c_char)                :: name
  real(c_float), intent(in), value :: value 
end subroutine
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_map_insert_real64(FckitMap* this, const char* name, double value)
!-------------------------------------------------------------------------------
subroutine c_fckit_map_insert_real64( this, name, value) &
  & bind(C,name="c_fckit_map_insert_real64")
  use iso_c_binding, only : c_ptr, c_double, c_char
  type(c_ptr), intent(in), value    :: this
  character(c_char)                 :: name
  real(c_double), intent(in), value :: value  
end subroutine
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_map_insert_c_ptr(FckitMap* this, const char* name, void* value)
!-------------------------------------------------------------------------------
subroutine c_fckit_map_insert_c_ptr( this, name, value) &
  & bind(C,name="c_fckit_map_insert_c_ptr")
  use iso_c_binding, only : c_ptr, c_char
  type(c_ptr), intent(in), value :: this
  character(c_char)              :: name
  type(c_ptr), intent(in), value :: value  
end subroutine
!-------------------------------------------------------------------------------


! ======== getters


!-------------------------------------------------------------------------------
! void c_fckit_map_get_int32(FckitMap* this, const char* name, int32 value)
!-------------------------------------------------------------------------------
subroutine c_fckit_map_get_int32( this, name, value) &
  & bind(C,name="c_fckit_map_get_int32")
  use iso_c_binding, only : c_ptr, c_char, c_int
  type(c_ptr), intent(in), value :: this
  character(c_char)              :: name 
  integer(c_int), intent(inout)  :: value  
end subroutine
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_map_get_int64(FckitMap* this, const char* name, int64 value)
!-------------------------------------------------------------------------------
subroutine c_fckit_map_get_int64( this, name, value) &
  & bind(C,name="c_fckit_map_get_int64")
  use iso_c_binding, only : c_ptr, c_char, c_int64_t
  type(c_ptr), intent(in), value :: this
  character(c_char)              :: name 
  integer(c_int64_t), intent(inout)  :: value
end subroutine
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_map_get_real32(FckitMap* this, const char* name, float value)
!-------------------------------------------------------------------------------
subroutine c_fckit_map_get_real32( this, name, value) &
  & bind(C,name="c_fckit_map_get_real32")
  use iso_c_binding, only : c_ptr, c_char, c_float
  type(c_ptr), intent(in), value :: this
  character(c_char)              :: name 
  real(c_float), intent(inout)  :: value  
end subroutine
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_map_get_real64(FckitMap* this, const char* name, double value)
!-------------------------------------------------------------------------------
subroutine c_fckit_map_get_real64( this, name, value) &
  & bind(C,name="c_fckit_map_get_real64")
  use iso_c_binding, only : c_ptr, c_char, c_double
  type(c_ptr), intent(in), value :: this
  character(c_char)              :: name 
  real(c_double), intent(inout)  :: value  
end subroutine
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! void c_fckit_map_get_c_ptr(FckitMap* this, const char* name, void* value)
!-------------------------------------------------------------------------------
subroutine c_fckit_map_get_c_ptr( this, name, value) &
  & bind(C,name="c_fckit_map_get_c_ptr")
  use iso_c_binding, only : c_ptr, c_char, c_double
  type(c_ptr), intent(in), value :: this
  character(c_char)              :: name 
  type(c_ptr), intent(inout)  :: value  
end subroutine
!-------------------------------------------------------------------------------


end interface



#endif