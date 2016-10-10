! Callback function, used from C++ side
subroutine fckit_write_to_fortran_unit(unit,msg_cptr) bind(C)
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr
  use fckit_c_interop_module, only : c_ptr_to_string
  integer(c_int), value, intent(in) :: unit
  type(c_ptr), value, intent(in) :: msg_cptr
  character(len=:), allocatable :: msg
  msg = c_ptr_to_string(msg_cptr)
  write(unit,'(A)') msg
end subroutine

function fckit_fortranunit_stdout() result(stdout) bind(C)
  use, intrinsic :: iso_c_binding, only : c_int
  use, intrinsic :: iso_fortran_env, only : output_unit
  integer(c_int) :: stdout
  stdout = output_unit
end function

function fckit_fortranunit_stderr() result(stderr) bind(C)
  use, intrinsic :: iso_c_binding, only : c_int
  use, intrinsic :: iso_fortran_env, only : error_unit
  integer(c_int) :: stderr
  stderr = error_unit
end function
