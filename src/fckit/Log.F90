! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

! Callback function, used from C++ side
subroutine fckit_write_to_fortran_unit(unit,msg_cptr) bind(C)
  use, intrinsic :: iso_c_binding, only: c_int32_t, c_ptr, c_char
  use fckit_c_interop_module, only : c_ptr_to_string
  integer(c_int32_t), value, intent(in) :: unit
  type(c_ptr), value, intent(in) :: msg_cptr
  character(kind=c_char,len=:), allocatable :: msg
  msg = c_ptr_to_string(msg_cptr)
  write(unit,'(A)') msg
end subroutine

function fckit_fortranunit_stdout() result(stdout) bind(C)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  use, intrinsic :: iso_fortran_env, only : output_unit
  integer(c_int32_t) :: stdout
  stdout = output_unit
end function

function fckit_fortranunit_stderr() result(stderr) bind(C)
  use, intrinsic :: iso_c_binding, only : c_int32_t
  use, intrinsic :: iso_fortran_env, only : error_unit
  integer(c_int32_t) :: stderr
  stderr = error_unit
end function
