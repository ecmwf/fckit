! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

#ifdef FORD
#define FORD_PRIVATE public
#else
#define FORD_PRIVATE private
interface
  ! void fckit__log_debug(char *msg, int newl, int flush)
  subroutine fckit__log_debug(msg,newl,flush) bind(c)
    use, intrinsic :: iso_c_binding , only : c_char, c_int32_t
    character(kind=c_char), dimension(*) :: msg
    integer(c_int32_t), value :: newl
    integer(c_int32_t), value :: flush
  end subroutine
  ! void fckit__log_info(char *msg, int newl, int flush)
  subroutine fckit__log_info(msg,newl,flush) bind(c)
    use, intrinsic :: iso_c_binding , only : c_char, c_int32_t
    character(kind=c_char), dimension(*) :: msg
    integer(c_int32_t), value :: newl
    integer(c_int32_t), value :: flush
  end subroutine
  ! void fckit__log_verbose(char *msg, int newl, int flush)
  subroutine fckit__log_verbose(msg,newl,flush) bind(c)
    use, intrinsic :: iso_c_binding , only : c_char, c_int32_t
    character(kind=c_char), dimension(*) :: msg
    integer(c_int32_t), value :: newl
    integer(c_int32_t), value :: flush
  end subroutine
  ! void fckit__log_warning(char *msg, int newl, int flush)
  subroutine fckit__log_warning(msg,newl,flush) bind(c)
    use, intrinsic :: iso_c_binding , only : c_char, c_int32_t
    character(kind=c_char), dimension(*) :: msg
    integer(c_int32_t), value :: newl
    integer(c_int32_t), value :: flush
  end subroutine
  ! void fckit__log_error(char *msg, int newl, int flush)
  subroutine fckit__log_error(msg,newl,flush) bind(c)
    use, intrinsic :: iso_c_binding , only : c_char, c_int32_t
    character(kind=c_char), dimension(*) :: msg
    integer(c_int32_t), value :: newl
    integer(c_int32_t), value :: flush
  end subroutine
  subroutine fckit__log_add_fortran_unit(unit,style) bind(c)
    use, intrinsic :: iso_c_binding, only : c_int32_t
    integer(c_int32_t), value :: unit
    integer(c_int32_t), value :: style
  end subroutine
  subroutine fckit__log_set_fortran_unit(unit,style) bind(c)
    use, intrinsic :: iso_c_binding, only : c_int32_t
    integer(c_int32_t), value :: unit
    integer(c_int32_t), value :: style
  end subroutine
  subroutine fckit__log_add_file(path,style) bind(c)
    use, intrinsic :: iso_c_binding , only : c_char, c_int32_t
    character(kind=c_char), dimension(*) :: path
    integer(c_int32_t), value :: style
  end subroutine
  subroutine fckit__log_set_file(path,style) bind(c)
    use, intrinsic :: iso_c_binding , only : c_char, c_int32_t
    character(kind=c_char), dimension(*) :: path
    integer(c_int32_t), value :: style
  end subroutine
  subroutine fckit__log_add_stdout(style) bind(c)
    use, intrinsic :: iso_c_binding , only : c_int32_t
    integer(c_int32_t), value :: style
  end subroutine
  subroutine fckit__log_set_stdout(style) bind(c)
    use, intrinsic :: iso_c_binding , only : c_int32_t
    integer(c_int32_t), value :: style
  end subroutine
  subroutine fckit__log_reset() bind(c)
  end subroutine
  subroutine fckit__log_flush() bind(c)
  end subroutine
  function fckit__log_info_channel() bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr
    type(c_ptr) :: fckit__log_info_channel
  end function
  function fckit__log_verbose_channel() bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr
    type(c_ptr) :: fckit__log_verbose_channel
  end function
  function fckit__log_warning_channel() bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr
    type(c_ptr) :: fckit__log_warning_channel
  end function
  function fckit__log_error_channel() bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr
    type(c_ptr) :: fckit__log_error_channel
  end function
  function fckit__log_debug_channel() bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr
    type(c_ptr) :: fckit__log_debug_channel
  end function

end interface
#endif
