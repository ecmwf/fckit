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

  !void fckit__set_abort_handler( eckit::abort_handler_t h )
  subroutine fckit__set_abort_handler( h ) bind(c)
    use iso_c_binding, only : c_funptr
    type(c_funptr), value :: h
  end subroutine

  !void fckit__abort (const char* what, const char* file, int32 line, const char* function )
  subroutine fckit__abort(what,file,line,function) bind(c)
    use iso_c_binding, only : c_char, c_int32_t
    character(kind=c_char), dimension(*) :: what
    character(kind=c_char), dimension(*) :: file
    integer(c_int32_t), value :: line
    character(kind=c_char), dimension(*) :: function
  end subroutine

  !void fckit__exception_throw (const char* what, const char* file, int32 line, const char* function )
  subroutine fckit__exception_throw(what,file,line,function) bind(c)
    use iso_c_binding, only : c_char, c_int32_t
    character(kind=c_char), dimension(*) :: what
    character(kind=c_char), dimension(*) :: file
    integer(c_int32_t), value :: line
    character(kind=c_char), dimension(*) :: function
  end subroutine

  !int fckit__exception_what (char* &what, size_t &what_size)
  function fckit__exception_what(what,what_size) result(error_code) bind(c)
    use iso_c_binding, only: c_int32_t, c_size_t, c_ptr
    integer(c_int32_t) :: error_code
    type(c_ptr) :: what
    integer(c_size_t) :: what_size
  end function

  !int fckit__exception_location ()
  function fckit__exception_location() result(location) bind(c)
    use iso_c_binding, only: c_int32_t
    integer(c_int32_t) :: location
  end function

  !int fckit__exception_file (char* &file, size_t &file_size)
  function fckit__exception_file(file,file_size) result(error_code) bind(c)
    use iso_c_binding, only: c_int32_t, c_size_t, c_ptr
    integer(c_int32_t) :: error_code
    type(c_ptr) :: file
    integer(c_size_t) :: file_size
  end function

  !int fckit__exception_line ()
  function fckit__exception_line() result(line) bind(c)
    use iso_c_binding, only: c_int32_t
    integer(c_int32_t) :: line
  end function

  !int fckit__exception_function (char* &function, size_t &function_size)
  function fckit__exception_function(function,function_size) result(error_code) bind(c)
    use iso_c_binding, only: c_int32_t, c_size_t, c_ptr
    integer(c_int32_t) :: error_code
    type(c_ptr) :: function
    integer(c_size_t) :: function_size
  end function

  !int fckit__exception_callstack (char* &callstack, size_t &callstack_size)
  function fckit__exception_callstack(callstack,callstack_size) result(error_code) bind(c)
    use iso_c_binding, only: c_int32_t, c_size_t, c_ptr
    integer(c_int32_t) :: error_code
    type(c_ptr) :: callstack
    integer(c_size_t) :: callstack_size
  end function

end interface

#endif

interface
  subroutine fckit_exception_handler()
  end subroutine
end interface
