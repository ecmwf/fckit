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

  !void fckit__abort (const char* what, const char* file, int line, const char* function )
  subroutine fckit__abort(what,file,line,function) bind(c)
    use iso_c_binding, only : c_char, c_int
    character(kind=c_char), dimension(*) :: what
    character(kind=c_char), dimension(*) :: file
    integer(c_int), value :: line
    character(kind=c_char), dimension(*) :: function
  end subroutine

  !void fckit__exception_throw (const char* what, const char* file, int line, const char* function )
  subroutine fckit__exception_throw(what,file,line,function) bind(c)
    use iso_c_binding, only : c_char, c_int
    character(kind=c_char), dimension(*) :: what
    character(kind=c_char), dimension(*) :: file
    integer(c_int), value :: line
    character(kind=c_char), dimension(*) :: function
  end subroutine
  
  !int fckit__exception_what (char* &what, int &what_size)
  function fckit__exception_what(what,what_size) result(error_code) bind(c)
    use iso_c_binding, only: c_int, c_ptr
    integer(c_int) :: error_code
    type(c_ptr) :: what
    integer(c_int) :: what_size
  end function
  
  !int fckit__exception_location ()
  function fckit__exception_location() result(location) bind(c)
    use iso_c_binding, only: c_int
    integer(c_int) :: location
  end function
  
  !int fckit__exception_file (char* &file, int &file_size)
  function fckit__exception_file(file,file_size) result(error_code) bind(c)
    use iso_c_binding, only: c_int, c_ptr
    integer(c_int) :: error_code
    type(c_ptr) :: file
    integer(c_int) :: file_size
  end function
  
  !int fckit__exception_line ()
  function fckit__exception_line() result(line) bind(c)
    use iso_c_binding, only: c_int
    integer(c_int) :: line
  end function

  !int fckit__exception_function (char* &function, int &function_size)
  function fckit__exception_function(function,function_size) result(error_code) bind(c)
    use iso_c_binding, only: c_int, c_ptr
    integer(c_int) :: error_code
    type(c_ptr) :: function
    integer(c_int) :: function_size
  end function

  !int fckit__exception_callstack (char* &callstack, int &callstack_size)
  function fckit__exception_callstack(callstack,callstack_size) result(error_code) bind(c)
    use iso_c_binding, only: c_int, c_ptr
    integer(c_int) :: error_code
    type(c_ptr) :: callstack
    integer(c_int) :: callstack_size
  end function

end interface

#endif

interface
  subroutine fckit_exception_handler()
  end subroutine
end interface
