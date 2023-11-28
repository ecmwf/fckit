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

  !void fckit__set_signal_handler( int signum, fckit::signal_handler_t )
  subroutine fckit__set_signal_handler( signum, signal_handler ) bind(c)
    use iso_c_binding, only : c_int32_t, c_funptr
    integer(c_int32_t), value :: signum
    type(c_funptr), value :: signal_handler
  end subroutine

  !void fckit__set_fckit_signal_handler( int signum )
  subroutine fckit__set_fckit_signal_handler( signum ) bind(c)
    use iso_c_binding, only : c_int32_t
    integer(c_int32_t), value :: signum
  end subroutine

  !void fckit__set_fckit_signal_handlers()
  subroutine fckit__set_fckit_signal_handlers() bind(c)
  end subroutine

  !void fckit__raise_signal( int signum )
  subroutine fckit__raise_signal( signum ) bind(c)
    use iso_c_binding, only : c_int32_t
    integer(c_int32_t), value :: signum
  end subroutine

  !void fckit__restore_signal_handler( int signum )
  subroutine fckit__restore_signal_handler( signum ) bind(c)
    use iso_c_binding, only : c_int32_t
    integer(c_int32_t), value :: signum
  end subroutine

  !void fckit__restore_all_signal_handlers()
  subroutine fckit__restore_all_signal_handlers() bind(c)
  end subroutine

  !int fckit__SIGABRT ()
  function fckit__SIGABRT() result(signum) bind(c,name="fckit__SIGABRT")
    use iso_c_binding, only: c_int32_t
    integer(c_int32_t) :: signum
  end function

  !int fckit__SIGKILL ()
  function fckit__SIGKILL() result(signum) bind(c,name="fckit__SIGKILL")
    use iso_c_binding, only: c_int32_t
    integer(c_int32_t) :: signum
  end function

  !int fckit__SIGALRM ()
  function fckit__SIGALRM() result(signum) bind(c,name="fckit__SIGALRM")
    use iso_c_binding, only: c_int32_t
    integer(c_int32_t) :: signum
  end function

  !int fckit__SIGINT ()
  function fckit__SIGINT() result(signum) bind(c,name="fckit__SIGINT")
    use iso_c_binding, only: c_int32_t
    integer(c_int32_t) :: signum
  end function

  !int fckit__SIGFPE ()
  function fckit__SIGFPE() result(signum) bind(c,name="fckit__SIGFPE")
    use iso_c_binding, only: c_int32_t
    integer(c_int32_t) :: signum
  end function

  !int fckit__SIGILL ()
  function fckit__SIGILL() result(signum) bind(c,name="fckit__SIGILL")
    use iso_c_binding, only: c_int32_t
    integer(c_int32_t) :: signum
  end function

  !int fckit__SIGSEGV ()
  function fckit__SIGSEGV() result(signum) bind(c,name="fckit__SIGSEGV")
    use iso_c_binding, only: c_int32_t
    integer(c_int32_t) :: signum
  end function

  !int fckit__SIGTERM ()
  function fckit__SIGTERM() result(signum) bind(c,name="fckit__SIGTERM")
    use iso_c_binding, only: c_int32_t
    integer(c_int32_t) :: signum
  end function

  !int fckit__SIGTRAP ()
  function fckit__SIGTRAP() result(signum) bind(c,name="fckit__SIGTRAP")
    use iso_c_binding, only: c_int32_t
    integer(c_int32_t) :: signum
  end function
  
  !int fckit__SIGBUS ()
  function fckit__SIGBUS() result(signum) bind(c,name="fckit__SIGBUS")
    use iso_c_binding, only: c_int32_t
    integer(c_int32_t) :: signum
  end function
end interface

#endif

interface
  subroutine fckit_signal_handler( signum ) bind(C)
    use, intrinsic :: iso_c_binding, only : c_int32_t
    integer(c_int32_t), value, intent(in) :: signum
  end subroutine
end interface


