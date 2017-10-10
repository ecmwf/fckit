! (C) Copyright 2013-2017 ECMWF.
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

  !int fckit__main_init (int argc, char* argv[])
  function fckit__main_init(argc,argv) &
    & result(error_code) bind(c,name="fckit__main_init")
    use iso_c_binding, only: c_int, c_ptr
    integer(c_int) :: error_code
    integer(c_int), value :: argc
    type(c_ptr), dimension(*) :: argv
  end function

  ! void fckit__main_finalise()
  subroutine fckit__main_finalise() bind(c)
  end subroutine


  !int fckit__main_ready (int& ready)
  function fckit__main_ready(ready) result(error_code) bind(c,name="fckit__main_ready")
    use iso_c_binding, only: c_int
    integer(c_int) :: error_code
    integer(c_int) :: ready
  end function

  !int fckit__main_taskID (int& taskID)
  function fckit__main_taskID(taskID) result(error_code) bind(c,name="fckit__main_taskID")
    use iso_c_binding, only: c_int
    integer(c_int) :: error_code
    integer(c_int) :: taskID
  end function

  function fckit__main_setTaskID(taskID) result(error_code) bind(c,name="fckit__main_setTaskID")
    use iso_c_binding, only: c_int
    integer(c_int) :: error_code
    integer(c_int), value :: taskID
  end function

  function fckit__main_debug() result(debug) bind(c)
    use iso_c_binding, only : c_int
    integer(c_int) :: debug
  end function

  !int fckit__main_name (char* &name, int &name_size)
  function fckit__main_name(name,name_size) result(error_code) bind(c)
    use iso_c_binding, only: c_int, c_ptr
    integer(c_int) :: error_code
    type(c_ptr) :: name
    integer(c_int) :: name_size
  end function

  !int fckit__main_displayname (char* &name, int &name_size)
  function fckit__main_displayname(name,name_size) result(error_code) bind(c)
    use iso_c_binding, only: c_int, c_ptr
    integer(c_int) :: error_code
    type(c_ptr) :: name
    integer(c_int) :: name_size
  end function

end interface

#endif
