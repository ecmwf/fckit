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

  !int32 fckit__main_init (int32 argc, char* argv[])
  function fckit__main_init(argc,argv) &
    & result(error_code) bind(c,name="fckit__main_init")
    use iso_c_binding, only: c_int32_t, c_ptr
    integer(c_int32_t) :: error_code
    integer(c_int32_t), value :: argc
    type(c_ptr), dimension(*) :: argv
  end function

  ! void fckit__main_finalise()
  subroutine fckit__main_finalise() bind(c)
  end subroutine


  !int32 fckit__main_ready (int32& ready)
  function fckit__main_ready(ready) result(error_code) bind(c,name="fckit__main_ready")
    use iso_c_binding, only: c_int32_t
    integer(c_int32_t) :: error_code
    integer(c_int32_t) :: ready
  end function

  !int32 fckit__main_taskID (int32& taskID)
  function fckit__main_taskID(taskID) result(error_code) bind(c,name="fckit__main_taskID")
    use iso_c_binding, only: c_int32_t
    integer(c_int32_t) :: error_code
    integer(c_int32_t) :: taskID
  end function

  function fckit__main_setTaskID(taskID) result(error_code) bind(c,name="fckit__main_setTaskID")
    use iso_c_binding, only: c_int32_t
    integer(c_int32_t) :: error_code
    integer(c_int32_t), value :: taskID
  end function

  function fckit__main_debug() result(debug) bind(c)
    use iso_c_binding, only : c_int32_t
    integer(c_int32_t) :: debug
  end function

  function fckit__main_verbose() result(verbose) bind(c)
    use iso_c_binding, only : c_int32_t
    integer(c_int32_t) :: verbose
  end function

  !int32 fckit__main_name (char* &name, size_t &name_size)
  function fckit__main_name(name,name_size) result(error_code) bind(c)
    use iso_c_binding, only: c_int32_t, c_size_t, c_ptr
    integer(c_int32_t) :: error_code
    type(c_ptr) :: name
    integer(c_size_t) :: name_size
  end function

  !int32 fckit__main_displayname (char* &name, size_t &name_size)
  function fckit__main_displayname(name,name_size) result(error_code) bind(c)
    use iso_c_binding, only: c_int32_t, c_size_t, c_ptr
    integer(c_int32_t) :: error_code
    type(c_ptr) :: name
    integer(c_size_t) :: name_size
  end function

end interface

#endif
