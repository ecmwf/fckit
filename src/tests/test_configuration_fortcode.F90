! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

function c_get_a( conf_cptr ) result(a) bind(c)
    use, intrinsic :: iso_c_binding, only : c_ptr
    use fckit_configuration_module
    implicit none
    integer :: a
    type(c_ptr), value :: conf_cptr
    type(fckit_configuration) :: conf
    write(0,*) "c_get_a ..."
    conf = fckit_configuration( conf_cptr )
    if( .not. conf%get("a",a) ) a = 0
    write(0,*) "c_get_a ... done"
end function

!! Following function could be called instead in above function "c_get_a" as:
!!   a = get_a( fckit_configuration( conf_c_ptr ) )

function get_a( conf ) result(a)
    use fckit_configuration_module
    implicit none
    integer :: a
    type(fckit_configuration), intent(in) :: conf
    write(0,*) "get_a ..."
    if( .not. conf%get("a",a) ) a = 0
    write(0,*) "get_a ... done"
end function
