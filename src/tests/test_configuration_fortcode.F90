! (C) Copyright 2013-2017 ECMWF.
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
    integer,external :: get_a
    type(c_ptr), value :: conf_cptr
    a = get_a( fckit_configuration(conf_cptr) )
end function

function get_a( conf ) result(a)
    use fckit_configuration_module
    implicit none
    integer :: a
    type(fckit_configuration) :: conf
    if( .not. conf%get("a",a) ) a = 0
end function
