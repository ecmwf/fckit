
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
