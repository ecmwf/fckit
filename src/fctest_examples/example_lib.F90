module example_module

implicit none

private

public :: example_function

contains

function example_function() result(j)
integer :: j
write(0,*) "example_function()"
j = 1
end function 

end module
