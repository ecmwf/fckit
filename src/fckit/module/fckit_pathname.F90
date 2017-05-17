
module fckit_pathname_module

implicit none

public :: fckit_PathName

private


TYPE :: fckit_PathName
  character(len=1), allocatable, private :: string(:)
contains
  procedure :: str => fckit_PathName__str
END TYPE fckit_PathName

interface fckit_PathName
  module procedure fckit_PathName__ctor_str
end interface

!========================================================
contains
!========================================================


function fckit_PathName__ctor_str(str) result(PathName)
  type(fckit_PathName) :: PathName
  character(len=*), intent(in) :: str
  integer i, nchars
  nchars = len(str)
  allocate( PathName%string(nchars) )
  do i=1,nchars
    PathName%string(i) = str(i:i)
  enddo
end function

function fckit_PathName__str(this) result(str)
  character(len=:), allocatable :: str
  class(fckit_PathName) :: this
  integer i, nchars
  nchars = size(this%string)
  allocate(character(len=nchars) :: str)
  do i=1,nchars
    str(i:i) = this%string(i)
  enddo
end function

end module fckit_pathname_module

