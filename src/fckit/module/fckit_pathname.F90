! (C) Copyright 2013-2017 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module fckit_pathname_module
  !! Module holding the [[fckit_pathname_module:fckit_pathname(type)]] type
  !! for strong typing of file paths

implicit none

public :: fckit_pathname

private


TYPE :: fckit_pathname
  !! Type encapsulated string, used for strong typing file paths.
  character(len=1), allocatable, private :: string(:)
contains
  procedure :: str => fckit_pathname__str
    !! Function that returns the file path as string
END TYPE fckit_pathname

interface fckit_pathname
  !! Constructor for [[fckit_pathname_module:fckit_pathname(type)]]
  module procedure fckit_pathname__ctor_str
end interface

!========================================================
contains
!========================================================


function fckit_pathname__ctor_str(str) result(pathname)
  type(fckit_pathname) :: pathname
  character(len=*), intent(in) :: str
  integer i, nchars
  nchars = len(str)
  allocate( pathname%string(nchars) )
  do i=1,nchars
    pathname%string(i) = str(i:i)
  enddo
end function

function fckit_pathname__str(this) result(str)
  character(len=:), allocatable :: str
  class(fckit_pathname) :: this
  integer i, nchars
  nchars = size(this%string)
  allocate(character(len=nchars) :: str)
  do i=1,nchars
    str(i:i) = this%string(i)
  enddo
end function

end module fckit_pathname_module

