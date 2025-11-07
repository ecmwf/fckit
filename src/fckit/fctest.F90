! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module fctest
  use, intrinsic :: iso_c_binding, only: c_float, c_double, c_int32_t, c_int64_t, c_char, c_int
  implicit none
  integer(c_int32_t), parameter :: sp=c_float
  integer(c_int32_t), parameter :: dp=c_double
public
  character(len=1024) :: source_file
  integer(c_int32_t) :: exit_status
  interface FCE
    module procedure fctest_check_equal_int32
    module procedure fctest_check_equal_int64_int32
    module procedure fctest_check_equal_int32_int64
    module procedure fctest_check_equal_int64
    module procedure fctest_check_equal_real32
    module procedure fctest_check_equal_real64
    module procedure fctest_check_equal_string
    module procedure fctest_check_equal_int32_r1
    module procedure fctest_check_equal_int64_r1
    module procedure fctest_check_equal_real32_r1
    module procedure fctest_check_equal_real64_r1
    module procedure fctest_check_equal_logical
  end interface FCE
  interface FCC
    module procedure fctest_check_close_real32
    module procedure fctest_check_close_real64
    module procedure fctest_check_close_real32_r1
    module procedure fctest_check_close_real64_r1
  end interface FCC
  interface ERR
    module procedure fctest_error
  end interface ERR

! TODO: These should be private
! private :: c_float, c_double, c_int32_t, c_int64_t, c_char

contains

function sweep_leading_blanks(in_str)
  character(kind=c_char,len=*), intent(in)  :: in_str
  character(kind=c_char,len=512) :: sweep_leading_blanks
  character(kind=c_char) :: ch
  integer(c_int32_t) :: j
  do j=1, len_trim(in_str)
    ! get j-th char
    ch = in_str(j:j)
    if (ch .ne. " ") then
      sweep_leading_blanks = trim(in_str(j:len_trim(in_str)))
      return
    endif
  end do
end function sweep_leading_blanks

subroutine fctest_error(line)
  integer(c_int32_t), intent(in) :: line
  write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
  exit_status=1
end subroutine

subroutine fctest_check_equal_int32(V1,V2,line)
  integer(c_int32_t), intent(in) :: V1, V2
  integer(c_int32_t), intent(in) :: line
  if(V1/=V2) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    write(0,*) "--> [",V1,"!=",V2,"]"
    exit_status=1
  endif
end subroutine

subroutine fctest_check_equal_int64(V1,V2,line)
  integer(c_int64_t), intent(in) :: V1, V2
  integer(c_int32_t), intent(in) :: line
  if(V1/=V2) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    write(0,*) "--> [",V1,"!=",V2,"]"
    exit_status=1
  endif
end subroutine

subroutine fctest_check_equal_int64_int32(V1,V2,line)
  integer(c_int64_t), intent(in) :: V1
  integer(c_int32_t), intent(in) :: V2
  integer(c_int32_t), intent(in) :: line
  if(V1/=V2) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    write(0,*) "--> [",V1,"!=",V2,"]"
    exit_status=1
  endif
end subroutine

subroutine fctest_check_equal_int32_int64(V1,V2,line)
  integer(c_int32_t), intent(in) :: V1
  integer(c_int64_t), intent(in) :: V2
  integer(c_int32_t), intent(in) :: line
  if(V1/=V2) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    write(0,*) "--> [",V1,"!=",V2,"]"
    exit_status=1
  endif
end subroutine

subroutine fctest_check_equal_real32(V1,V2,line)
  real(kind=c_float), intent(in) :: V1, V2
  integer(c_int32_t), intent(in) :: line
  if(V1/=V2) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    write(0,*) "--> [",V1,"!=",V2,"]"
    exit_status=1
  endif
end subroutine

subroutine fctest_check_equal_real64(V1,V2,line)
  real(kind=c_double), intent(in) :: V1, V2
  integer(c_int32_t), intent(in) :: line
  if(V1/=V2) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    write(0,*) "--> [",V1,"!=",V2,"]"
    exit_status=1
  endif
end subroutine

subroutine fctest_check_equal_logical(V1,V2,line)
  logical, intent(in) :: V1, V2
  integer(c_int32_t), intent(in) :: line
  if(V1.neqv.V2) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    write(0,*) "--> [",V1,"!=",V2,"]"
    exit_status=1
  endif
end subroutine

subroutine fctest_check_equal_string(V1,V2,line)
  character(kind=c_char,len=*), intent(in) :: V1, V2
  integer(c_int32_t), intent(in) :: line
  if(V1/=V2) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    write(0,*) "--> [",V1,"!=",V2,"]"
    exit_status=1
  endif
end subroutine

subroutine fctest_check_equal_int32_r1(V1,V2,line)
  integer(c_int32_t), intent(in) :: V1(:), V2(:)
  integer(c_int32_t), intent(in) :: line
  logical :: compare
  integer(c_int32_t) :: j
  compare = .True.
  if( size(V1) /= size(V2) ) compare = .False.
  if( compare .eqv. .True. ) then
    do j=1,size(V1)
      if( V1(j)/=V2(j) ) compare = .False.
    enddo
  endif
  if( compare .eqv. .False. ) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    if( size(V1) <= 30 ) then
      write(0,*) "-->  [ ",V1," ] != [ ",V2," ] "
    endif
    exit_status=1
  endif
end subroutine

subroutine fctest_check_equal_int64_r1(V1,V2,line)
  integer(c_int64_t), intent(in) :: V1(:), V2(:)
  integer(c_int32_t), intent(in) :: line
  logical :: compare
  integer(c_int32_t) :: j
  compare = .True.
  if( size(V1) /= size(V2) ) compare = .False.
  if( compare .eqv. .True. ) then
    do j=1,size(V1)
      if( V1(j)/=V2(j) ) compare = .False.
    enddo
  endif
  if( compare .eqv. .False. ) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    if( size(V1) <= 30 ) then
      write(0,*) "--> [ ",V1," ] != [ ",V2," ]"
    endif
    exit_status=1
  endif
end subroutine

subroutine fctest_check_equal_real32_r1(V1,V2,line)
  real(kind=c_float), intent(in) :: V1(:), V2(:)
  integer(c_int32_t), intent(in) :: line
  logical :: compare
  integer(c_int32_t) :: j
  compare = .True.
  if( size(V1) /= size(V2) ) compare = .False.
  if( compare .eqv. .True. ) then
    do j=1,size(V1)
      if( V1(j)/=V2(j) ) compare = .False.
    enddo
  endif
  if( compare .eqv. .False. ) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    if( size(V1) <= 30 ) then
      write(0,*) "--> [ ",V1," ] != [ ",V2," ]"
    endif
    exit_status=1
  endif
end subroutine

subroutine fctest_check_equal_real64_r1(V1,V2,line)
  real(kind=c_double), intent(in) :: V1(:), V2(:)
  integer(c_int32_t), intent(in) :: line
  logical :: compare
  integer(c_int32_t) :: j
  compare = .True.
  if( size(V1) /= size(V2) ) compare = .False.
  if( compare .eqv. .True. ) then
    do j=1,size(V1)
      if( V1(j)/=V2(j) ) compare = .False.
    enddo
  endif
  if( compare .eqv. .False. ) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    if( size(V1) <= 30 ) then
      write(0,*) "--> [ ",V1," ] != [ ",V2," ]"
    endif
    exit_status=1
  endif
end subroutine

subroutine fctest_check_close_real32(V1,V2,TOL,line)
  real(kind=c_float), intent(in) :: V1, V2, TOL
  integer(c_int32_t), intent(in) :: line
  if(.not.(abs(V1-V2)<=TOL)) then;
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    write(0,*) "--> [",V1,"!=",V2,"]"
    exit_status=1
  endif
end subroutine

subroutine fctest_check_close_real64(V1,V2,TOL,line)
  real(kind=c_double), intent(in) :: V1, V2, TOL
  integer(c_int32_t), intent(in) :: line
  if(.not.(abs(V1-V2)<=TOL)) then;
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    write(0,*) "--> [",V1,"!=",V2,"]"
    exit_status=1
  endif
end subroutine

subroutine fctest_check_close_real32_r1(V1,V2,TOL,line)
  real(kind=c_float), intent(in) :: V1(:), V2(:)
  real(kind=c_float), intent(in) :: TOL
  integer(c_int32_t), intent(in) :: line
  logical :: compare
  integer(c_int32_t) :: j
  compare = .True.
  if( size(V1) /= size(V2) ) compare = .False.
  if( compare .eqv. .True. ) then
    do j=1,size(V1)
      if(.not.(abs(V1(j)-V2(j))<=TOL)) compare = .False.
    enddo
  endif
  if( compare .eqv. .False. ) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    if( size(V1) <= 30 ) then
      write(0,*) "--> [ ",V1," ] != [ ",V2," ]"
    endif
    exit_status=1
  endif
end subroutine

subroutine fctest_check_close_real64_r1(V1,V2,TOL,line)
  real(kind=c_double), intent(in) :: V1(:), V2(:)
  real(kind=c_double), intent(in) :: TOL
  integer(c_int32_t), intent(in) :: line
  logical :: compare
  integer(c_int32_t) :: j
  compare = .True.
  if( size(V1) /= size(V2) ) compare = .False.
  if( compare .eqv. .True. ) then
    do j=1,size(V1)
      if(.not.(abs(V1(j)-V2(j))<=TOL)) compare = .False.
    enddo
  endif
  if( compare .eqv. .False. ) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    if( size(V1) <= 30 ) then
      write(0,*) "--> [ ",V1," ] != [ ",V2," ]"
    endif
    exit_status=1
  endif
end subroutine

function get_source_line(line_number) result(source_line)
  integer(c_int32_t), intent(in)  :: line_number
  ! Variables
  integer(c_int32_t) stat, jline
  character(kind=c_char,len=512) :: source_line

  ! open input file
  open (10, file=source_file, status='old', iostat=stat)
  if (stat .ne. 0)then
    source_line = 'source_file '//trim(source_file)//' can not be opened'
    close(10)
    return
  end if

  ! process file
  do jline=1,line_number
    read (10, '(A)', end=99) source_line ! read line from input file
  enddo
  close(10)

  ! close files
  99 continue
  close (10)
end function get_source_line

end module fctest
