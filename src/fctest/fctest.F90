module fctest
  use, intrinsic :: iso_c_binding, only: c_float, c_double
  implicit none
  integer, parameter :: sp=c_float
  integer, parameter :: dp=c_double
public
  character(len=132) :: source_file
  integer :: exit_status
  interface FCE
    module procedure fctest_check_equal_int
    module procedure fctest_check_equal_real32
    module procedure fctest_check_equal_real64
    module procedure fctest_check_equal_string
    module procedure fctest_check_equal_int_r1
    module procedure fctest_check_equal_real32_r1
    module procedure fctest_check_equal_real64_r1
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

contains

function sweep_leading_blanks(in_str)
  character(len=*), intent(in)  :: in_str
  character(len=:), allocatable :: sweep_leading_blanks
  character :: ch
  integer :: j
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
  integer, intent(in) :: line
  write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
  exit_status=1
end subroutine

subroutine fctest_check_equal_int(V1,V2,line)
  integer, intent(in) :: V1, V2
  integer, intent(in) :: line
  if(V1/=V2) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    write(0,*) "--> [",V1,"!=",V2,"]"
    exit_status=1
  endif
end subroutine

subroutine fctest_check_equal_real32(V1,V2,line)
  real(kind=c_float), intent(in) :: V1, V2
  integer, intent(in) :: line
  if(V1/=V2) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    write(0,*) "--> [",V1,"!=",V2,"]"
    exit_status=1
  endif
end subroutine

subroutine fctest_check_equal_real64(V1,V2,line)
  real(kind=c_double), intent(in) :: V1, V2
  integer, intent(in) :: line
  if(V1/=V2) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    write(0,*) "--> [",V1,"!=",V2,"]"
    exit_status=1
  endif
end subroutine

subroutine fctest_check_equal_string(V1,V2,line)
  character(len=*), intent(in) :: V1, V2
  integer, intent(in) :: line
  if(V1/=V2) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    write(0,*) "--> [",V1,"!=",V2,"]"
    exit_status=1
  endif
end subroutine

subroutine fctest_check_equal_int_r1(V1,V2,line)
  integer, intent(in) :: V1(:), V2(:)
  integer, intent(in) :: line
  logical :: compare = .True.
  integer :: j
  if( size(V1) /= size(V2) ) compare = .False.
  if( compare .eqv. .True. ) then
    do j=1,size(V1)
      if( V1(j)/=V2(j) ) compare = .False.
    enddo
  endif
  if( compare .eqv. .False. ) then
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    if( size(V1) <= 30 ) then
      write(0,*) "--> [ (\",V1,"\) != \(",V2,"\) ]"
    endif
    exit_status=1
  endif
end subroutine

subroutine fctest_check_equal_real32_r1(V1,V2,line)
  real(kind=c_float), intent(in) :: V1(:), V2(:)
  integer, intent(in) :: line
  logical :: compare
  integer :: j
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
      write(0,*) "--> [ (\",V1,"\) != \(",V2,"\) ]"
    endif
    exit_status=1
  endif
end subroutine

subroutine fctest_check_equal_real64_r1(V1,V2,line)
  real(kind=c_double), intent(in) :: V1(:), V2(:)
  integer, intent(in) :: line
  logical :: compare
  integer :: j
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
      write(0,*) "--> [ (\",V1,"\) != \(",V2,"\) ]"
    endif
    exit_status=1
  endif
end subroutine

subroutine fctest_check_close_real32(V1,V2,TOL,line)
  real(kind=c_float), intent(in) :: V1, V2, TOL
  integer, intent(in) :: line
  if(.not.(abs(V1-V2)<=TOL)) then;
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    write(0,*) "--> [",V1,"!=",V2,"]"
    exit_status=1
  endif
end subroutine

subroutine fctest_check_close_real64(V1,V2,TOL,line)
  real(kind=c_double), intent(in) :: V1, V2, TOL
  integer, intent(in) :: line
  if(.not.(abs(V1-V2)<=TOL)) then;
    write(0,'(2A,I0,2A)') trim(source_file),":",line,": warning: ",trim(sweep_leading_blanks(get_source_line(line)))
    write(0,*) "--> [",V1,"!=",V2,"]"
    exit_status=1
  endif
end subroutine

subroutine fctest_check_close_real32_r1(V1,V2,TOL,line)
  real(kind=c_float), intent(in) :: V1(:), V2(:)
  real(kind=c_float), intent(in) :: TOL
  integer, intent(in) :: line
  logical :: compare
  integer :: j
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
      write(0,*) "--> [ (\",V1,"\) != \(",V2,"\) ]"
    endif
    exit_status=1
  endif
end subroutine

subroutine fctest_check_close_real64_r1(V1,V2,TOL,line)
  real(kind=c_double), intent(in) :: V1(:), V2(:)
  real(kind=c_double), intent(in) :: TOL
  integer, intent(in) :: line
  logical :: compare
  integer :: j
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
      write(0,*) "--> [ (\",V1,"\) != \(",V2,"\) ]"
    endif
    exit_status=1
  endif
end subroutine

function get_source_line(line_number) result(source_line)
  integer, intent(in)  :: line_number
  ! Variables
  integer stat, jline
  character(len=512) :: source_line

  ! open input file
  open (10, file=source_file, status='old', iostat=stat)
  if (stat .ne. 0)then
    source_line = 'source_file '//source_file//' can not be opened'
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
