#:mute
#:set ranks  = [1,2,3,4,5]
#:set dim    = ['',':',':,:',':,:,:',':,:,:,:',':,:,:,:,:']
#:set ftypes = ['integer(c_int)','integer(c_long)','real(c_float)','real(c_double)', 'logical']
#:set dtypes = ['int32', 'int64', 'real32', 'real64', 'logical32']
#:set types  = list(zip(dtypes,ftypes))

#:def print_code_location()
write(0,*) "This is a very long line in which we will print the code location of this line: ", "${_FILE_}$", ${_LINE_}$
#:enddef

#:def ensure(cond, msg=None)
    if (.not. (${cond}$)) then
      write(0,*) 'Run-time check failed'
      write(0,*) 'Condition: ${cond.replace("'", "''")}$' 
      #:if msg is not None
        write(*,*) 'Message: ', ${msg}$
      #:endif
      write(*,*) 'File: ${_FILE_}$'
      write(*,*) 'Line: ', ${_LINE_}$
      err_code = 1
      ! should be throwing error or aborting instead
    end if
#:enddef

#:endmute

program test_fypp
use, intrinsic :: iso_c_binding
implicit none

    integer :: err_code

#:for rank  in ranks
#:for dtype,ftype in types
${ftype}$, allocatable :: var_${dtype}$_${rank}$(${dim[rank]}$)
#:endfor
#:endfor
  integer :: a
  a = 1

  err_code = 0

  @:print_code_location()
  @:ensure( a == 2, "a must be 2" )

  if( err_code == 0 ) then
    STOP 1 ! error
  endif

end program