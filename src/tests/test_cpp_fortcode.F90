subroutine run() bind(c)
  use fckit_module
  character(len=:), allocatable :: name

  call fckit_main%name(name)
  call fckit_log%info("info: "//name)
  call fckit_log%debug("debug")
  if( .not. fckit_main%debug() ) then
    call fckit_log%warning("debug not active")
  endif
end subroutine
