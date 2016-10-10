subroutine run() bind(c)
  use fckit_log_module
  use fckit_main_module
  character(len=:), allocatable :: name

  call main%name(name)
  call log%info("info: "//name)
  call log%debug("debug")
  if( .not. main%debug() ) then
    call log%warning("debug not active")
  endif
end subroutine
