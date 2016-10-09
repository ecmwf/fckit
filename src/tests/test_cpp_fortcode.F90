subroutine run() bind(c)
  use fckit_log_module
  use fckit_main_module
  call log%info("info")
  call log%debug("debug")
  if( .not. main%debug() ) then
    call log%warning("debug not active")
  endif
end subroutine