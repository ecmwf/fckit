subroutine run() bind(c)
  use fckit_log_module
  call log%info("info")
end subroutine