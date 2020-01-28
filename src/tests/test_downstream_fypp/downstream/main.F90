program main
use fckit_module, only: fckit_log
use downstream
use downstream_override
implicit none

call fckit_log%info("Printing downstream compile flags")

call print_downstream_compile_flags()

call fckit_log%info("Asserting downstream compile flags")

call assert_downstream_compile_flags()


call fckit_log%info("Printing downstream_override compile flags")

call print_downstream_override_compile_flags()

call fckit_log%info("Asserting downstream_override compile flags")

call assert_downstream_override_compile_flags()

call fckit_log%info("End of main")

end program