
add_custom_target( fctest_includes SOURCES
       fctest/fctest.h
       tools/fctest_generate_runner.py )

install(PROGRAMS tools/fctest_generate_runner.py
        DESTINATION share/fctest/tools )

ecbuild_add_library( TARGET fctest
                     SOURCES fctest/fctest.F90
                             fctest/fctest.h
                     INSTALL_HEADERS LISTED )

add_subdirectory(examples)
