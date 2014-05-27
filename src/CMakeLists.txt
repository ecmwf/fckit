
add_custom_target( fctest_includes SOURCES
       fctest/fctest.h
       tools/fctest_generate_runner.py )

ecbuild_add_library( TARGET fctest
                     SOURCES fctest/fctest.F90 )

add_subdirectory(examples)
