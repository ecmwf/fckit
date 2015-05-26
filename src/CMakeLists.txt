
add_custom_target( fctest_includes SOURCES
       fctest/fctest.h
       tools/fctest_generate_runner.py )
       
install( FILES tools/fctest_generate_runner.py DESTINATION libexec )

ecbuild_add_library( TARGET fctest
                     SOURCES fctest/fctest.F90 )
                     
install( FILES fctest/fctest.h DESTINATION ${INSTALL_INCLUDE_DIR}/fctest )
install( FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/fctest.mod DESTINATION ${INSTALL_INCLUDE_DIR} )

add_subdirectory(examples)
