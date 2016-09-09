add_custom_target( fctest_includes SOURCES
       fckit/fctest.h
       ../tools/fctest_generate_runner.py )
       
install( FILES ../tools/fctest_generate_runner.py DESTINATION libexec )

ecbuild_add_library( TARGET     fckit
                     SOURCES 
                                fckit/fctest.F90
                                fckit/fckit_array.F90
                                fckit/fckit_C_interop.cc
                                fckit/fckit_C_interop.F90
                    )
                     
install( FILES fckit/fctest.h DESTINATION ${INSTALL_INCLUDE_DIR}/fckit )
install( FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/fctest.mod DESTINATION ${INSTALL_INCLUDE_DIR} )

add_subdirectory(fctest_examples)
add_subdirectory(tests)