add_custom_target( fctest_includes SOURCES
       fckit/fctest.h
       ../tools/fctest_generate_runner.py )
       
install( FILES ../tools/fctest_generate_runner.py DESTINATION libexec )

if( HAVE_ECKIT )
list( APPEND eckit_required_src 
    fckit/fckit_refcounted.cc
    fckit/fckit_refcounted.F90
)
endif()

ecbuild_add_library( TARGET     fckit
                     SOURCES 
                                fckit/fctest.F90
                                fckit/fckit_array.F90
                                fckit/fckit_C_interop.cc
                                fckit/fckit_C_interop.F90
                                fckit/fckit_object.F90
                                ${eckit_required_src}
                     PRIVATE_INCLUDES
                                ${ECKIT_INCLUDE_DIRS}
                     LIBS
                                ${ECKIT_LIBRARIES}
                    )
                     
install( FILES fckit/fctest.h DESTINATION ${INSTALL_INCLUDE_DIR}/fckit )
install( FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/fctest.mod DESTINATION ${INSTALL_INCLUDE_DIR} )

add_subdirectory(fctest_examples)
add_subdirectory(tests)