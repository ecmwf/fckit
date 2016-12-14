add_custom_target( fctest_includes SOURCES
       fctest.h
       ../../tools/fctest_generate_runner.py )

install( FILES ../../tools/fctest_generate_runner.py DESTINATION libexec )

configure_file(fckit_defines.h.in  fckit_defines.h)

if( HAVE_ECKIT )
list( APPEND eckit_required_src
    module/fckit.F90
    module/fckit_refcounted.cc
    module/fckit_refcounted.F90
    module/fckit_mpi.cc
    module/fckit_mpi.F90
    module/fckit_main.cc
    module/fckit_main.F90
    module/fckit_resource.cc
    module/fckit_resource.F90
    module/fckit_log.cc
    module/fckit_log.F90
    Libfckit.h
    Libfckit.cc
    Main.h
    Main.cc
    Log.h
    Log.cc
    Log.F90
)

if( FCKIT_HAVE_MPI AND EC_OS_NAME MATCHES "macosx" )
  list( APPEND FCKIT_DEFINITIONS BUG_ECKIT_166 )
  list( APPEND ECKIT_LIBRARIES ${MPI_CXX_LIBRARIES} )
  ecbuild_include_mpi()
endif()

list( APPEND ECKIT_LIBRARIES ${ECBUILD_CXX_IMPLICIT_LINK_LIBRARIES} )

endif()

if( EC_HAVE_Fortran_FINALIZATION )
    list( APPEND FCKIT_DEFINITIONS EC_HAVE_Fortran_FINALIZATION )
endif()

if( Fortran_FINAL_FUNCTION_RESULT )
    list( APPEND FCKIT_DEFINITIONS Fortran_FINAL_FUNCTION_RESULT )
endif()

if( Fortran_FINAL_UNINITIALIZED_LHS )
    list( APPEND FCKIT_DEFINITIONS Fortran_FINAL_UNINITIALIZED_LHS )
endif()

if( Fortran_FINAL_UNINITIALIZED_INTENT_OUT )
    list( APPEND FCKIT_DEFINITIONS Fortran_FINAL_UNINITIALIZED_INTENT_OUT )
endif()

if( Fortran_FINAL_UNINITIALIZED_INTENT_INOUT )
    list( APPEND FCKIT_DEFINITIONS Fortran_FINAL_UNINITIALIZED_INTENT_INOUT )
endif()

if( BUILD_SHARED_LIBS )
    set( fckit_type SHARED )
else()
    set( fckit_type STATIC )
endif()

ecbuild_add_library( TARGET     fckit
                     LINKER_LANGUAGE Fortran
                     SOURCES
                                fctest.F90
                                module/fckit_array.F90
                                module/fckit_C_interop.cc
                                module/fckit_C_interop.F90
                                module/fckit_object.F90
                                module/fckit_refcounted_fortran.F90
                                ${eckit_required_src}
                     PRIVATE_INCLUDES
                                ${ECKIT_INCLUDE_DIRS}
                                ${CMAKE_CURRENT_BINARY_DIR}
                     LIBS
                                ${ECKIT_LIBRARIES}
                     DEFINITIONS
                                ${FCKIT_DEFINITIONS}

                     INSTALL_HEADERS_LIST
                                fctest.h
                                Log.h
                                Main.h

                     HEADER_DESTINATION  ${INSTALL_INCLUDE_DIR}/fckit
                    )

# install( FILES fctest.h DESTINATION ${INSTALL_INCLUDE_DIR}/fckit )
# install( FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/fctest.mod DESTINATION ${INSTALL_INCLUDE_DIR} )
