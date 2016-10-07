add_custom_target( fctest_includes SOURCES
       fckit/fctest.h
       ../tools/fctest_generate_runner.py )

install( FILES ../tools/fctest_generate_runner.py DESTINATION libexec )

if( HAVE_ECKIT )
list( APPEND eckit_required_src 
    fckit/fckit_refcounted.cc
    fckit/fckit_refcounted.F90
    fckit/fckit_mpi.cc
    fckit/fckit_mpi.F90
    fckit/fckit_runtime.cc
    fckit/fckit_runtime.F90
    fckit/fckit_resource.cc
    fckit/fckit_resource.F90
    fckit/fckit_log.cc
    fckit/fckit_log.F90
)
endif()

if( FCKIT_HAVE_MPI AND EC_OS_NAME MATCHES "macosx" )
  list( APPEND FCKIT_DEFINITIONS BUG_ECKIT_166 )
  list( APPEND ECKIT_LIBRARIES ${MPI_CXX_LIBRARIES} )
  ecbuild_include_mpi()
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

if( fckit_type MATCHES "SHARED" AND CMAKE_Fortran_COMPILER_ID MATCHES "PGI" )
  if( CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 16.8 )
    ecbuild_warn("PGI compiler version tested up to 16.7 does link fckit shared libraries."
                 "Change fckit linking to STATIC")
    set( fckit_type STATIC )
  endif()
endif()

ecbuild_add_library( TARGET     fckit
                     TYPE       ${fckit_type}
                     LINKER_LANGUAGE CXX
                     SOURCES 
                                fckit/fctest.F90
                                fckit/fckit_array.F90
                                fckit/fckit_C_interop.cc
                                fckit/fckit_C_interop.F90
                                fckit/fckit_object.F90
                                fckit/fckit_refcounted_fortran.F90
                                ${eckit_required_src}
                     PRIVATE_INCLUDES
                                ${ECKIT_INCLUDE_DIRS}
                     LIBS
                                ${ECKIT_LIBRARIES}
                     DEFINITIONS
                                ${FCKIT_DEFINITIONS}
                    )

install( FILES fckit/fctest.h DESTINATION ${INSTALL_INCLUDE_DIR}/fckit )
# install( FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/fctest.mod DESTINATION ${INSTALL_INCLUDE_DIR} )

add_subdirectory(fctest_examples)
add_subdirectory(tests)