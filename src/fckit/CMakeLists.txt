# (C) Copyright 2013-2017 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

add_custom_target( fctest_includes SOURCES
       fctest.h
       ../../tools/fctest_generate_runner.py )

install( FILES ../../tools/fctest_generate_runner.py DESTINATION libexec )

configure_file(fckit.h.in  fckit.h)
install( FILES ${CMAKE_CURRENT_BINARY_DIR}/fckit.h DESTINATION ${INSTALL_INCLUDE_DIR}/fckit )

set( FCKIT_HAVE_ECKIT 0 )
if( HAVE_ECKIT )
  set( FCKIT_HAVE_ECKIT 1 )
  list( APPEND eckit_required_src
      module/fckit.F90
      module/fckit_pathname.F90
      module/fckit_configuration.F90
      module/fckit_configuration.cc
      module/fckit_mpi.cc
      module/fckit_mpi.F90
      module/fckit_main.cc
      module/fckit_main.F90
      module/fckit_exception.F90
      module/fckit_signal.F90
      module/fckit_resource.cc
      module/fckit_resource.F90
      module/fckit_log.cc
      module/fckit_log.F90
      module/fckit_buffer.cc
      module/fckit_buffer.F90
      module/fckit_owned_object.F90
      Libfckit.h
      Libfckit.cc
      Main.h
      Main.cc
      Log.h
      Log.cc
      Log.F90
  )
endif()

if( BUILD_SHARED_LIBS )
    set( fckit_type SHARED )
else()
    set( fckit_type STATIC )
endif()

set( FCKIT_LINKER_LANGUAGE CXX )
if( HAVE_ECKIT )
  # We need the C++ linker for the error handling to work
  # Otherwise death by recursion in function "fckit_terminate"
  # when uncaught exceptions are encountered
  set( FCKIT_LINKER_LANGUAGE CXX )
endif()

ecbuild_add_library( TARGET     fckit
                     LINKER_LANGUAGE ${FCKIT_LINKER_LANGUAGE}
                     SOURCES
                                fctest.F90
                                module/fckit_array.F90
                                module/fckit_C_interop.cc
                                module/fckit_C_interop.F90
                                module/fckit_object.F90
                                module/fckit_final.F90
                                module/fckit_owned.cc
                                module/fckit_refcount.F90
                                module/fckit_shared_ptr.F90
                                module/fckit_shared_object.F90
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

if( CMAKE_Fortran_COMPILER_ID MATCHES "PGI" )
  # Turn off all compiler warnings for reason :
  #   PGF90-W-0435-Array declared with zero size
  #      (/tmp/nawd/atlas-bundle/source/fckit/src/fckit/module/fckit_array.F90: 25)
  set_source_files_properties( module/fckit_array.F90  PROPERTIES COMPILE_FLAGS "-w" )
endif()
