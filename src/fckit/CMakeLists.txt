# (C) Copyright 2013 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

configure_file(fckit.h.in  fckit.h)
install( FILES ${CMAKE_CURRENT_BINARY_DIR}/fckit.h DESTINATION include/fckit )
set( source_files_properties fckit.h.in PROPERTIES HEADER_FILE_ONLY TRUE )

if( HAVE_ECKIT )
  list( APPEND eckit_required_src
      module/fckit.F90
      module/fckit_pathname.F90
      module/fckit_configuration.F90
      module/fckit_configuration.cc
      module/fckit_mpi.cc
      module/fckit_mpi.fypp
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
      module/fckit_owned.cc
      module/fckit_owned_object.F90
      Libfckit.h
      Libfckit.cc
      Main.h
      Main.cc
      Log.h
      Log.cc
  )
  set( eckit_required_libs eckit eckit_mpi )
endif()

if( BUILD_SHARED_LIBS )
    set( fckit_type SHARED )
else()
    set( fckit_type STATIC )
endif()

if( CMAKE_Fortran_COMPILER_ID MATCHES "PGI|NVHPC" )
  # Bug tested with pgi/17.7 (fixed pgi/19.4) requires fckit to be compiled statically
  if( ${CMAKE_Fortran_COMPILER_VERSION} VERSION_LESS 19.4 )
      ecbuild_warn( "Bug in pgi < 19.4 requires fckit to be compiled statically" )
      set( fckit_type STATIC )
  endif()

  # Turn off all compiler warnings for reason :
  #   PGF90-W-0435-Array declared with zero size
  #      (/tmp/nawd/atlas-bundle/source/fckit/src/fckit/module/fckit_array.F90: 25)
  set_source_files_properties( module/fckit_array.F90  PROPERTIES COMPILE_FLAGS "-w" )
endif()


ecbuild_add_library( 
    TARGET     fckit
    LINKER_LANGUAGE CXX
        # We need the C++ linker for the error handling to work
        # Otherwise death by recursion in function "fckit_terminate"
        # when uncaught exceptions are encountered
    SOURCES
               fctest.h
               fctest.F90
               Log.F90
               module/fckit_array.F90
               module/fckit_C_interop.cc
               module/fckit_C_interop.F90
               module/fckit_object.F90
               module/fckit_final.F90
               module/fckit_refcount.F90
               module/fckit_shared_ptr.F90
               module/fckit_shared_object.F90
               ${eckit_required_src}
    PRIVATE_INCLUDES
               ${ECKIT_INCLUDE_DIRS}
               ${CMAKE_CURRENT_BINARY_DIR}

    INSTALL_HEADERS_LIST
               fctest.h
               Log.h
               Main.h

    HEADER_DESTINATION  include/fckit

    PUBLIC_INCLUDES
        $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/src>
        $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
        $<INSTALL_INTERFACE:include>
        $<INSTALL_INTERFACE:module/fckit>

    PRIVATE_LIBS
       ${eckit_required_libs}

    TYPE ${fckit_type}

)

fckit_target_preprocess_fypp( fckit )
