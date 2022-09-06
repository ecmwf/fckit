# (C) Copyright 2013 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

cmake_policy( SET CMP0064 NEW ) # Recognize ``TEST`` as operator for the ``if()`` command. (introduced in CMake version 3.4)

add_fctest( TARGET  fckit_test_array
            LINKER_LANGUAGE Fortran
            SOURCES test_array.F90
            LIBS    fckit)

add_fctest( TARGET  fckit_test_resource
            LINKER_LANGUAGE Fortran
            SOURCES test_resource.F90
            ARGS    -integer 10 -long 5000000000 -float 0.123456 -double 0.1234567890123456789 -string "hello world"
            CONDITION HAVE_ECKIT
            LIBS    fckit)

add_fctest( TARGET  fckit_test_log
            LINKER_LANGUAGE Fortran
            SOURCES test_log.F90
            CONDITION HAVE_ECKIT
            LIBS    fckit)

add_fctest( TARGET  fckit_test_shared_ptr
            LINKER_LANGUAGE Fortran
            SOURCES
              test_shared_ptr.F90
              test_shared_ptr.cc
            LIBS    fckit)

add_fctest( TARGET  fckit_test_mpi
            LINKER_LANGUAGE Fortran
            SOURCES test_mpi.F90
            MPI 4
            CONDITION HAVE_MPI
            LIBS    fckit)

add_fctest( TARGET  fckit_test_abort
            LINKER_LANGUAGE Fortran
            SOURCES test_abort.F90
            CONDITION HAVE_ECKIT
            LIBS    fckit)

add_fctest( TARGET  fckit_test_configuration
            LINKER_LANGUAGE Fortran
            SOURCES test_configuration.F90
            CONDITION HAVE_ECKIT
            LIBS    fckit)
if( TEST fckit_test_configuration )
  if( NOT CMAKE_VERSION VERSION_LESS 3.15 ) # support for COMPILE_LANG_AND_ID generator expression
    target_compile_options( fckit_test_configuration PRIVATE $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-Wno-uninitialized> )
  endif()
endif()

add_fctest( TARGET  fckit_test_configuration_fails
            LINKER_LANGUAGE Fortran
            SOURCES test_configuration_fails.F90
            CONDITION HAVE_ECKIT
            LIBS    fckit)
if( TEST fckit_test_configuration_fails )
    set_tests_properties( fckit_test_configuration_fails PROPERTIES
        PASS_REGULAR_EXPRESSION "Could not find \"missing\" in Configuration"
        TIMEOUT 100 )
endif()

add_fctest( TARGET  fckit_test_broadcast_file
            LINKER_LANGUAGE Fortran
            SOURCES test_broadcast_file.F90
            MPI 4
            CONDITION HAVE_MPI
            LIBS    fckit)

add_fctest( TARGET fckit_test_fypp
  LINKER_LANGUAGE Fortran
  SOURCES test_fypp.fypp
  LIBS fckit
)

ecbuild_add_test( TARGET  fckit_test_configuration_cpp
            LINKER_LANGUAGE Fortran
            SOURCES
            test_configuration.cc
            test_configuration_fortcode.F90
            INCLUDES ${ECKIT_INCLUDE_DIRS}
            CONDITION HAVE_ECKIT
            LIBS    fckit  eckit eckit_mpi )

if( TEST fckit_test_configuration_cpp )
  set_property( TEST fckit_test_configuration_cpp APPEND PROPERTY LABELS "fortran" )
endif()

ecbuild_add_test( TARGET fckit_test_cpp
  LINKER_LANGUAGE Fortran
  SOURCES test_cpp.cc test_cpp_fortcode.F90
  INCLUDES ${ECKIT_INCLUDE_DIRS}
  ENVIRONMENT "DEBUG=1"
  CONDITION HAVE_ECKIT
  LIBS fckit eckit eckit_mpi )

### Fix linking for C++ test executables linked with Fortran linker
if( CMAKE_Fortran_COMPILER_ID MATCHES "PGI|NVHPC" )
  set( NO_MAIN "-Mnomain" )
elseif( CMAKE_Fortran_COMPILER_ID MATCHES "Intel" )
  set( NO_MAIN "-nofor-main" )
elseif( CMAKE_Fortran_COMPILER_ID MATCHES "Fujitsu" )
  set( NO_MAIN "-mlcmain=main" )
endif()
if( NO_MAIN )
  foreach( _test fckit_test_configuration_cpp fckit_test_cpp )
    if( TEST ${_test} )
      set_target_properties(${_test} PROPERTIES LINK_FLAGS ${NO_MAIN} )
    endif()
  endforeach()
endif()

### Quarantaine tests if requested
foreach( _test ${FCKIT_QUARANTAINED_TESTS} )
    if( TEST ${_test} )
      set_tests_properties( ${_test} PROPERTIES WILL_FAIL 1 )
      ecbuild_warn("Test ${_test} is quarantained (shows as passed)")
    endif()
endforeach()

### Test downstream find_package(fckit) projects
add_subdirectory( test_downstream_fypp )
add_subdirectory( test_downstream_fctest )
