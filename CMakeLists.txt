# (C) Copyright 2013 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

############################################################################################
# FCKIT

cmake_minimum_required( VERSION 3.6 FATAL_ERROR )
if( POLICY CMP0074 )
  cmake_policy( SET CMP0074 NEW )
  # This policy allows to search for packages with <package>_ROOT variables
  #                        (only supported with CMake 3.12 and above)
  # This policy can be removed once cmake_minimum_required( VERSION 3.12 ) is used
endif()

find_package( ecbuild 3.0.0 REQUIRED )

project( fckit LANGUAGES C CXX Fortran )

set(CMAKE_DIRECTORY_LABELS "fckit")
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

################################################################################################
# options & dependencies

ecbuild_find_python( REQUIRED NO_LIBS )

### Fortran ...
ecbuild_enable_fortran( REQUIRED MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/module )

ecbuild_check_fortran( FEATURES finalization )

set( FEATURE_FINAL_DEFAULT ON )
set( PGIBUG_ATLAS_197 0 )
if( CMAKE_Fortran_COMPILER_ID MATCHES "PGI" )
  if( ${CMAKE_Fortran_COMPILER_VERSION} VERSION_LESS 19.4 )
      set( PGIBUG_ATLAS_197 1 )
  endif()
  if( ${CMAKE_Fortran_COMPILER_VERSION} VERSION_LESS 19.10 )
    # Compilation works, but runtime segmentation faults occur (tested with pgi/17.7)
    set( FEATURE_FINAL_DEFAULT OFF )
  else()
    list( APPEND FCKIT_QUARANTAINED_TESTS fckit_test_shared_ptr )
    # deallocation of list of shared_ptr only seems to deallocate first entry
  endif()
endif()

ecbuild_add_option( FEATURE FINAL
                    DESCRIPTION "Enable automatic finalisation for derived types (destructors)"
                    DEFAULT ${FEATURE_FINAL_DEFAULT}
                    CONDITION EC_HAVE_Fortran_FINALIZATION )

if( fckit_HAVE_FINAL )
    include( final-support )
    check_final_support()
    ecbuild_info( "FCKIT_HAVE_FINAL [1]")
    ecbuild_info( "  FCKIT_FINAL_FUNCTION_RESULT              = ${FCKIT_FINAL_FUNCTION_RESULT}")
    ecbuild_info( "  FCKIT_FINAL_UNINITIALIZED_LOCAL          = ${FCKIT_FINAL_UNINITIALIZED_LOCAL}")
    ecbuild_info( "  FCKIT_FINAL_UNINITIALIZED_INTENT_OUT     = ${FCKIT_FINAL_UNINITIALIZED_INTENT_OUT}")
    ecbuild_info( "  FCKIT_FINAL_UNINITIALIZED_INTENT_INOUT   = ${FCKIT_FINAL_UNINITIALIZED_INTENT_INOUT}")
    ecbuild_info( "  FCKIT_FINAL_NOT_PROPAGATING              = ${FCKIT_FINAL_NOT_PROPAGATING}")
    ecbuild_info( "  FCKIT_FINAL_NOT_INHERITING               = ${FCKIT_FINAL_NOT_INHERITING}")
    ecbuild_info( "  FCKIT_FINAL_BROKEN_FOR_ALLOCATABLE_ARRAY = ${FCKIT_FINAL_BROKEN_FOR_ALLOCATABLE_ARRAY}")
    ecbuild_info( "  FCKIT_FINAL_BROKEN_FOR_AUTOMATIC_ARRAY   = ${FCKIT_FINAL_BROKEN_FOR_AUTOMATIC_ARRAY}")
endif()
if( NOT fckit_HAVE_FINAL )
    ecbuild_info( "fckit_HAVE_FINAL [0]")
    set( FCKIT_FINAL_FUNCTION_RESULT              0 )
    set( FCKIT_FINAL_UNINITIALIZED_LOCAL          0 )
    set( FCKIT_FINAL_UNINITIALIZED_INTENT_OUT     0 )
    set( FCKIT_FINAL_UNINITIALIZED_INTENT_INOUT   0 )
    set( FCKIT_FINAL_NOT_PROPAGATING              0 )
    set( FCKIT_FINAL_NOT_INHERITING               0 )
    set( FCKIT_FINAL_BROKEN_FOR_ALLOCATABLE_ARRAY 0 )
    set( FCKIT_FINAL_BROKEN_FOR_AUTOMATIC_ARRAY   0 )
endif()

ecbuild_find_package( NAME eckit VERSION 0.21.0 QUIET )
ecbuild_add_option( FEATURE ECKIT
                    DESCRIPTION "Wrap ecKit functionality"
                    CONDITION eckit_FOUND )
if( NOT fckit_HAVE_ECKIT )
    ecbuild_warn("ecKit could not be found. This disables various fckit features such as MPI, Configuration, Logging")
endif()

## MPI
set( fckit_HAVE_ECKIT_MPI_PARALLEL 0 )
if( fckit_HAVE_ECKIT )
  if( NOT ECKIT_HAVE_MPI )
    set( fckit_HAVE_ECKIT_MPI_PARALLEL 0 )
    set( HAVE_MPI 0 )
  else()
    set( fckit_HAVE_ECKIT_MPI_PARALLEL 1 )
    set( HAVE_MPI 1 )
  endif()
endif()
set( fckit_HAVE_MPI ${HAVE_MPI} )
if( fckit_HAVE_ECKIT AND NOT fckit_HAVE_MPI )
  ecbuild_warn("ecKit has been compiled without MPI. This causes fckit to not be able to run parallel executables.")
endif()

if( eckit_VERSION VERSION_LESS "0.25" )
  set( ECKIT_IMPROVED_MPI 0 )
  if( fckit_HAVE_ECKIT )
    ecbuild_warn( "eckit version does not allow support for" 
                  "- fckit_mpi_comm%set_default()"
                  "- fckit_mpi_comm%name()"
                  "- fckit_mpi_comm%delete()" )
  endif()
else()
  set( ECKIT_IMPROVED_MPI 1 )
endif()

################################################################################################
# export package info

set( FCKIT_LIBRARIES  fckit )

################################################################################################
# sources

set( FYPP ${CMAKE_CURRENT_SOURCE_DIR}/tools/fckit-eval.sh ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/tools/fckit-fypp.py )
set( FCTEST_GENERATOR ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/tools/fctest-generate-runner.py )
include( fckit_preprocess_fypp )
include( add_fctest )

add_subdirectory( src )
add_subdirectory( doc )

################################################################################################
# finalize

ecbuild_print_summary()
################################################################################################

if( ECBUILD_INSTALL_FORTRAN_MODULES )
    install( DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/${CMAKE_CFG_INTDIR}
             DESTINATION module/fckit
             COMPONENT modules )
endif()
if( NOT PROJECT_NAME STREQUAL CMAKE_PROJECT_NAME )
  set( FCTEST_GENERATOR ${FCTEST_GENERATOR} PARENT_SCOPE )
  set( FYPP ${FYPP} PARENT_SCOPE )
endif()
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/cmake/add_fctest.cmake ADD_FCTEST)
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/cmake/fckit_preprocess_fypp.cmake FCKIT_PREPROCESS_FYPP )

set( install_permissions OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE )
install( FILES tools/fctest-generate-runner.py DESTINATION libexec PERMISSIONS ${install_permissions} )
install( FILES tools/fckit-eval.sh DESTINATION libexec PERMISSIONS ${install_permissions} )
install( FILES contrib/fypp-679566c-20190108/bin/fypp DESTINATION libexec RENAME fckit-fypp.py PERMISSIONS ${install_permissions} )

ecbuild_install_project( NAME fckit )
