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

set( ENABLE_OS_TESTS            OFF   CACHE BOOL " " )
set( ENABLE_LARGE_FILE_SUPPORT  OFF   CACHE BOOL " " )

project( fckit C CXX Fortran )

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild/cmake")

set( CMAKE_DIRECTORY_LABELS "fckit" )

set( ECBUILD_DEFAULT_BUILD_TYPE Release )

include( ecbuild_system NO_POLICY_SCOPE )

ecbuild_requires_macro_version( 2.7 )

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

################################################################################################
# project

ecbuild_declare_project()

################################################################################################
# options & dependencies

ecbuild_find_python( REQUIRED NO_LIBS )

### Fortran ...
ecbuild_enable_fortran( REQUIRED MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/module )

ecbuild_check_fortran( FEATURES finalization )

set( FEATURE_FINAL_DEFAULT ON )
if( CMAKE_Fortran_COMPILER_ID MATCHES "PGI" )
  # Compilation works, but runtime segmentation faults occur (tested with pgi/17.7)
  set( FEATURE_FINAL_DEFAULT OFF )
endif()

ecbuild_add_option( FEATURE FINAL
                    DESCRIPTION "Enable automatic finalisation for derived types (destructors)"
                    DEFAULT ${FEATURE_FINAL_DEFAULT}
                    CONDITION EC_HAVE_Fortran_FINALIZATION )

set( FCKIT_HAVE_FINAL 0 )
if( ENABLE_FINAL )
    set( FCKIT_HAVE_FINAL 1 )
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
if( NOT FCKIT_HAVE_FINAL )
    ecbuild_info( "FCKIT_HAVE_FINAL [0]")
    set( FCKIT_FINAL_FUNCTION_RESULT              0 )
    set( FCKIT_FINAL_UNINITIALIZED_LOCAL          0 )
    set( FCKIT_FINAL_UNINITIALIZED_INTENT_OUT     0 )
    set( FCKIT_FINAL_UNINITIALIZED_INTENT_INOUT   0 )
    set( FCKIT_FINAL_NOT_PROPAGATING              0 )
    set( FCKIT_FINAL_NOT_INHERITING               0 )
    set( FCKIT_FINAL_BROKEN_FOR_ALLOCATABLE_ARRAY 0 )
    set( FCKIT_FINAL_BROKEN_FOR_AUTOMATIC_ARRAY   0 )
endif()

ecbuild_add_option( FEATURE ECKIT
                    DESCRIPTION "Wrap ecKit functionality"
                    REQUIRED_PACKAGES "PROJECT eckit VERSION 0.21.0" )

ecbuild_add_cxx11_flags()

if( HAVE_ECKIT )
    if( NOT ECKIT_HAVE_MPI )
        ecbuild_warn("ecKit has been compiled without MPI. This causes fckit to not be able to run parallel executables.")
    elseif(NOT eckit_mpi IN_LIST ECKIT_LIBRARIES)
        list(APPEND ECKIT_LIBRARIES eckit_mpi)
    endif()
endif()

if( ECKIT_VERSION VERSION_LESS "0.25" )
  set( ECKIT_IMPROVED_MPI 0 )
  if( HAVE_ECKIT )
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

list( APPEND FCKIT_INCLUDE_DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${CMAKE_CURRENT_BINARY_DIR}/src
    ${CMAKE_Fortran_MODULE_DIRECTORY})
set( FCKIT_LIBRARIES  fckit )

################################################################################################
# sources

set( FYPP ${CMAKE_CURRENT_SOURCE_DIR}/tools/fckit-eval.sh ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/tools/fckit-fypp.py )
set( FCTEST_GENERATOR ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/tools/fctest-generate-runner.py )
include( fckit_preprocess_fypp )
include( add_fctest )

set( HAVE_FCTEST 1)
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
