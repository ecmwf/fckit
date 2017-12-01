# (C) Copyright 2013-2017 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

############################################################################################
# FCKIT

cmake_minimum_required( VERSION 2.8 FATAL_ERROR )

set( ENABLE_OS_TESTS            OFF   CACHE BOOL " " )
set( ENABLE_LARGE_FILE_SUPPORT  OFF   CACHE BOOL " " )

project( fckit CXX Fortran )

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild/cmake")
set( ECBUILD_DEFAULT_BUILD_TYPE Release )

include( ecbuild_system NO_POLICY_SCOPE )

ecbuild_requires_macro_version( 2.5 )

###############################################################################
# project

ecbuild_declare_project()

################################################################################################
# options & dependencies

ecbuild_find_python( REQUIRED NO_LIBS )

### Fortran ...
ecbuild_enable_fortran( REQUIRED MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/module )

# Experimental... (read: not working!)
#ecbuild_add_option( FEATURE FORTRAN_FINALIZATION
#                    DEFAULT OFF )
ecbuild_warn( "ENABLE_FORTRAN_FINALIZATION = ${ENABLE_FORTRAN_FINALIZATION} " )
if( ENABLE_FORTRAN_FINALIZATION )
  ecbuild_check_fortran( FEATURES finalization )
ecbuild_warn( "EC_HAVE_Fortran_FINALIZATION = ${EC_HAVE_Fortran_FINALIZATION} " )
  if( EC_HAVE_Fortran_FINALIZATION )
    include( final-support )
    check_final_support()
    message( "Fortran_FINAL_FUNCTION_RESULT            = ${Fortran_FINAL_FUNCTION_RESULT}")
    message( "Fortran_FINAL_UNINITIALIZED_LOCAL        = ${Fortran_FINAL_UNINITIALIZED_LOCAL}")
    message( "Fortran_FINAL_UNINITIALIZED_INTENT_OUT   = ${Fortran_FINAL_UNINITIALIZED_INTENT_OUT}")
    message( "Fortran_FINAL_UNINITIALIZED_INTENT_INOUT = ${Fortran_FINAL_UNINITIALIZED_INTENT_INOUT}")
  endif()
endif()

ecbuild_add_option( FEATURE ECKIT
                    DESCRIPTION "Wrap ecKit functionality"
                    REQUIRED_PACKAGES "PROJECT eckit VERSION 0.18" )
if( HAVE_ECKIT )
    set( ECKIT_LIBRARIES eckit eckit_mpi )
    ecbuild_add_cxx11_flags()
endif()

if( NOT ECKIT_HAVE_MPI )
  ecbuild_warn("ecKit has been compiled without MPI. This causes fckit to not be able to run parallel executables.")
endif()

############################################################################################
# export package info

list( APPEND FCKIT_INCLUDE_DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${CMAKE_CURRENT_BINARY_DIR}/src
    ${CMAKE_Fortran_MODULE_DIRECTORY})
set( FCKIT_LIBRARIES  fckit )

############################################################################################
# sources

set( FCTEST_GENERATOR ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/tools/fctest_generate_runner.py )
include(add_fctest)

include_directories( ${FCKIT_INCLUDE_DIRS} )

set( HAVE_FCTEST 1)
add_subdirectory( src )
add_subdirectory( doc )

############################################################################################
# finalize

ecbuild_print_summary()
#####################################################################################

if(ECBUILD_INSTALL_FORTRAN_MODULES)
  install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/${CMAKE_CFG_INTDIR} DESTINATION ${INSTALL_INCLUDE_DIR} )
endif()

set( INSTALL_LIBEXEC_DIR libexec )
if( NOT PROJECT_NAME STREQUAL CMAKE_PROJECT_NAME )
  set( FCTEST_GENERATOR ${FCTEST_GENERATOR} PARENT_SCOPE )
endif()
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/cmake/add_fctest.cmake FCKIT_IMPORT)

ecbuild_install_project( NAME fckit )
