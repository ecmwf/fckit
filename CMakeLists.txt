# (C) Copyright 1996-2015 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

############################################################################################
# FCKIT

cmake_minimum_required( VERSION 2.8.11 FATAL_ERROR )

set( ENABLE_OS_TESTS       OFF   CACHE BOOL " " )
set( ENABLE_LARGE_FILE_SUPPORT  OFF   CACHE BOOL " " )

project( fckit CXX Fortran )

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild/cmake")
set( ECBUILD_DEFAULT_BUILD_TYPE Release )

include( ecbuild_system NO_POLICY_SCOPE )

ecbuild_requires_macro_version( 1.9 )

###############################################################################
# project

ecbuild_declare_project()

################################################################################################
# options & dependencies

ecbuild_find_python( REQUIRED NO_LIBS )

### Fortran ...
ecbuild_enable_fortran( REQUIRED MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/module )

ecbuild_check_fortran( FEATURES finalization )

### eckit ...
ecbuild_add_option( FEATURE ECKIT
                    DESCRIPTION "Wrap ecKit functionality"
                    REQUIRED_PACKAGES "PROJECT eckit VERSION 0.12.3"
                   )
if( HAVE_ECKIT )
    set( ECKIT_LIBRARIES eckit )
endif()

############################################################################################
# export package info

list( APPEND FCKIT_INCLUDE_DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${CMAKE_CURRENT_BINARY_DIR}/src
    ${CMAKE_Fortran_MODULE_DIRECTORY})
set( FCKIT_LIBRARIES  fckit )

#
# --- IMPORT ---
#
set( INSTALL_LIBEXEC_DIR libexec )
# At this point fckit-import.cmake has not been configured in the build tree yet
# Include the template and fix the path to fctest_generate_runner.py
# Set Policy CMP0053 to allow to include fckit-import.cmake.in directly
# Run "cmake --help-policy CMP0053" for policy details
if(POLICY CMP0053)
  cmake_policy(PUSH)
  cmake_policy(SET CMP0053 OLD)
endif()
include(${CMAKE_CURRENT_SOURCE_DIR}/fckit-import.cmake.in)
if(POLICY CMP0053)
  cmake_policy(POP)
endif()
set( FCTEST_GENERATOR ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/tools/fctest_generate_runner.py )

if( NOT PROJECT_NAME STREQUAL CMAKE_PROJECT_NAME )
  set( FCTEST_GENERATOR ${FCTEST_GENERATOR} PARENT_SCOPE )
endif()

############################################################################################
# sources

include_directories( ${FCKIT_INCLUDE_DIRS} )

set( HAVE_FCTEST 1)
add_subdirectory( src )

############################################################################################
# finalize

ecbuild_print_summary()
#####################################################################################

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/${CMAKE_CFG_INTDIR} DESTINATION ${INSTALL_INCLUDE_DIR} )

ecbuild_install_project( NAME fckit )
