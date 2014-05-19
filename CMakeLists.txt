# (C) Copyright 1996-2014 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

############################################################################################
# FCTEST
#
# TODO:
#	* add renumbering to improve cache performance
#	* add detection of OpenMP flags (mayeb move to ecbuild)

cmake_minimum_required( VERSION 2.8.4 FATAL_ERROR )

project( fctest Fortran C )

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild/cmake")
set( ECBUILD_DEFAULT_BUILD_TYPE Release )

include( ecbuild_system )

ecbuild_requires_macro_version( 1.3 )

###############################################################################
# project

ecbuild_declare_project()

################################################################################################
# options & dependencies

ecbuild_find_python( REQUIRED )

### Fortran ...
ecbuild_enable_fortran( REQUIRED MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/module )

############################################################################################
# export package info

set( FCTEST_INCLUDE_DIRS   ${CMAKE_CURRENT_SOURCE_DIR}/src ${CMAKE_CURRENT_BINARY_DIR}/src )
set( FCTEST_GENERATOR ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/src/tools/fctest_generate_runner.py )
SET( FCTEST_IMPORT_FILE ${CMAKE_CURRENT_SOURCE_DIR}/fctest-import.cmake.in )
configure_file( "${FCTEST_IMPORT_FILE}" "${PROJECT_BINARY_DIR}/fctest-import.cmake" @ONLY )
include( "${PROJECT_BINARY_DIR}/fctest-import.cmake" )

############################################################################################
# sources

# include( atlas_compiler_flags )

include_directories( ${FCTEST_INCLUDE_DIRS} ${FCTEST_EXTRA_INCLUDE_DIRS} )

set( HAVE_FCTEST 1)
add_subdirectory( src )

############################################################################################
# finalize

SET( FCTEST_IMPORT_FILE ${CMAKE_CURRENT_SOURCE_DIR}/fctest-imports.cmake.in )
ecbuild_install_project( NAME fctest )

ecbuild_print_summary()
