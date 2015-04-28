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
ecbuild_enable_fortran( REQUIRED MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/module )


############################################################################################
# sources

set( FCTEST_INCLUDE_DIRS   ${CMAKE_CURRENT_SOURCE_DIR}/src ${CMAKE_CURRENT_BINARY_DIR}/src ${CMAKE_Fortran_MODULE_DIRECTORY} )
set( FCTEST_LIBRARIES  fctest )

include_directories( ${FCTEST_INCLUDE_DIRS} ${FCTEST_EXTRA_INCLUDE_DIRS} )

if( CMAKE_Fortran_COMPILER_ID MATCHES "GNU" )
  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-form -ffree-line-length-none")
endif()

if( CMAKE_Fortran_COMPILER_ID MATCHES "Cray" )
  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -emf -rmoid -lhugetlbfs")
endif()

set( FCTEST_IMPORT_FILE ${CMAKE_CURRENT_SOURCE_DIR}/fctest-import.cmake.in )

set( FCTEST_GENERATOR ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/src/tools/fctest_generate_runner.py )
configure_file( "${FCTEST_IMPORT_FILE}" "${PROJECT_BINARY_DIR}/fctest-import.cmake" @ONLY )
include( "${PROJECT_BINARY_DIR}/fctest-import.cmake" )

set( HAVE_FCTEST 1)
add_subdirectory( src )

############################################################################################
# finalize

set( FCTEST_GENERATOR ${PYTHON_EXECUTABLE} ${CMAKE_INSTALL_PREFIX}/share/fctest/tools/fctest_generate_runner.py )
configure_file( "${FCTEST_IMPORT_FILE}" "${PROJECT_BINARY_DIR}/fctest-import-install.cmake" @ONLY )

install( FILES "${PROJECT_BINARY_DIR}/fctest-import-install.cmake"
         DESTINATION "${INSTALL_CMAKE_DIR}"
         RENAME "fctest-import.cmake" )

install( FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/${CMAKE_CFG_INTDIR}/fctest.mod
         DESTINATION ${INSTALL_INCLUDE_DIR} )

ecbuild_install_project( NAME fctest )



ecbuild_print_summary()
