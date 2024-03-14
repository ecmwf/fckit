# (C) Copyright 2013 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

cmake_minimum_required( VERSION 3.12 FATAL_ERROR )

find_package( ecbuild 3.4 REQUIRED )

project( fctest_examples VERSION 0.0.0 LANGUAGES Fortran )

ecbuild_enable_fortran( REQUIRED MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/module )

find_package( fckit REQUIRED )

ecbuild_add_library( TARGET fctest_example_lib
                     SOURCES fctest_example_lib.F90
                     NOINSTALL )

add_fctest( TARGET fctest_example_simple
            LINKER_LANGUAGE Fortran
            SOURCES fctest_example_simple.F90
            LIBS    fctest_example_lib )

add_fctest( TARGET fctest_example_with_fixture
            LINKER_LANGUAGE Fortran
            SOURCES fctest_example_with_fixture.F90 )

