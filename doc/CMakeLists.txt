# (C) Copyright 2013-2017 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

ecbuild_add_option(
  FEATURE DOCS
  DESCRIPTION "Generate reference documentation"
  REQUIRED_PACKAGES "FORD" )

if( HAVE_DOCS )

  set( FCKIT_DOC fckit_doc )
  if( PROJECT_NAME STREQUAL CMAKE_PROJECT_NAME )
    set( FCKIT_DOC doc )
  endif()

  set( FORDFILE ford.md CACHE INTERNAL "FORD project filename" )
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${FORDFILE}.in "${CMAKE_CURRENT_BINARY_DIR}/${FORDFILE}" @ONLY )

  add_custom_command(OUTPUT "ford/index.html"
  	COMMAND ${FORD_EXECUTABLE} "${CMAKE_CURRENT_BINARY_DIR}/${FORDFILE}"
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")
  add_custom_target( ${FCKIT_DOC} DEPENDS "ford/index.html")

endif()


