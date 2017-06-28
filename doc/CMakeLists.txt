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


