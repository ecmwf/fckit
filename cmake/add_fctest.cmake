function( fctest_generate_runner )

  set( options           )
  set( single_value_args OUTPUT FILENAME )
  set( multi_value_args DEPENDS )

  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  get_filename_component(base ${_PAR_FILENAME} NAME_WE)
  set(base_abs ${CMAKE_CURRENT_SOURCE_DIR}/${base})
  set(outfile ${CMAKE_CURRENT_BINARY_DIR}/${base}_main.F90)
  set(${_PAR_OUTPUT} ${outfile} PARENT_SCOPE)

  list( APPEND _depends
    ${_PAR_FILENAME}
    ${_PAR_DEPENDS}
  )

  add_custom_command(
    OUTPUT ${outfile}
    COMMAND ${FCTEST_GENERATOR} -i ${CMAKE_CURRENT_SOURCE_DIR}/${_PAR_FILENAME} -o ${outfile}
    DEPENDS ${_depends} )

  set_source_files_properties(${outfile} PROPERTIES GENERATED TRUE)

endfunction()

if( NOT (CMAKE_VERSION VERSION_LESS 3.4 ) )
  set( _cmake_supports_checking_for_TEST TRUE )
  cmake_policy( SET CMP0064 NEW )
endif()

macro( add_fctest )

  ecbuild_add_test( ${ARGV} )

  set( options           )
  set( single_value_args TARGET )
  set( multi_value_args )

  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}" ${_FIRST_ARG} ${ARGN} )

  if( TARGET ${_PAR_TARGET} )
      get_property( test_sources TARGET ${_PAR_TARGET} PROPERTY SOURCES )
      list( GET test_sources 0 TESTSUITE )
      list( REMOVE_ITEM test_sources ${TESTSUITE} )
      set_property( TARGET ${_PAR_TARGET} PROPERTY SOURCES ${test_sources} )
      fctest_generate_runner(
          OUTPUT TESTRUNNER
          FILENAME ${TESTSUITE} )
      target_include_directories( ${_PAR_TARGET} PUBLIC ${FCKIT_INCLUDE_DIRS} )
      target_link_libraries( ${_PAR_TARGET} fckit )
      if( _cmake_supports_checking_for_TEST )
        if( TEST ${_PAR_TARGET} )
          set_property( TEST ${_PAR_TARGET} APPEND PROPERTY LABELS "fortran" )
        endif()
      endif()
      target_sources( ${_PAR_TARGET} PUBLIC ${TESTRUNNER} )
      add_custom_target( ${_PAR_TARGET}_testsuite SOURCES ${TESTSUITE} )
  endif()

endmacro()
