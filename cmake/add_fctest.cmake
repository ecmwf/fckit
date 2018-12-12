# (C) Copyright 2013 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

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

function( add_fctest )

if( NOT (CMAKE_VERSION VERSION_LESS 3.4 ) )
  set( _cmake_supports_checking_for_TEST TRUE )
  cmake_policy( SET CMP0064 NEW )
endif()

if( CMAKE_VERSION VERSION_LESS 3.1 )
  ecbuild_deprecate( "add_fctest is better supported with CMake > 3.1 (have ${CMAKE_VERSION})" )
endif()


if( NOT (CMAKE_VERSION VERSION_LESS 3.1) )

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

      list( APPEND _properties COMPILE_FLAGS COMPILE_DEFINITIONS )
      foreach( _prop ${_properties} )
          get_source_file_property( TESTSUITE_PROPERTY ${TESTSUITE} ${_prop} )
          if( TESTSUITE_PROPERTY )
              set_source_files_properties( ${TESTRUNNER} PROPERTIES ${_prop} ${TESTSUITE_PROPERTY} )
          endif()
      endforeach()

      add_custom_target( ${_PAR_TARGET}_testsuite SOURCES ${TESTSUITE} )

  endif()

else()

  set( options           )
  set( single_value_args TARGET )
  set( multi_value_args SOURCES LIBS LABELS )

  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  include_directories( ${FCKIT_INCLUDE_DIRS} )

  list( GET _PAR_SOURCES 0 TESTSUITE )
  list( REMOVE_ITEM _PAR_SOURCES ${TESTSUITE})

  list( APPEND _PAR_LIBS fckit )

  fctest_generate_runner( OUTPUT TESTRUNNER
                          FILENAME ${TESTSUITE}
                          DEPENDS ${_PAR_LIBS} )

  set( _PAR_LABELS fortran ${_PAR_LABELS} )
  ecbuild_add_test( TARGET ${_PAR_TARGET} ${_PAR_UNPARSED_ARGUMENTS}
                    SOURCES ${TESTRUNNER} ${_PAR_SOURCES}
                    LIBS ${_PAR_LIBS}
                    LABELS ${_PAR_LABELS} )

endif()

endfunction()
