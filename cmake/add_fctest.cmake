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
  set(outfile ${CMAKE_CURRENT_BINARY_DIR}/${base}_main.F90)
  set(${_PAR_OUTPUT} ${outfile} PARENT_SCOPE)

  list( APPEND _depends
    ${_PAR_FILENAME}
    ${_PAR_DEPENDS}
  )

  add_custom_command(
    OUTPUT ${outfile}
    COMMAND ${FCTEST_GENERATOR} -i ${_PAR_FILENAME} -o ${outfile}
    DEPENDS ${_depends}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} )

  set_source_files_properties(${outfile} PROPERTIES GENERATED TRUE)

endfunction()

function( add_fctest )

cmake_policy( SET CMP0064 NEW ) # Recognize ``TEST`` as operator for the ``if()`` command. (introduced in CMake version 3.4)

  ecbuild_add_test( ${ARGV} )

  set( options           )
  set( single_value_args TARGET )
  set( multi_value_args )

  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}" ${_FIRST_ARG} ${ARGN} )

  if( TARGET ${_PAR_TARGET} )
      get_target_property( test_sources ${_PAR_TARGET} SOURCES )
      list( GET test_sources 0 TESTSUITE )

      get_filename_component( extension ${TESTSUITE} EXT )
      get_filename_component( base ${TESTSUITE} NAME_WE )

    ### Preprocess files with extension ".fypp.F90"      
      fckit_target_preprocess_fypp( ${_PAR_TARGET} )

    ### Remove TESTSUITE from target
      get_target_property( test_sources ${_PAR_TARGET} SOURCES )
      set( match_regex "${base}.F90")
      set( match_found FALSE )
      foreach( source ${test_sources} )
        if( ${source} MATCHES "${match_regex}" )
          if( match_found ) 
            message( FATAL_ERROR "Second match found for ${match_regex} in fctest ${_PAR_TARGET}" )
          endif()
          set( match_found TRUE )
          set( TESTSUITE ${source} )
          list( FILTER test_sources EXCLUDE REGEX ${source} )
        endif()
      endforeach()
      if( NOT match_found )
        message( FATAL_ERROR "No match found for ${match_regex} in fctest ${_PAR_TARGET}" )
      endif()
      set_property( TARGET ${_PAR_TARGET} PROPERTY SOURCES ${test_sources} )

    ### Add TESTRUNNER generated from TESTSUITE
      fctest_generate_runner(
          OUTPUT TESTRUNNER
          FILENAME ${TESTSUITE} )
      target_sources( ${_PAR_TARGET} PUBLIC ${TESTRUNNER} )

    ### Add dependencies
      target_include_directories( ${_PAR_TARGET} PUBLIC ${FCKIT_INCLUDE_DIRS} )
      target_link_libraries( ${_PAR_TARGET} fckit )
      if( TEST ${_PAR_TARGET} )
        set_property( TEST ${_PAR_TARGET} APPEND PROPERTY LABELS "fortran" )
      endif()

    ### Add compile flags
      list( APPEND _properties COMPILE_FLAGS COMPILE_DEFINITIONS )
      foreach( _prop ${_properties} )
          if( NOT ORIGINAL_TESTSUITE )
            set( ORIGINAL_TESTSUITE ${TESTSUITE} )
          endif()
          get_source_file_property( TESTSUITE_PROPERTY ${ORIGINAL_TESTSUITE} ${_prop} )
          if( TESTSUITE_PROPERTY )
              set_source_files_properties( ${TESTRUNNER} PROPERTIES ${_prop} ${TESTSUITE_PROPERTY} )
          endif()
      endforeach()

      add_custom_target( ${_PAR_TARGET}_testsuite SOURCES ${TESTSUITE} )
  endif()

endfunction()
