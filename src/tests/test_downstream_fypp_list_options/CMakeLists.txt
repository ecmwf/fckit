
# This test configures a downstream package that calls fckit_preprocess_fypp_sources
# with a list-valued COMPILE_OPTIONS property set on an fypp source file.
#
# Test created in response to a bug where source file properties were being
# expanded unquoted, resulting in a CMake configuration error.

if( HAVE_TESTS )

  configure_file( test-downstream.sh.in ${CMAKE_CURRENT_BINARY_DIR}/test-downstream.sh @ONLY )

  unset( _test_args )
  if( CMAKE_TOOLCHAIN_FILE )
    if( NOT IS_ABSOLUTE ${CMAKE_TOOLCHAIN_FILE})
      set( CMAKE_TOOLCHAIN_FILE "${CMAKE_BINARY_DIR}/${CMAKE_TOOLCHAIN_FILE}" )
    endif()
    list( APPEND _test_args "-DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}" )
  endif()
  foreach( lang C CXX Fortran )
    if( CMAKE_${lang}_COMPILER )
      list( APPEND _test_args "-DCMAKE_${lang}_COMPILER=${CMAKE_${lang}_COMPILER}" )
    endif()
  endforeach()

  add_test( NAME fckit_test_downstream_fypp_list_options
            COMMAND ${CMAKE_CURRENT_BINARY_DIR}/test-downstream.sh ${_test_args} )

endif()
