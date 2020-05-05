
# This test builds a package that requires fypp processing
# It uses the overriding of compile flags, like IFS is using.
#
# Test created to avoid regression after fixing issue FCKIT-19,
# where compile flags were not propagated to fypp-generated files.

if( HAVE_TESTS AND HAVE_ECKIT )

  configure_file( test-downstream.sh.in ${CMAKE_CURRENT_BINARY_DIR}/test-downstream.sh @ONLY )

  unset( _test_args )
  if( CMAKE_TOOLCHAIN_FILE )
    list( APPEND _test_args "-DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}" )
  endif()
  foreach( lang C CXX Fortran )
    if( CMAKE_${lang}_COMPILER )
      list( APPEND _test_args "-DCMAKE_${lang}_COMPILER=${CMAKE_${lang}_COMPILER}" )
    endif()
  endforeach()

  add_test( NAME fckit_test_downstream_fypp
            COMMAND ${CMAKE_CURRENT_BINARY_DIR}/test-downstream.sh ${_test_args} )

endif()
