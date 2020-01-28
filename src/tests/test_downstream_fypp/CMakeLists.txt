
# This test builds a package that requires fypp processing
# It uses the overriding of compile flags, like IFS is using.
#
# Test created to avoid regression after fixing issue FCKIT-19,
# where compile flags were not propagated to fypp-generated files.

if( HAVE_TESTS )
  if( DEFINED ecbuild_VERSION AND NOT ecbuild_VERSION VERSION_LESS 3.0 )

    configure_file( test-downstream.sh.in ${CMAKE_CURRENT_BINARY_DIR}/test-downstream.sh @ONLY )

    unset( _test_args )
    if( CMAKE_TOOLCHAIN_FILE )
      list( APPEND _test_args "-DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}" )
    endif()

    add_test( NAME fckit_test_downstream_fypp
              COMMAND ${CMAKE_CURRENT_BINARY_DIR}/test-downstream.sh ${_test_args} )

  endif()
endif()
