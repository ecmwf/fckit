#
# This test builds a package that requires fypp processing
# It verifies the target-ordering behaviour and NO_TARGET_DEPENDS option.
#
# Test created alongside addition of the target ordering functionality.

if( HAVE_TESTS )

  configure_file( test-downstream.sh.in ${CMAKE_CURRENT_BINARY_DIR}/test-downstream.sh @ONLY )

  unset( _test_args )
  if( CMAKE_TOOLCHAIN_FILE )
    if( NOT IS_ABSOLUTE ${CMAKE_TOOLCHAIN_FILE} )
      set( CMAKE_TOOLCHAIN_FILE "${CMAKE_BINARY_DIR}/${CMAKE_TOOLCHAIN_FILE}" )
    endif()
    list( APPEND _test_args "-DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}" )
  endif()
  foreach( lang C CXX Fortran )
    if( CMAKE_${lang}_COMPILER )
      list( APPEND _test_args "-DCMAKE_${lang}_COMPILER=${CMAKE_${lang}_COMPILER}" )
    endif()
    if( CMAKE_${lang}_FLAGS )
      string(REPLACE "--coverage" "" CMAKE_${lang}_FLAGS "${CMAKE_${lang}_FLAGS}")
      list( APPEND _test_args "-DECBUILD_${lang}_FLAGS=${CMAKE_${lang}_FLAGS}" )
    endif()
  endforeach()

  add_test( NAME fckit_test_downstream_fypp_ordering
            COMMAND ${CMAKE_CURRENT_BINARY_DIR}/test-downstream.sh ${_test_args} )

endif()
