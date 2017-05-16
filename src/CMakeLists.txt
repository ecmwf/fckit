add_subdirectory(fckit)
add_subdirectory(fctest_examples)
add_subdirectory(tests)

if ( ENABLE_SANDBOX )
  add_subdirectory(sandbox)
endif()
