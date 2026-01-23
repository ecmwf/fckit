# (C) Copyright 2013 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# This macro provides compiler introspection of the behaviour of finalisation of derived types

set( FINAL_SUPPORT_SOURCE ${CMAKE_CURRENT_LIST_DIR}/final-support.F90 )

macro( check_final_support )

    set( DEBUG_FINAL_SUPPORT FALSE  )
    macro( debug_test case )
      if( DEBUG_FINAL_SUPPORT )
        ecbuild_add_executable(
          TARGET  fckit-test-${case}
          SOURCES ${FINAL_SUPPORT_SOURCE}
          DEFINITIONS ${case}
        )
      endif()
    endmacro()

    macro( check_final_support_case case )
    
      if( NOT DEFINED FCKIT_${case} )
    
        try_compile( ${case}_compiled
                     ${CMAKE_CURRENT_BINARY_DIR}
                     ${FINAL_SUPPORT_SOURCE}
                     COMPILE_DEFINITIONS -D${case}
                     LINK_LIBRARIES "${CMAKE_EXE_LINKER_FLAGS}"
                     OUTPUT_VARIABLE FCKIT_${case}_compile_output
                     COPY_FILE ${CMAKE_CURRENT_BINARY_DIR}/${case}.bin )
    
        execute_process( COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${case}.bin
                         WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                         RESULT_VARIABLE _run_res
                         OUTPUT_VARIABLE FCKIT_${case} ERROR_VARIABLE _run_err )
    
    
        string( STRIP ${FCKIT_${case}} FCKIT_${case} )
        set( FCKIT_${case} ${FCKIT_${case}} CACHE STRING "" )
        debug_test( ${case} )
      endif()
    
    endmacro()

    list( APPEND cases
      FINAL_FUNCTION_RESULT
      FINAL_UNINITIALIZED_LOCAL
      FINAL_UNINITIALIZED_INTENT_OUT
      FINAL_UNINITIALIZED_INTENT_INOUT
      FINAL_NOT_PROPAGATING
      FINAL_NOT_INHERITING
      FINAL_BROKEN_FOR_ALLOCATABLE_ARRAY
      FINAL_BROKEN_FOR_AUTOMATIC_ARRAY
      FINAL_NOT_INHERITING_FOR_ALLOCATABLE_ARRAY
      FINAL_NOT_INHERITING_FOR_AUTOMATIC_ARRAY
    )
    foreach( case ${cases})
      check_final_support_case( ${case} )
    endforeach()
    
    ecbuild_add_executable(
      TARGET  fckit-final-support
      SOURCES ${FINAL_SUPPORT_SOURCE}
      NOINSTALL
    )

endmacro()
