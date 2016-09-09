if( HAVE_FCTEST )

add_fctest( TARGET  fckit_test_array 
            SOURCES test_array.F90 
            LIBS    fckit)

endif()
