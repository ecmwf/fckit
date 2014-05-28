if( HAVE_FCTEST )

add_fctest( TARGET  example_simple 
            SOURCES example_simple.F90 )

                  
add_fctest( TARGET  example_with_fixture 
            SOURCES example_with_fixture.F90 )

endif()
