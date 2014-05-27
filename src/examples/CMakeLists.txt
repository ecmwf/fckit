if( HAVE_FCTEST )

add_fctest( TARGET  example_simple 
            LIBS    fctest
            SOURCES example_simple.F90 )

                  
add_fctest( TARGET  example_with_fixture 
            LIBS    fctest
            SOURCES example_with_fixture.F90 )

endif()
