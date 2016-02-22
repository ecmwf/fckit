if( HAVE_FCTEST )

ecbuild_add_library( TARGET example_lib
                     SOURCES example_lib.F90
                     NOINSTALL )


add_fctest( TARGET  example_simple 
            SOURCES example_simple.F90 
            LIBS    example_lib)

                  
add_fctest( TARGET  example_with_fixture 
            SOURCES example_with_fixture.F90 )

endif()
