if( HAVE_FCTEST )

ecbuild_add_library( TARGET fctest_example_lib
                     SOURCES fctest_example_lib.F90
                     NOINSTALL )


add_fctest( TARGET fctest_example_simple
            LINKER_LANGUAGE Fortran
            SOURCES fctest_example_simple.F90
            LIBS    fctest_example_lib)


add_fctest( TARGET fctest_example_with_fixture
            LINKER_LANGUAGE Fortran
            SOURCES fctest_example_with_fixture.F90 )

endif()
