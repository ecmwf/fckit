if( HAVE_FCTEST )

add_fctest( TARGET  fckit_test_array
            LINKER_LANGUAGE Fortran
            SOURCES test_array.F90 
            DEFINITIONS ${FCKIT_DEFINITIONS}
            LIBS    fckit)

add_fctest( TARGET  fckit_test_refcounting 
            LINKER_LANGUAGE Fortran
            SOURCES test_refcounting.F90 
            DEFINITIONS ${FCKIT_DEFINITIONS}
            CONDITION HAVE_ECKIT
            LIBS    fckit)

add_fctest( TARGET  fckit_test_refcounting_fortran
            LINKER_LANGUAGE Fortran
            SOURCES test_refcounting_fortran.F90 
            DEFINITIONS ${FCKIT_DEFINITIONS}
            LIBS    fckit)

endif()
