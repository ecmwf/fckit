if( HAVE_FCTEST )

add_fctest( TARGET  fckit_test_array
            LINKER_LANGUAGE Fortran
            SOURCES test_array.F90
            DEFINITIONS ${FCKIT_DEFINITIONS}
            LIBS    fckit)

add_fctest( TARGET  fckit_test_resource
            LINKER_LANGUAGE Fortran
            SOURCES test_resource.F90
            ARGS    -integer 10 -long 5000000000 -float 0.123456 -double 0.1234567890123456789 -string "hello world"
            DEFINITIONS ${FCKIT_DEFINITIONS}
            CONDITION HAVE_ECKIT
            LIBS    fckit)

add_fctest( TARGET  fckit_test_log
            LINKER_LANGUAGE Fortran
            SOURCES test_log.F90
            DEFINITIONS ${FCKIT_DEFINITIONS}
            CONDITION HAVE_ECKIT
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
            CONDITION HAVE_ECKIT
            LIBS    fckit)

add_fctest( TARGET  fckit_test_mpi
            LINKER_LANGUAGE Fortran
            SOURCES test_mpi.F90
            MPI 4
            DEFINITIONS ${FCKIT_DEFINITIONS}
            CONDITION HAVE_ECKIT
            LIBS    fckit)

endif()
