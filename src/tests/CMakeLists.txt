# (C) Copyright 2013-2017 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

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
            CONDITION HAVE_ECKIT
            LIBS    fckit)

add_fctest( TARGET  fckit_test_refcounting_fortran
            LINKER_LANGUAGE Fortran
            SOURCES test_refcounting_fortran.F90
            CONDITION HAVE_ECKIT
            LIBS    fckit)

add_fctest( TARGET  fckit_test_shared_ptr
            LINKER_LANGUAGE Fortran
            SOURCES 
              test_shared_ptr.F90
              test_shared_ptr.cc
            LIBS    fckit)

add_fctest( TARGET  fckit_test_mpi
            LINKER_LANGUAGE Fortran
            SOURCES test_mpi.F90
            MPI 4
            DEFINITIONS ${FCKIT_DEFINITIONS}
            CONDITION HAVE_ECKIT AND ECKIT_HAVE_MPI
            LIBS    fckit)


add_fctest( TARGET  fckit_test_abort
            LINKER_LANGUAGE Fortran
            SOURCES test_abort.F90
            CONDITION HAVE_ECKIT
            LIBS    fckit)

add_fctest( TARGET  fckit_test_configuration
            LINKER_LANGUAGE Fortran
            DEFINITIONS ${FCKIT_DEFINITIONS}
            SOURCES test_configuration.F90
            DEFINITIONS ${FCKIT_DEFINITIONS}
            CONDITION HAVE_ECKIT
            LIBS    fckit)

add_fctest( TARGET  fckit_test_broadcast_file
            LINKER_LANGUAGE Fortran
            SOURCES test_broadcast_file.F90
            MPI 4
            DEFINITIONS ${FCKIT_DEFINITIONS}
            CONDITION HAVE_ECKIT AND ECKIT_HAVE_MPI
            LIBS    fckit)

endif()

ecbuild_add_test( TARGET  fckit_test_configuration_cpp
            LINKER_LANGUAGE CXX
            SOURCES
            test_configuration.cc
            test_configuration_fortcode.F90
            DEFINITIONS ${FCKIT_DEFINITIONS}
            INCLUDES ${ECKIT_INCLUDE_DIRS}
            CONDITION HAVE_ECKIT
            LIBS    fckit ${FCKIT_EXTRA_LINK_LIBRARIES})

ecbuild_add_test( TARGET fckit_test_cpp
  LINKER_LANGUAGE CXX
  SOURCES test_cpp.cc test_cpp_fortcode.F90
  INCLUDES ${ECKIT_INCLUDE_DIRS}
  ENVIRONMENT "DEBUG=1"
  CONDITION HAVE_ECKIT
  LIBS fckit )
