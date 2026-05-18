cmake_minimum_required( VERSION 3.12 FATAL_ERROR )

find_package( ecbuild REQUIRED )

project( downstream VERSION 0.1.0 LANGUAGES Fortran)

find_package( fckit REQUIRED COMPONENTS ECKIT )


ecbuild_add_library(
    TARGET downstream
    SOURCES downstream.fypp downstream_override.fypp
    PUBLIC_LIBS fckit
    DEFINITIONS _POSIX_C_SOURCE=200809L exclude_from_fypp)

set( _fypp_args FYPP_ARGS_EXCLUDE "exclude_from_fypp" )
if( DEFINED DOWNSTREAM_FYPP_OUTPUT_SUFFIX )
  list( APPEND _fypp_args OUTPUT_SUFFIX "${DOWNSTREAM_FYPP_OUTPUT_SUFFIX}" )
endif()

fckit_target_preprocess_fypp( downstream ${_fypp_args} )

ecbuild_add_executable( TARGET main SOURCES main.F90 LIBS fckit downstream )
