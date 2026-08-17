cmake_minimum_required( VERSION 3.12 FATAL_ERROR )

find_package( ecbuild REQUIRED )

project( downstream_output_sources VERSION 0.1.0 LANGUAGES Fortran )

find_package( fckit REQUIRED )


ecbuild_add_library(
    TARGET downstream
    SOURCES module_a.fypp )

fckit_target_preprocess_fypp( downstream OUTPUT_SOURCES _downstream_generated )

# Verify OUTPUT_SOURCES: returned list must be non-empty and every entry must be
# a generated .F90 path.
if( NOT _downstream_generated )
  message( FATAL_ERROR
    "fckit_target_preprocess_fypp OUTPUT_SOURCES returned an empty list. "
    "Expected a generated .F90 file for module_a.fypp." )
endif()
foreach( _gen ${_downstream_generated} )
  if( NOT _gen MATCHES "\\.F90$" )
    message( FATAL_ERROR
      "fckit_target_preprocess_fypp OUTPUT_SOURCES entry '${_gen}' does not end in .F90" )
  endif()
endforeach()

ecbuild_add_executable( TARGET main SOURCES main.F90 LIBS downstream )
