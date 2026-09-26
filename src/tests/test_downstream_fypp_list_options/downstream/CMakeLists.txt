cmake_minimum_required( VERSION 3.12 FATAL_ERROR )

find_package( ecbuild REQUIRED )

project( downstream_fypp_list_options VERSION 0.1.0 LANGUAGES Fortran )

find_package( fckit REQUIRED )

# Set COMPILE_OPTIONS to a multi-element CMake list:
set_source_files_properties( test_source.fypp
    PROPERTIES COMPILE_OPTIONS "-DFOO;-DBAR" )

unset( _output_sources )
fckit_preprocess_fypp_sources( _output_sources
    SOURCES test_source.fypp )
