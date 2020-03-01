cmake_minimum_required( VERSION 3.6 FATAL_ERROR )

find_package( ecbuild REQUIRED )

project( downstream VERSION 0.1.0 LANGUAGES Fortran)

find_package( fckit REQUIRED COMPONENTS ECKIT )

ecbuild_add_library( TARGET downstream SOURCES downstream.fypp downstream_override.fypp LIBS fckit )
fckit_target_preprocess_fypp( downstream )

ecbuild_add_executable( TARGET main SOURCES main.F90 LIBS fckit downstream )
