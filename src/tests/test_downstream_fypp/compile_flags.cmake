# File used to change or override compile flags

set( DOWNSTREAM_Fortran_FLAGS                "-DDOWNSTREAM_Fortran_FLAGS='\"DOWNSTREAM_Fortran_FLAGS\"'" )
set( DOWNSTREAM_Fortran_FLAGS_RELWITHDEBINFO "-DDOWNSTREAM_Fortran_FLAGS_RELWITHDEBINFO='\"DOWNSTREAM_Fortran_FLAGS_RELWITHDEBINFO\"'" )

set_source_files_properties( downstream.fypp          PROPERTIES COMPILE_FLAGS          "-DDOWNSTREAM_COMPILE_FLAGS='\"DOWNSTREAM_COMPILE_FLAGS\"'" )
set_source_files_properties( downstream_override.fypp PROPERTIES OVERRIDE_COMPILE_FLAGS "-DDOWNSTREAM_COMPILE_FLAGS='\"DOWNSTREAM_OVERRIDE_COMPILE_FLAGS\"'" )
