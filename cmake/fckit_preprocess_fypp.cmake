function( fckit_preprocess_fypp output )

  set( options NO_LINE_NUMBERING )
  set( single_value_args "" )
  set( multi_value_args SOURCES FYPP_ARGS TARGET_INCLUDES )
  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  foreach( filename ${_PAR_SOURCES} )

    set(outfile ${CMAKE_CURRENT_BINARY_DIR}/${filename})
    
    # Append to output and set in parent scope
    set(${output} ${${output}} ${outfile} PARENT_SCOPE)

    list( APPEND args -l 132 ) # Line length
    list( APPEND args -p )     # Create parent folder
    if( (NOT _PAR_NO_LINE_NUMBERING) AND (NOT FYPP_NO_LINE_NUMBERING) )
      list( APPEND args -n )   # Create line numbering for compile errors
      if( CMAKE_Fortran_COMPILER_ID MATCHES "Cray" )
        list( APPEND args -N nocontlines )  # workaround for line numbers in continuation lines
      endif()
    endif()

    if( _PAR_FYPP_ARGS )
        set( args ${args} ${_PAR_FYPP_ARGS} )
    endif()

    if( _PAR_TARGET_INCLUDES )
      foreach( target ${_PAR_TARGET_INCLUDES} )
        get_target_property( _target_include_dirs ${target} INTERFACE_INCLUDE_DIRECTORIES )
        if( _target_include_dirs )
          foreach( _target_include_dir ${_target_include_dirs} )
            list( APPEND args -I ${_target_include_dir} } )
          endforeach()
        endif()
        get_target_property( _target_include_dirs ${target} INCLUDE_DIRECTORIES )
        if( _target_include_dirs )
          foreach( _target_include_dir ${_target_include_dirs} )
            list( APPEND args -I ${_target_include_dir} )
          endforeach()
        endif()
      endforeach()
    endif()

    add_custom_command(
      OUTPUT ${outfile}
      COMMAND ${FYPP} ARGS ${args} ${CMAKE_CURRENT_SOURCE_DIR}/${filename} ${outfile}
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${filename} )
    set_source_files_properties(${outfile} PROPERTIES GENERATED TRUE)

  endforeach()

endfunction()
