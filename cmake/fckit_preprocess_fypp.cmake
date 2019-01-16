function( fckit_preprocess_fypp output )

  set( options NO_LINE_NUMBERING )
  set( single_value_args "" )
  set( multi_value_args SOURCES FYPP_ARGS )
  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  foreach( filename ${_PAR_SOURCES} )

    set(outfile ${CMAKE_CURRENT_BINARY_DIR}/${filename})
    
    # Append to output and set in parent scope
    set(${output} ${${output}} ${outfile} PARENT_SCOPE)

    list( APPEND args -l 132 )
    if( (NOT _PAR_NO_LINE_NUMBERING) AND (NOT FYPP_NO_LINE_NUMBERING) )
      list( APPEND args -n )
    endif()

    add_custom_command(
      OUTPUT ${outfile}
      COMMAND ${FYPP} ARGS ${args} ${CMAKE_CURRENT_SOURCE_DIR}/${filename} ${outfile}
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${filename} )
    set_source_files_properties(${outfile} PROPERTIES GENERATED TRUE)

  endforeach()

endfunction()
