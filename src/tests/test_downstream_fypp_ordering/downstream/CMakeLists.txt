cmake_minimum_required( VERSION 3.12 FATAL_ERROR )

find_package( ecbuild REQUIRED )

project( downstream_ordering VERSION 0.1.0 LANGUAGES Fortran )

find_package( fckit REQUIRED )


# --- Library with ordering dependency (default behaviour) ---
#
# fckit_target_preprocess_fypp should create a 'libwithorder_fypp_order'
# custom target and add it as a build dependency of 'libwithorder'.

ecbuild_add_library(
    TARGET libwithorder
    SOURCES module_a.fypp )

fckit_target_preprocess_fypp( libwithorder )

# Verify ordering target was created.
if( NOT TARGET libwithorder_fypp_order )
  message( FATAL_ERROR
    "fckit_target_preprocess_fypp did not create the expected "
    "'libwithorder_fypp_order' ordering target." )
endif()
get_target_property( _libwithorder_deps libwithorder MANUALLY_ADDED_DEPENDENCIES )
if( NOT "libwithorder_fypp_order" IN_LIST _libwithorder_deps )
  message( FATAL_ERROR
    "fckit_target_preprocess_fypp did not add 'libwithorder_fypp_order' as a "
    "dependency of the 'libwithorder' target." )
endif()


# --- Library with NO_TARGET_DEPENDS (ordering dependency suppressed) ---
#
# When NO_TARGET_DEPENDS is passed, fckit_target_preprocess_fypp must not
# create an ordering target.

ecbuild_add_library(
    TARGET libnodep
    SOURCES module_b.fypp )

fckit_target_preprocess_fypp( libnodep NO_TARGET_DEPENDS )

# Verify ordering target was NOT created.
if( TARGET libnodep_fypp_order )
  message( FATAL_ERROR
    "fckit_target_preprocess_fypp created 'libnodep_fypp_order' despite "
    "NO_TARGET_DEPENDS being set." )
endif()


ecbuild_add_executable( TARGET main SOURCES main.F90 LIBS libwithorder libnodep )
