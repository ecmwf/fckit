# (C) Copyright 2013 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

set(FCKIT_HAVE_ECKIT_TENSOR 0)
if( HAVE_ECKIT )
  if(eckit_VERSION VERSION_GREATER_EQUAL 1.23.0)
    set(FCKIT_HAVE_ECKIT_TENSOR 1)
  endif()
endif()

add_subdirectory(fckit)
add_subdirectory(apps)
add_subdirectory(tests)

if ( HAVE_SANDBOX )
  add_subdirectory(sandbox)
endif()
