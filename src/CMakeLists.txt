# (C) Copyright 2013-2017 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


set( FCKIT_EXTRA_LINK_LIBRARIES
  #/usr/local/apps/gcc/5.3.0/lib/gcc/x86_64-suse-linux/5.3.0/libstdc++.so
)

add_subdirectory(fckit)
add_subdirectory(fctest_examples)
add_subdirectory(tests)

if ( ENABLE_SANDBOX )
  add_subdirectory(sandbox)
endif()
