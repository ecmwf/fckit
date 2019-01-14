/*
 * (C) Copyright 2013 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include <iostream>

#include "eckit/config/LocalConfiguration.h"

using namespace eckit;

// Fortran binding
extern "C" {
int c_get_a( const Configuration* );
}

int get_a( const Configuration& conf ) {
    return c_get_a( &conf );
}

int main( int argc, char** argv ) {
    LocalConfiguration conf;
    conf.set( "a", 10l );

    int a = get_a( conf );

    if ( a != 10 ) {
        std::cout << "a != 10" << std::endl;
        return 1;
    }
    return 0;
}
