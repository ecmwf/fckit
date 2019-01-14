/*
 * (C) Copyright 2013 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once

#include "eckit/log/Log.h"

namespace fckit {

class Log : public eckit::Log {
public:
    enum Style
    {
        SIMPLE    = 0,
        PREFIX    = 1,
        TIMESTAMP = 2
    };

    static void addFortranUnit( int unit, Style = PREFIX, const char* prefix = "" );
    static void setFortranUnit( int unit, Style = PREFIX, const char* prefix = "" );

    static void addFile( const std::string& path, Style = PREFIX, const std::string& prefix = "" );
    static void setFile( const std::string& path, Style = PREFIX, const std::string& prefix = "" );

    static void addFile( const char* path, Style = PREFIX, const char* prefix = "" );
    static void setFile( const char* path, Style = PREFIX, const char* prefix = "" );

    static void addStdOut( Style = PREFIX, const char* prefix = "" );
    static void setStdOut( Style = PREFIX, const char* prefix = "" );

    static void reset();

    static void flush();

    // Fortran unit numbers
    static int output_unit();
    static int error_unit();
};

}  // namespace fckit
