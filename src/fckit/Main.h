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

#include <cstdint>
#include <iosfwd>
#include <map>
#include <string>
#include "eckit/runtime/Main.h"

namespace fckit {

class Main : public eckit::Main {
public:
    Main( int argc, char** argv, const char* homeenv = 0 );

    static void initialise( int argc, char** argv, const char* homeenv = 0 );
    static void initialize( int argc, char** argv, const char* homeenv = 0 ) { initialise( argc, argv, homeenv ); }

    static void finalise();
    static void finalize() { finalise(); }
};

// ------------------------------------------------------------------------------------

typedef void ( *signal_handler_t )( std::int32_t );

class Signal {
public:
    Signal();

    Signal( int signum );

    Signal( int signum, signal_handler_t signal_handler );

    operator int() const { return signum_; }
    std::string str() const { return str_; }
    const signal_handler_t& handler() const { return signal_handler_; }

private:
    friend std::ostream& operator<<( std::ostream&, const Signal& );

    int signum_;
    std::string str_;
    signal_handler_t signal_handler_;
};

// ------------------------------------------------------------------------------------

class Signals {
private:
    Signals() {}

public:
    static Signals& instance();
    void setSignalHandlers();
    void setSignalHandler( const Signal& );
    void restoreSignalHandler( int signum );
    void restoreAllSignalHandlers();
    const Signal& signal( int signum ) const;

private:
    typedef std::map<int, Signal> registered_signals_t;
    registered_signals_t registered_signals_;
};

// ------------------------------------------------------------------------------------

}  // namespace fckit
