/*
 * (C) Copyright 2013 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "fckit/Log.h"
#include <algorithm>
#include "eckit/exception/Exceptions.h"
#include "eckit/log/CallbackTarget.h"
#include "eckit/log/FileTarget.h"
#include "eckit/log/OStreamTarget.h"
#include "eckit/log/PrefixTarget.h"
#include "eckit/log/TimeStampTarget.h"
#include "eckit/runtime/Main.h"
#include "fckit/Libfckit.h"

using eckit::Channel;
using eckit::LogTarget;
using eckit::Main;
using eckit::PrefixTarget;
using eckit::system::Library;
using fckit::Log;

extern "C" {
void fckit_write_to_fortran_unit( int unit, const char* msg );
int fckit_fortranunit_stdout();
int fckit_fortranunit_stderr();
}

namespace {

static void write_to_fortran_unit( void* ctxt, const char* msg ) {
    fckit_write_to_fortran_unit( *static_cast<int*>( ctxt ), msg );
}

static std::string debug_prefix( const std::string& libname ) {
    std::string s = libname;
    std::transform( s.begin(), s.end(), s.begin(), ::toupper );
    s += "_DEBUG";
    return s;
}

void libs_debug_addTarget( LogTarget* target ) {
    for ( std::string libname : Library::list() ) {
        const Library& lib = Library::lookup( libname );
        if ( lib.debug() ) {
            lib.debugChannel().addTarget( new PrefixTarget( debug_prefix( libname ), target ) );
        }
    }
}

void libs_debug_setTarget( LogTarget* target ) {
    for ( std::string libname : Library::list() ) {
        const Library& lib = Library::lookup( libname );
        if ( lib.debug() ) {
            lib.debugChannel().setTarget( new PrefixTarget( debug_prefix( libname ), target ) );
        }
    }
}

}  // namespace

namespace fckit {

class FortranUnitTarget : public eckit::CallbackTarget {
public:
    FortranUnitTarget( int unit );

private:
    int unit_;
};

FortranUnitTarget::FortranUnitTarget( int unit ) :
    eckit::CallbackTarget( &write_to_fortran_unit, &unit_ ),
    unit_( unit ) {}

LogTarget* createStyleTarget( LogTarget* target, Log::Style style, const char* prefix ) {
    if ( style == Log::SIMPLE )
        return target;
    if ( style == Log::PREFIX )
        return new eckit::PrefixTarget( prefix, target );
    if ( style == Log::TIMESTAMP )
        return new eckit::TimeStampTarget( prefix, target );
    NOTIMP;
}

void Log::addFortranUnit( int unit, Style style, const char* ) {
    LogTarget* funit = new FortranUnitTarget( unit );
    info().addTarget( createStyleTarget( funit, style, "(I)" ) );
    warning().addTarget( createStyleTarget( funit, style, "(W)" ) );
    error().addTarget( createStyleTarget( funit, style, "(E)" ) );
    if ( Main::instance().debug() )
        debug().addTarget( createStyleTarget( funit, style, "(D)" ) );
    libs_debug_addTarget( funit );
}
void Log::setFortranUnit( int unit, Style style, const char* ) {
    LogTarget* funit = new FortranUnitTarget( unit );
    info().setTarget( createStyleTarget( funit, style, "(I)" ) );
    warning().setTarget( createStyleTarget( funit, style, "(W)" ) );
    error().setTarget( createStyleTarget( funit, style, "(E)" ) );
    if ( Main::instance().debug() )
        debug().setTarget( createStyleTarget( funit, style, "(D)" ) );
    libs_debug_setTarget( funit );
}

void Log::addFile( const char* path, Style style, const char* ) {
    LogTarget* file = new eckit::FileTarget( path );
    info().addTarget( createStyleTarget( file, style, "(I)" ) );
    warning().addTarget( createStyleTarget( file, style, "(W)" ) );
    error().addTarget( createStyleTarget( file, style, "(E)" ) );
    if ( Main::instance().debug() )
        debug().addTarget( createStyleTarget( file, style, "(D)" ) );
    libs_debug_addTarget( file );
}
void Log::setFile( const char* path, Style style, const char* ) {
    LogTarget* file = new eckit::FileTarget( path );
    info().setTarget( createStyleTarget( file, style, "(I)" ) );
    warning().setTarget( createStyleTarget( file, style, "(W)" ) );
    error().setTarget( createStyleTarget( file, style, "(E)" ) );
    if ( Main::instance().debug() )
        debug().setTarget( createStyleTarget( file, style, "(D)" ) );
    libs_debug_setTarget( file );
}

void Log::addFile( const std::string& path, Style style, const std::string& prefix ) {
    return addFile( path.c_str(), style, prefix.c_str() );
}

void Log::setFile( const std::string& path, Style style, const std::string& prefix ) {
    return setFile( path.c_str(), style, prefix.c_str() );
}

void Log::addStdOut( Style style, const char* ) {
    LogTarget* stdout = new eckit::OStreamTarget( std::cout );
    info().addTarget( createStyleTarget( stdout, style, "(I)" ) );
    warning().addTarget( createStyleTarget( stdout, style, "(W)" ) );
    error().addTarget( createStyleTarget( stdout, style, "(E)" ) );
    if ( Main::instance().debug() )
        debug().addTarget( createStyleTarget( stdout, style, "(D)" ) );
    libs_debug_addTarget( stdout );
}

void Log::setStdOut( Style style, const char* ) {
    LogTarget* stdout = new eckit::OStreamTarget( std::cout );
    info().setTarget( createStyleTarget( stdout, style, "(I)" ) );
    warning().setTarget( createStyleTarget( stdout, style, "(W)" ) );
    error().setTarget( createStyleTarget( stdout, style, "(E)" ) );
    if ( Main::instance().debug() )
        debug().setTarget( createStyleTarget( stdout, style, "(D)" ) );
    libs_debug_setTarget( stdout );
}

int Log::output_unit() {
    return fckit_fortranunit_stdout();
}
int Log::error_unit() {
    return fckit_fortranunit_stderr();
}

void Log::reset() {
    eckit::Log::reset();
    for ( std::string libname : Library::list() ) {
        if ( Channel& debug = Library::lookup( libname ).debugChannel() ) {
            debug.reset();
        }
    }
}

void Log::flush() {
    eckit::Log::flush();
    for ( std::string libname : Library::list() ) {
        if ( Channel& debug = Library::lookup( libname ).debugChannel() ) {
            debug.flush();
        }
    }
}

}  // namespace fckit
