/*
 * (C) Copyright 2013 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include <csignal>
#include <cstdint>
#include <cstring>
#include <exception>
#include <stdexcept>

#include "eckit/config/LibEcKit.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/CodeLocation.h"

#include "fckit/Log.h"
#include "fckit/Main.h"

using int32  = std::int32_t;
using size_t = std::size_t;

static int32 SUCCESS = 0;
// static int ERROR   = -1;

void fckit_abort( const std::string& what, const eckit::CodeLocation& loc ) {
    throw eckit::Abort( what, loc );
}

void fckit_throw( const std::string& what, const eckit::CodeLocation& loc ) {
    if ( loc )
        throw eckit::Exception( what, loc );
    else
        throw eckit::Exception( what );
}

extern "C" {

int32 fckit__main_init( int32 argc, char* argv[] ) {
    if ( not fckit::Main::ready() ) { fckit::Main::initialise( argc, argv ); }

    return SUCCESS;
}

void fckit__main_finalise() {
    fckit::Main::finalise();
}

int32 fckit__main_ready( int32& ready ) {
    ready = fckit::Main::ready();
    return SUCCESS;
}

int32 fckit__main_taskID( int32& taskID ) {
    taskID = fckit::Main::instance().taskID();
    return SUCCESS;
}

int32 fckit__main_setTaskID( int32 taskID ) {
    fckit::Main::instance().taskID( taskID );
    return SUCCESS;
}

int32 fckit__main_debug() {
    return fckit::Log::debug() != 0;
}

int32 fckit__main_name( char*& name, size_t& size ) {
    std::string v = fckit::Main::instance().name();
    size          = v.size();
    name          = new char[size + 1];
    ::strcpy( name, v.c_str() );
    return SUCCESS;
}

int32 fckit__main_displayname( char*& name, size_t& size ) {
    std::string v = dynamic_cast<const fckit::Main&>( fckit::Main::instance() ).displayName();
    size          = v.size();
    name          = new char[size + 1];
    std::strcpy( name, v.c_str() );
    return SUCCESS;
}

void fckit__set_abort_handler( eckit::abort_handler_t h ) {
    eckit::LibEcKit::instance().setAbortHandler( h );
}

void fckit__abort( const char* what, const char* file, int32 line, const char* function ) {
    fckit_abort( what, eckit::CodeLocation( file, line, function ) );
}

void fckit__exception_throw( const char* what, const char* file, int32 line, const char* function ) {
    fckit_throw( what, eckit::CodeLocation( file, line, function ) );
}

void fckit__set_signal_handler( int32 signum, fckit::signal_handler_t signal_handler ) {
    fckit::Signals::instance().setSignalHandler( fckit::Signal( signum, signal_handler ) );
}

void fckit__set_fckit_signal_handler( int32 signum ) {
    fckit::Signals::instance().setSignalHandler( fckit::Signal( signum ) );
}

void fckit__set_fckit_signal_handlers() {
    fckit::Signals::instance().setSignalHandlers();
}

void fckit__raise_signal( int32 signum ) {
    std::raise( signum );
}

void fckit__restore_signal_handler( int32 signum ) {
    fckit::Signals::instance().restoreSignalHandler( signum );
}

void fckit__restore_all_signal_handlers() {
    fckit::Signals::instance().restoreAllSignalHandlers();
}

int32 fckit__SIGABRT() {
    return SIGABRT;
}
int32 fckit__SIGKILL() {
    return SIGKILL;
}
int32 fckit__SIGINT() {
    return SIGINT;
}
int32 fckit__SIGALRM() {
    return SIGALRM;
}
int32 fckit__SIGFPE() {
    return SIGFPE;
}
int32 fckit__SIGTERM() {
    return SIGTERM;
}
int32 fckit__SIGSEGV() {
    return SIGSEGV;
}
int32 fckit__SIGILL() {
    return SIGILL;
}

}  // extern "C"
