/*
 * (C) Copyright 2013 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include <cstdint>
#include <cstring>

#include "eckit/log/Channel.h"
#include "fckit/Log.h"
using eckit::Channel;
using fckit::Log;

using int32 = std::int32_t;

extern "C" {

void fckit__log( Channel* channel, char* msg, int32 newl, int32 flush ) {
    if (::strlen( msg ) )
        *channel << msg;
    else
        *channel << " ";
    if ( newl ) *channel << eckit::newl;
    if ( flush ) *channel << std::flush;
}

void fckit__log_debug( char* msg, int32 newl, int32 flush ) {
    fckit__log( &Log::debug(), msg, newl, flush );
}

void fckit__log_info( char* msg, int32 newl, int32 flush ) {
    fckit__log( &Log::info(), msg, newl, flush );
}

void fckit__log_warning( char* msg, int32 newl, int32 flush ) {
    fckit__log( &Log::warning(), msg, newl, flush );
}

void fckit__log_error( char* msg, int32 newl, int32 flush ) {
    fckit__log( &Log::error(), msg, newl, flush );
}

void fckit__log_add_fortran_unit( int32 unit, int32 style ) {
    Log::addFortranUnit( unit, Log::Style( style ) );
}

void fckit__log_set_fortran_unit( int32 unit, int32 style ) {
    Log::setFortranUnit( unit, Log::Style( style ) );
}

void fckit__log_add_file( const char* path, int32 style ) {
    Log::addFile( path, Log::Style( style ) );
}

void fckit__log_set_file( const char* path, int32 style ) {
    Log::setFile( path, Log::Style( style ) );
}

void fckit__log_add_stdout( int32 style ) {
    Log::addStdOut( Log::Style( style ) );
}

void fckit__log_set_stdout( int32 style ) {
    Log::setStdOut( Log::Style( style ) );
}

void fckit__log_reset() {
    Log::reset();
}

void fckit__log_flush() {
    Log::flush();
}

Channel* fckit__log_info_channel() {
    return &Log::info();
}

Channel* fckit__log_warning_channel() {
    return &Log::warning();
}

Channel* fckit__log_error_channel() {
    return &Log::error();
}

Channel* fckit__log_debug_channel() {
    return &Log::debug();
}

}  // extern "C"
