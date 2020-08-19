/*
 * (C) Copyright 2013 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include <cstdarg>
#include <cstdint>
#include <cstring>
#include <iostream>
#include <sstream>
#include <stdexcept>

#include "eckit/config/Configuration.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"

#include "eckit/log/JSON.h"

using eckit::CodeLocation;
using eckit::Configuration;
using eckit::Exception;
using eckit::JSON;
using eckit::LocalConfiguration;
using eckit::PathName;
using eckit::YAMLConfiguration;
using std::string;
using std::stringstream;
using std::vector;

using int32  = std::int32_t;
using int64  = std::int64_t;
using size_t = std::size_t;

namespace fckit {

class NotLocalConfiguration : public Exception {
public:
    NotLocalConfiguration( const CodeLocation& location ) :
        Exception( "Configuration must be of concrete LocalConfiguration type", location ) {}
};

class ConfigurationNotFound : public Exception {
public:
    ConfigurationNotFound( const std::string& name ) :
        Exception( "Could not find \"" + name + "\" in Configuration" ) {}
};

extern "C" {

void c_fckit_throw_configuration_not_found( const char* name ) {
    throw ConfigurationNotFound( name );
}

Configuration* c_fckit_configuration_new() {
    return new LocalConfiguration();
}

Configuration* c_fckit_configuration_new_from_yaml( const char* yaml ) {
    stringstream s;
    s << yaml;
    return new YAMLConfiguration( s );
}

const Configuration* c_fckit_configuration_new_from_file( const char* path ) {
    PathName p( path );
    return new YAMLConfiguration( p );
}

const Configuration* c_fckit_configuration_new_from_buffer( eckit::CountedBuffer* buffer ) {
    eckit::SharedBuffer sb( buffer );
    return new YAMLConfiguration( eckit::SharedBuffer( buffer ) );
}

void c_fckit_configuration_delete( Configuration* This ) {
    ASSERT( This != nullptr );
    delete This;
}

void c_fckit_configuration_set_config( Configuration* This, const char* name, const Configuration* value ) {
    ASSERT( This != nullptr );
    if ( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>( This ) )
        local->set( string( name ), LocalConfiguration( *value ) );
    else
        throw NotLocalConfiguration( Here() );
}

void c_fckit_configuration_set_config_list( Configuration* This, const char* name, const Configuration* value[],
                                            size_t size ) {
    ASSERT( This != nullptr );
    vector<LocalConfiguration> params( size );
    for ( size_t i = 0; i < size; ++i )
        params[i] = LocalConfiguration( *value[i] );
    if ( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>( This ) )
        local->set( string( name ), params );
    else
        throw NotLocalConfiguration( Here() );
}

void c_fckit_configuration_set_bool( Configuration* This, const char* name, int32 value ) {
    ASSERT( This != nullptr );
    ASSERT( value == 0 || value == 1 );
    if ( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>( This ) )
        local->set( string( name ), bool( value ) );
    else
        throw NotLocalConfiguration( Here() );
}

void c_fckit_configuration_set_int32( Configuration* This, const char* name, int32 value ) {
    ASSERT( This != nullptr );
    if ( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>( This ) )
        local->set( string( name ), value );
    else
        throw NotLocalConfiguration( Here() );
}

void c_fckit_configuration_set_int64( Configuration* This, const char* name, int64 value ) {
    ASSERT( This != nullptr );
    if ( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>( This ) )
        // TODO: long should be converted to int64 once ECKIT-349 is fixed
        local->set( string( name ), long( value ) );
    else
        throw NotLocalConfiguration( Here() );
}

void c_fckit_configuration_set_float( Configuration* This, const char* name, float value ) {
    ASSERT( This != nullptr );
    if ( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>( This ) )
        local->set( string( name ), value );
    else
        throw NotLocalConfiguration( Here() );
}

void c_fckit_configuration_set_double( Configuration* This, const char* name, double value ) {
    ASSERT( This != nullptr );
    if ( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>( This ) )
        local->set( string( name ), value );
    else
        throw NotLocalConfiguration( Here() );
}

void c_fckit_configuration_set_string( Configuration* This, const char* name, const char* value ) {
    ASSERT( This != nullptr );
    if ( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>( This ) )
        local->set( string( name ), string( value ) );
    else
        throw NotLocalConfiguration( Here() );
}

void c_fckit_configuration_set_array_string( Configuration* This, const char* name, const char* value, size_t length,
                                             size_t size ) {
    ASSERT( This != nullptr );
    vector<string> v;
    for ( size_t jj = 0; jj < size; ++jj ) {
        char str[length + 1];
        ASSERT( snprintf( str, sizeof( str ), "%s", value + jj * length ) >= 0 );
        v.push_back( string( str ) );
    }
    if ( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>( This ) )
        local->set( string( name ), v );
    else
        throw NotLocalConfiguration( Here() );
}

void c_fckit_configuration_set_array_int32( Configuration* This, const char* name, int32 value[], size_t size ) {
    ASSERT( This != nullptr );
    vector<int32> v;
    v.assign( value, value + size );
    if ( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>( This ) )
        local->set( string( name ), v );
    else
        throw NotLocalConfiguration( Here() );
}

void c_fckit_configuration_set_array_int64( Configuration* This, const char* name, int64 value[], size_t size ) {
    ASSERT( This != nullptr );
    // TODO: long should be converted to int64 once ECKIT-349 is fixed
    vector<long> v;
    v.assign( value, value + size );
    if ( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>( This ) )
        local->set( string( name ), v );
    else
        throw NotLocalConfiguration( Here() );
}

void c_fckit_configuration_set_array_float( Configuration* This, const char* name, float value[], size_t size ) {
    vector<float> v;
    v.assign( value, value + size );
    if ( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>( This ) )
        local->set( string( name ), v );
    else
        throw NotLocalConfiguration( Here() );
}
void c_fckit_configuration_set_array_double( Configuration* This, const char* name, double value[], size_t size ) {
    vector<double> v;
    v.assign( value, value + size );
    if ( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>( This ) )
        local->set( string( name ), v );
    else
        throw NotLocalConfiguration( Here() );
}

int32 c_fckit_configuration_get_config( const Configuration* This, const char* name, LocalConfiguration* value ) {
    if ( !This->get( string( name ), *value ) )
        return false;
    return true;
}

int32 c_fckit_configuration_get_config_list( const Configuration* This, const char* name, LocalConfiguration**& value,
                                             size_t& size ) {
    value = nullptr;
    vector<LocalConfiguration> vector;
    if ( !This->get( string( name ), vector ) )
        return false;
    size  = vector.size();
    value = new LocalConfiguration*[size];
    for ( size_t i = 0; i < size; ++i ) {
        value[i] = new LocalConfiguration( vector[i] );
    }
    return true;
}

int32 c_fckit_configuration_get_bool( const Configuration* This, const char* name, int32& value ) {
    bool _value;
    if ( !This->get( string( name ), _value ) ) {
        return false;  // _value unassigned!
    }
    value = _value;
    return true;
}

int32 c_fckit_configuration_get_int32( const Configuration* This, const char* name, int32& value ) {
    if ( !This->get( string( name ), value ) )
        return false;
    return true;
}

int32 c_fckit_configuration_get_int64( const Configuration* This, const char* name, int64& value ) {
    // TODO: long should be converted to int64 once ECKIT-349 is fixed
    long v;
    if ( !This->get( string( name ), v ) )
        return false;
    value = v;
    return true;
}

int32 c_fckit_configuration_get_float( const Configuration* This, const char* name, float& value ) {
    if ( !This->get( string( name ), value ) )
        return false;
    return true;
}

int32 c_fckit_configuration_get_double( const Configuration* This, const char* name, double& value ) {
    if ( !This->get( string( name ), value ) )
        return false;
    return true;
}

int32 c_fckit_configuration_get_string( const Configuration* This, const char* name, char*& value, size_t& size ) {
    string s;
    if ( !This->get( string( name ), s ) ) {
        value = nullptr;
        return false;
    }
    size  = s.size() + 1;
    value = new char[size];
    strcpy( value, s.c_str() );
    return true;
}

int32 c_fckit_configuration_get_array_int32( const Configuration* This, const char* name, int32*& value,
                                             size_t& size ) {
    vector<int32> v;
    if ( !This->get( string( name ), v ) )
        return false;
    size  = v.size();
    value = new int32[size];
    for ( size_t j = 0; j < v.size(); ++j )
        value[j] = v[j];
    return true;
}

int32 c_fckit_configuration_get_array_int64( const Configuration* This, const char* name, int64*& value,
                                             size_t& size ) {
    // TODO: long should be converted to int64 once ECKIT-349 is fixed
    vector<long> v;
    if ( !This->get( string( name ), v ) )
        return false;
    size  = v.size();
    value = new int64[size];
    for ( size_t j = 0; j < v.size(); ++j )
        value[j] = v[j];
    return true;
}

int32 c_fckit_configuration_get_array_float( const Configuration* This, const char* name, float*& value,
                                             size_t& size ) {
    vector<float> v;
    if ( !This->get( string( name ), v ) )
        return false;
    size  = v.size();
    value = new float[size];
    for ( size_t j = 0; j < v.size(); ++j )
        value[j] = v[j];
    return true;
}

int32 c_fckit_configuration_get_array_double( const Configuration* This, const char* name, double*& value,
                                              size_t& size ) {
    vector<double> v;
    if ( !This->get( string( name ), v ) )
        return false;
    size  = v.size();
    value = new double[size];
    for ( size_t j = 0; j < v.size(); ++j )
        value[j] = v[j];
    return true;
}

int32 c_fckit_configuration_get_array_string( const Configuration* This, const char* name, char*& value, size_t& size,
                                              size_t*& offsets, size_t& numelem ) {
    vector<string> s;
    if ( !This->get( string( name ), s ) ) {
        return false;
    }
    numelem = s.size();
    offsets = new size_t[numelem];
    size    = 0;
    for ( size_t j = 0; j < numelem; ++j ) {
        offsets[j] = size;
        size += s[j].size();
    }
    value = new char[size + 1];
    for ( size_t j = 0; j < numelem; ++j ) {
        strcpy( &value[offsets[j]], s[j].c_str() );
    }
    return true;
}

int32 c_fckit_configuration_has( const Configuration* This, const char* name ) {
    return This->has( name );
}

int32 c_fckit_configuration_get_size( const Configuration* This, const char* name ) {
    return This->getStringVector( name ).size();
}

void c_fckit_configuration_json( const Configuration* This, char*& json, size_t& size ) {
    stringstream s;
    JSON parser( s );
    parser.precision( 17 );  // round-trippable double
    parser << *This;
    string json_str = s.str();
    size            = json_str.size();
    json            = new char[size + 1];
    strcpy( json, json_str.c_str() );
}

}  // extern "C"

}  // namespace fckit
