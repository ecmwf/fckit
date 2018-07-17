/*
 * (C) Copyright 2013 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <iostream>
#include <stdexcept>
#include <sstream>
#include <cstdarg>
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/config/Configuration.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/parser/JSON.h"
#include "eckit/parser/JSONParser.h"

using std::vector;
using std::string;
using std::stringstream;
using eckit::Configuration;
using eckit::LocalConfiguration;
using eckit::YAMLConfiguration;
using eckit::JSON;
using eckit::PathName;
using eckit::Exception;
using eckit::CodeLocation;

namespace fckit {

class NotLocalConfiguration : public Exception {
  public:
    NotLocalConfiguration(const CodeLocation& location) :
      Exception("Configuration must be of concrete LocalConfiguration type",location) {
    }
};

class ConfigurationNotFound : public Exception {
  public:
    ConfigurationNotFound(const std::string& name) :
      Exception("Could not find \""+name+"\" in Configuration") {
    }
};


extern "C" {

void c_fckit_throw_configuration_not_found ( const char* name ) {
  throw ConfigurationNotFound(name);
}

Configuration* c_fckit_configuration_new () {
    return new LocalConfiguration();
}

Configuration* c_fckit_configuration_new_from_json (const char* json) {
    stringstream s;
    s << json;
    return new YAMLConfiguration(s);
}

const Configuration* c_fckit_configuration_new_from_file (const char* path) {
    PathName p(path);
    return new YAMLConfiguration( p );
}

const Configuration* c_fckit_configuration_new_from_buffer (eckit::CountedBuffer* buffer) {
    eckit::SharedBuffer sb(buffer);
    return new YAMLConfiguration( eckit::SharedBuffer(buffer) );
}

void c_fckit_configuration_delete (Configuration* This) {
    ASSERT( This != 0 );
    delete This;
}

void c_fckit_configuration_set_config (Configuration* This, const char* name, const Configuration* value)
{
    ASSERT( This != 0 );
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
        local->set( string(name), LocalConfiguration(*value) );
    else
        throw NotLocalConfiguration(Here());
}

void c_fckit_configuration_set_config_list (Configuration* This, const char* name, const Configuration* value[], int size)
{
    ASSERT( This != 0 );
    vector<LocalConfiguration> params(size);
    for(int i = 0; i < size; ++i)
        params[i] = LocalConfiguration(*value[i]);
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
        local->set( string(name), params );
    else
        throw NotLocalConfiguration(Here());
}

void c_fckit_configuration_set_int (Configuration* This, const char* name, int value) {
    ASSERT( This != 0 );
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
        local->set( string(name), value );
    else
        throw NotLocalConfiguration(Here());
}

void c_fckit_configuration_set_long (Configuration* This, const char* name, long value) {
    ASSERT( This != 0 );
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
        local->set( string(name), value );
    else
        throw NotLocalConfiguration(Here());
}

void c_fckit_configuration_set_float (Configuration* This, const char* name, float value) {
    ASSERT( This != 0 );
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
        local->set( string(name), value );
    else
        throw NotLocalConfiguration(Here());
}

void c_fckit_configuration_set_double (Configuration* This, const char* name, double value) {
    ASSERT( This != 0 );
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
        local->set( string(name), value );
    else
        throw NotLocalConfiguration(Here());
}

void c_fckit_configuration_set_string (Configuration* This, const char* name, const char* value) {
    ASSERT( This != 0 );
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
        local->set( string(name), string(value) );
    else
        throw NotLocalConfiguration(Here());
}

void c_fckit_configuration_set_array_int (Configuration* This, const char* name, int value[], int size) {
    ASSERT( This != 0 );
    vector<int> v;
    v.assign(value,value+size);
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
        local->set( string(name), v );
    else
        throw NotLocalConfiguration(Here());
}

void c_fckit_configuration_set_array_long (Configuration* This, const char* name, long value[], int size) {
    ASSERT( This != 0 );
    vector<long> v;
    v.assign(value,value+size);
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
        local->set( string(name), v );
    else
        throw NotLocalConfiguration(Here());
}

void c_fckit_configuration_set_array_float (Configuration* This, const char* name, float value[], int size) {
    vector<float> v;
    v.assign(value,value+size);
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
        local->set( string(name), v );
    else
        throw NotLocalConfiguration(Here());
}
void c_fckit_configuration_set_array_double (Configuration* This, const char* name, double value[], int size) {
    vector<double> v;
    v.assign(value,value+size);
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
        local->set( string(name), v );
    else
        throw NotLocalConfiguration(Here());
}

int c_fckit_configuration_get_config (const Configuration* This, const char* name, LocalConfiguration* value) {
    if( ! This->get(string(name),*value) )
        return false;
    return true;
}

int c_fckit_configuration_get_config_list (const Configuration* This, const char* name, LocalConfiguration** &value, int &size) {
    value = 0;
    vector<LocalConfiguration> vector;
    if( ! This->get(string(name),vector) ) return false;
    size = vector.size();
    value = new LocalConfiguration*[size];
    for(int i = 0; i < size; ++i) {
        value[i] = new LocalConfiguration(vector[i]);
    }
    return true;
}

int c_fckit_configuration_get_int (const Configuration* This, const char* name, int& value) {
    if( ! This->get(string(name),value) )  return false;
    return true;
}

int c_fckit_configuration_get_long (const Configuration* This, const char* name, long& value) {
    if( ! This->get(string(name),value) )  return false;
    return true;
}

int c_fckit_configuration_get_float (const Configuration* This, const char* name, float& value) {
    if( ! This->get(string(name),value) )  return false;
    return true;
}

int c_fckit_configuration_get_double (const Configuration* This, const char* name, double& value) {
    if( ! This->get(string(name),value) )  return false;
    return true;
}

int c_fckit_configuration_get_string( const Configuration* This, const char* name, char* &value, int &size) {
    string s;
    if( ! This->get(string(name),s) ) {
        value = NULL;
        return false;
    }
    size = s.size()+1;
    value = new char[size];
    strcpy(value,s.c_str());
    return true;
}

int c_fckit_configuration_get_array_int (const Configuration* This, const char* name, int* &value, int& size) {
    vector<int> v;
    if( ! This->get(string(name),v) )
        return false;
    size = v.size();
    value = new int[size];
    for( size_t j=0; j<v.size(); ++j ) value[j] = v[j];
    return true;
}

int c_fckit_configuration_get_array_long (const Configuration* This, const char* name, long* &value, int& size) {
    vector<long> v;
    if( ! This->get(string(name),v) )
        return false;
    size = v.size();
    value = new long[size];
    for( size_t j=0; j<v.size(); ++j ) value[j] = v[j];
  return true;
}

int c_fckit_configuration_get_array_float (const Configuration* This, const char* name, float* &value, int& size) {
    vector<float> v;
    if( ! This->get(string(name),v) )
        return false;
    size = v.size();
    value = new float[size];
    for( size_t j=0; j<v.size(); ++j ) value[j] = v[j];
    return true;
}

int c_fckit_configuration_get_array_double (const Configuration* This, const char* name, double* &value, int& size) {
    vector<double> v;
    if( ! This->get(string(name),v) )
        return false;
    size = v.size();
    value = new double[size];
    for( size_t j=0; j<v.size(); ++j ) value[j] = v[j];
    return true;
}

int c_fckit_configuration_has (const Configuration* This, const char *name) {
    return This->has( string(name) );
}

void c_fckit_configuration_json(const Configuration* This, char* &json, int &size) {
    stringstream s;
    JSON parser(s);
    parser.precision(17); // round-trippable double
    parser << *This;
    string json_str = s.str();
    size = json_str.size();
    json = new char[size+1];
    strcpy(json,json_str.c_str());
}

} // extern "C"

} // namespace fckit
