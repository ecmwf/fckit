/*
 * (C) Copyright 1996-2017 ECMWF.
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
#include "eckit/config/JSONConfiguration.h"
#include "eckit/parser/JSON.h"
#include "eckit/parser/JSONParser.h"

using std::string;
using eckit::Configuration;
using eckit::LocalConfiguration;
using eckit::JSONConfiguration;

extern "C" {

Configuration* c_fckit_configuration_new () {
  return new LocalConfiguration();
}

Configuration* c_fckit_configuration_new_from_json (const char* json) {
  std::stringstream s;
  s << json;
  return new JSONConfiguration(s);
}

const Configuration* c_fckit_configuration_new_from_file (const char* path) {
  eckit::PathName p(path);
  return new JSONConfiguration( p );
}

void c_fckit_configuration_delete (Configuration* This) {
  ASSERT( This != 0 );
  delete This;
}

void c_fckit_configuration_set_config (Configuration* This, const char* name, const Configuration* value)
{
  ASSERT( This != 0 );
  if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
    local->set( std::string(name), LocalConfiguration(*value) );    
  else
    throw eckit::Exception("Configuration must be of concrete LocalConfiguration type");
}

void c_fckit_configuration_set_config_list (Configuration* This, const char* name, const Configuration* value[], int size)
{
  ASSERT( This != 0 );
  std::vector<LocalConfiguration> params(size);
  for(int i = 0; i < size; ++i)
    params[i] = LocalConfiguration(*value[i]);
  if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
    local->set( std::string(name), params );
  else
    throw eckit::Exception("Configuration must be of concrete LocalConfiguration type");
}

void c_fckit_configuration_set_int (Configuration* This, const char* name, int value) {
    ASSERT( This != 0 );
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
      local->set( std::string(name), long(value) );
    else
      throw eckit::Exception("Configuration must be of concrete LocalConfiguration type");
}
void c_fckit_configuration_set_long (Configuration* This, const char* name, long value) {
    ASSERT( This != 0 );
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
      local->set( std::string(name), value );
    else
      throw eckit::Exception("Configuration must be of concrete LocalConfiguration type");
}
void c_fckit_configuration_set_float (Configuration* This, const char* name, float value) {
    ASSERT( This != 0 );
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
      local->set( std::string(name), double(value) );
    else
      throw eckit::Exception("Configuration must be of concrete LocalConfiguration type");
}
void c_fckit_configuration_set_double (Configuration* This, const char* name, double value) {
    ASSERT( This != 0 );
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
      local->set( std::string(name), value );
    else
      throw eckit::Exception("Configuration must be of concrete LocalConfiguration type");
}
void c_fckit_configuration_set_string (Configuration* This, const char* name, const char* value) {
    ASSERT( This != 0 );
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
      local->set( std::string(name), std::string(value) );
    else
      throw eckit::Exception("Configuration must be of concrete LocalConfiguration type");
}
void c_fckit_configuration_set_array_int (Configuration* This, const char* name, int value[], int size) {
    ASSERT( This != 0 );
    std::vector<long> v;
    v.assign(value,value+size);
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
      local->set( std::string(name), v );
    else
      throw eckit::Exception("Configuration must be of concrete LocalConfiguration type");
}
void c_fckit_configuration_set_array_long (Configuration* This, const char* name, long value[], int size) {
    ASSERT( This != 0 );
    std::vector<long> v;
    v.assign(value,value+size);
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
      local->set( std::string(name), v );
    else
      throw eckit::Exception("Configuration must be of concrete LocalConfiguration type");
}
void c_fckit_configuration_set_array_float (Configuration* This, const char* name, float value[], int size) {
    std::vector<double> v;
    v.assign(value,value+size);
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
      local->set( std::string(name), v );
    else
      throw eckit::Exception("Configuration must be of concrete LocalConfiguration type");
}
void c_fckit_configuration_set_array_double (Configuration* This, const char* name, double value[], int size) {
    std::vector<double> v;
    v.assign(value,value+size);
    if( LocalConfiguration* local = dynamic_cast<LocalConfiguration*>(This) )
      local->set( std::string(name), v );
    else
      throw eckit::Exception("Configuration must be of concrete LocalConfiguration type");
}

int c_fckit_configuration_get_config (const Configuration* This, const char* name, LocalConfiguration* value) {
    if( ! This->get(std::string(name),*value) )
      return false;
    return true;
}

int c_fckit_configuration_get_config_list (const Configuration* This, const char* name, LocalConfiguration** &value, int &size) {
    value = 0;
    std::vector<LocalConfiguration> vector;
    if( ! This->get(std::string(name),vector) ) return false;
    size = vector.size();
    value = new LocalConfiguration*[size];
    for(int i = 0; i < size; ++i) {
      value[i] = new LocalConfiguration(vector[i]);
    }
    return true;
}

int c_fckit_configuration_get_int (const Configuration* This, const char* name, int& value) {
    long long_value = value;
    if( ! This->get(std::string(name),long_value) )  return false;
    ASSERT( int(long_value) == long_value );
    value = long_value;
    return true;
}
int c_fckit_configuration_get_long (const Configuration* This, const char* name, long& value) {
    if( ! This->get(std::string(name),value) )  return false;
    return true;
}
int c_fckit_configuration_get_float (const Configuration* This, const char* name, float& value) {
    double double_value;
    if ( ! This->get(std::string(name), double_value) ) return false;
    value = double_value;
    return true;
}
int c_fckit_configuration_get_double (const Configuration* This, const char* name, double& value) {
    if( ! This->get(std::string(name),value) )  return false;
    return true;
}
int c_fckit_configuration_get_string( const Configuration* This, const char* name, char* &value, int &size) {
    std::string s;
    if( ! This->get(std::string(name),s) ) {
      value = NULL;
      return false;
    }
    size = s.size()+1;
    value = new char[size];
    strcpy(value,s.c_str());
    return true;
}
int c_fckit_configuration_get_array_int (const Configuration* This, const char* name, int* &value, int& size) {
    std::vector<long> v;
    if( ! This->get(std::string(name),v) )
      return false;
    size = v.size();
    value = new int[size];
    for ( size_t j = 0; j < v.size(); ++j ) {
      ASSERT(int(v[j]) == v[j]);
      value[j] = v[j];
    }
    return true;
}
int c_fckit_configuration_get_array_long (const Configuration* This, const char* name, long* &value, int& size) {
    std::vector<long> v;
    if( ! This->get(std::string(name),v) )
      return false;
    size = v.size();
    value = new long[size];
    for( size_t j=0; j<v.size(); ++j ) value[j] = v[j];
  return true;
}
int c_fckit_configuration_get_array_float (const Configuration* This, const char* name, float* &value, int& size) {
    std::vector<double> v;
    if( ! This->get(std::string(name),v) )
      return false;
    size = v.size();
    value = new float[size];
    for ( size_t j = 0; j < v.size(); ++j ) {
      ASSERT(float(v[j]) == v[j]);
      value[j] = v[j];
    }
    return true;
}
int c_fckit_configuration_get_array_double (const Configuration* This, const char* name, double* &value, int& size) {
    std::vector<double> v;
    if( ! This->get(std::string(name),v) )
      return false;
    size = v.size();
    value = new double[size];
    for( size_t j=0; j<v.size(); ++j ) value[j] = v[j];
    return true;
}

int c_fckit_configuration_has (const Configuration* This, const char *name) {
    return This->has( std::string(name) );
    return 0;
}

void c_fckit_configuration_json(const Configuration* This, char* &json, int &size) {
#ifdef JSON
    std::stringstream s;
    eckit::JSON j(s);
    j.precision(16);
    j << *This;
    std::string json_str = s.str();
    size = json_str.size();
    json = new char[size+1];
    strcpy(json,json_str.c_str());
#else
    std::stringstream s;
    s << *This;
    std::string json_str = s.str();
    size = json_str.size();
    json = new char[size+1];
    strcpy(json,json_str.c_str());
#endif
}

} // extern "C"