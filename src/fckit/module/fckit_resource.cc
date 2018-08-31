/*
 * (C) Copyright 2013 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <string.h>
#include <cstdint>
#include <cstring>
#include "eckit/config/Resource.h"

using int32 = std::int32_t;
using int64 = std::int64_t;

static int32 SUCCESS =  0;
//static int ERROR   = -1;

extern "C"
{
  int32 fckit__resource_int32 (const char* resource, int32 default_value, int32 &value)
  {
    value = eckit::Resource<int32>( std::string(resource), default_value );
    return SUCCESS;
  }

  int32 fckit__resource_int64 (const char* resource, int64 default_value, int64 &value)
  {
    value = eckit::Resource<int64>( std::string(resource), default_value );
    return SUCCESS;
  }

  int32 fckit__resource_float (const char* resource, float default_value, float &value)
  {
    value = static_cast<float>( eckit::Resource<double>( std::string(resource), static_cast<double>(default_value) ) );
    return SUCCESS;
  }

  int32 fckit__resource_double (const char* resource, double default_value, double &value)
  {
    value = eckit::Resource<double>( std::string(resource), default_value );
    return SUCCESS;
  }

  int32 fckit__resource_string (const char* resource, const char* default_value, char* &value, size_t &size )
  {
    std::string v = eckit::Resource<std::string>( std::string(resource), std::string(default_value) );
    size = v.size();
    value = new char[size+1];
    std::strcpy(value,v.c_str());
    return SUCCESS;
  }
}
