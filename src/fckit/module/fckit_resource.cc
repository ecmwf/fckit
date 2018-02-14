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
#include "eckit/config/Resource.h"

static int SUCCESS =  0;
//static int ERROR   = -1;

extern "C"
{
  int fckit__resource_int (const char* resource, int default_value, int &value)
  {
    value = eckit::Resource<int>( std::string(resource), default_value );
    return SUCCESS;
  }

  int fckit__resource_long (const char* resource, long default_value, long &value)
  {
    value = eckit::Resource<long>( std::string(resource), default_value );
    return SUCCESS;
  }

  int fckit__resource_float (const char* resource, float default_value, float &value)
  {
    value = static_cast<float>( eckit::Resource<double>( std::string(resource), static_cast<double>(default_value) ) );
    return SUCCESS;
  }

  int fckit__resource_double (const char* resource, double default_value, double &value)
  {
    value = eckit::Resource<double>( std::string(resource), default_value );
    return SUCCESS;
  }

  int fckit__resource_string (const char* resource, const char* default_value, char* &value, int &size )
  {
    std::string v = eckit::Resource<std::string>( std::string(resource), std::string(default_value) );
    size = (int)v.size();
    value = new char[size+1];
    ::strcpy(value,v.c_str());
    return SUCCESS;
  }
}
