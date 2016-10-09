#ifndef fckit_resource_h
#define fckit_resource_h

#include "eckit/config/Resource.h"

static int SUCCESS =  0;
static int ERROR   = -1;

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

  int fckit__resource_string (const char* resource, const char* default_value,  char* &value, size_t &value_size )
  {
    std::string v = eckit::Resource<std::string>( std::string(resource), std::string(default_value) );
    value_size = v.size();
    value = new char[value_size+1];
    strcpy(value,v.c_str());
    return SUCCESS;
  }
}

#endif
