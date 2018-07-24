/*
 * (C) Copyright 2013 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <string>
#include <cstdint>

#include "eckit/io/SharedBuffer.h"

using int32  = std::int32_t;
using size_t = std::size_t;

extern "C" {

  int32 c_fckit_buffer_str(const eckit::Buffer* This, char* &str, size_t &size) {
    std::string s(*This, This->size());
    size = s.size()+1;
    str = new char[size];
    strcpy(str,s.c_str());
    return true;
  }

  void c_fckit_buffer_delete( eckit::CountedBuffer* This ) {
    delete This;
  }

}
