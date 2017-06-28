#include <string>
#include "eckit/io/Buffer.h"

extern "C" {

  int c_fckit_buffer_str(const eckit::Buffer* This, char* &str, size_t &size) {
    std::string s(*This, This->size());
    size = s.size()+1;
    str = new char[size];
    strcpy(str,s.c_str());
    return true;  
  }

  void c_fckit_buffer_delete( eckit::Buffer* This ) {
    delete This;
  }

}

