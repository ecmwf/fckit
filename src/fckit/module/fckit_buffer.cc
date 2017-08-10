#include <string>

#include "eckit/eckit_version.h"

#ifdef ECKIT_VERSION_INT
#undef ECKIT_VERSION_INT
#endif
#define ECKIT_VERSION_INT (ECKIT_MAJOR_VERSION * 10000 \
                         + ECKIT_MINOR_VERSION * 100 \
                         + ECKIT_PATCH_VERSION)

#if ECKIT_VERSION_INT > 1700

#include "eckit/io/SharedBuffer.h"

extern "C" {

  int c_fckit_buffer_str(const eckit::Buffer* This, char* &str, size_t &size) {
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

#else

// Legacy
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

#endif

