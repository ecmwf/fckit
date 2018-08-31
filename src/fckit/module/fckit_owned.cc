/*
 * (C) Copyright 2013 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <cstdint>
#include "fckit/fckit.h"

#if FCKIT_HAVE_ECKIT
#include "eckit/memory/Owned.h"

extern "C" {

  std::int32_t fckit__Owned__owners(const eckit::Owned* owned)
  {
    return owned->owners();
  }

  void fckit__Owned__attach(const eckit::Owned* owned)
  {
    owned->attach();
  }

  void fckit__Owned__detach(const eckit::Owned* owned)
  {
    owned->detach();
  }

  void fckit__delete_Owned(eckit::Owned* owned)
  {
    delete owned;
    owned = 0;
  }

}

#else

extern "C" {
  std::int32_t  fckit__Owned__owners(const eckit::Owned* owned) { return 0; }
  void fckit__Owned__attach(const void*) {}
  void fckit__Owned__detach(const void*) {}
  void fckit__delete_Owned(void*) {}
}

#endif

