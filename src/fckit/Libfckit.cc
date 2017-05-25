/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <string>
#include <algorithm>

#include "fckit/Libfckit.h"
#include "fckit/fckit_defines.h"
#include "eckit/eckit_version.h"

namespace fckit {

//----------------------------------------------------------------------------------------------------------------------

// Support for eckit 0.16.5 improved library registration using REGISTER_LIBRARY macro
// See issue ECKIT-244
#if ECKIT_MAJOR_VERSION >= 0 && ECKIT_MINOR_VERSION >= 16 && ECKIT_PATCH_VERSION >= 5
#define DECLARE_STATIC(Library,lib) static Library lib
#else
#define REGISTER_LIBRARY(Library) static Library library
#define DECLARE_STATIC(Library,lib)
#endif

REGISTER_LIBRARY(Libfckit);

Libfckit::Libfckit() : eckit::system::Library("fckit") {}

const Libfckit& Libfckit::instance()
{
    DECLARE_STATIC(Libfckit,library);
    return library;
}

const void* Libfckit::addr() const { return this; }

std::string Libfckit::version() const {
    return FCKIT_VERSION;
}

std::string Libfckit::gitsha1(unsigned int count) const {
    static std::string sha1(FCKIT_GIT_SHA1);
    if(sha1.empty()) {
        return "not available";
    }
    sha1 = sha1.substr(0, std::min(count,40u));
    return sha1.c_str();
}

//----------------------------------------------------------------------------------------------------------------------

} // namespace fckit

