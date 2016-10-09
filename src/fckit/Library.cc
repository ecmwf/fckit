/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "fckit/Library.h"

namespace fckit {

//----------------------------------------------------------------------------------------------------------------------

static Library library;

Library::Library() : eckit::system::Library("fckit") {}

const Library& Library::instance()
{
    return library;
}

const void* Library::addr() const { return this; }

std::string Library::version() const {
    return "NOTIMP";
    //return fckit_version_str();
}

std::string Library::gitsha1(unsigned int count) const {
    return"NOTIMP";
    // return fckit_git_sha1_abbrev(count);
}

//----------------------------------------------------------------------------------------------------------------------

} // namespace fckit

