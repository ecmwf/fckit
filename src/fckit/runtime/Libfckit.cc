/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "fckit/runtime/Libfckit.h"

namespace fckit {

//----------------------------------------------------------------------------------------------------------------------

static Libfckit library;

Libfckit::Libfckit() : eckit::system::Library("fckit") {}

const Libfckit& Libfckit::instance()
{
    return library;
}

const void* Libfckit::addr() const { return this; }

std::string Libfckit::version() const {
    return "not available";
}

std::string Libfckit::gitsha1(unsigned int count) const {
    return "not available";
}

//----------------------------------------------------------------------------------------------------------------------

} // namespace fckit

