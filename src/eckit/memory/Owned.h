/*
 * (C) Copyright 1996-2013 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @file Owned.h
/// @author Tiago Quintino
/// @date May 2014

#ifndef eckit_memory_Owned_h
#define eckit_memory_Owned_h

#include "eckit/memory/Counted.h"

//-----------------------------------------------------------------------------

namespace eckit {

//-----------------------------------------------------------------------------

/// Counted object, with third party managed memory and not thread lockable resource
typedef CountedT< memory::detail::ThreadedLock, memory::detail::NotManaged >  OwnedLock;

/// Counted object, with third party managed memory and not thread lockable resource
typedef CountedT< memory::detail::NoLock, memory::detail::NotManaged >  OwnedNoLock;

/// Default Owned type
/// Same as OwnedNoLock
typedef OwnedNoLock Owned;

//-----------------------------------------------------------------------------

} // namespace eckit

#endif
