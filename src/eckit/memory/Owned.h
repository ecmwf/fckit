/*
 * (C) Copyright 1996- ECMWF.
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


namespace eckit {

//----------------------------------------------------------------------------------------------------------------------

/// Reference counting objects
/// Subclass from this class to use a SharedPtr class

template <typename LOCK>
class OwnedT : public LOCK {

public:  // methods

    OwnedT() : count_(0) {}

    OwnedT(const OwnedT&)            = delete;
    OwnedT& operator=(const OwnedT&) = delete;
    OwnedT(OwnedT&&)                 = delete;
    OwnedT& operator=(OwnedT&&)      = delete;

    virtual ~OwnedT() {}

    void attach() const {
        LOCK::lock();
        count_++;
        LOCK::unlock();
    }

    void detach() const {
        LOCK::lock();
        --count_;
        LOCK::unlock();
    }

    size_t owners() const { return count_; }

private:  // members

    mutable size_t count_;
};

//----------------------------------------------------------------------------------------------------------------------

/// Owned object without thread lockable resource
using OwnedLock = OwnedT<memory::detail::ThreadedLock>;

/// Owned object with thread lockable resource
using OwnedNoLock = OwnedT<memory::detail::NoLock>;

/// Default Owned type
/// Same as OwnedNoLock
using Owned = OwnedNoLock;

//----------------------------------------------------------------------------------------------------------------------

}  // namespace eckit

#endif
