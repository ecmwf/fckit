/*
 * (C) Copyright 2013 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "fckit/Main.h"
#include "fckit/Log.h"
#include "eckit/mpi/Comm.h"
#include "fckit/Libfckit.h"

using namespace fckit;

extern "C" {
  void run();
}


#include <memory>
#include "eckit/log/OStreamTarget.h"
#include "eckit/log/PrefixTarget.h"
namespace fckit {
namespace test {
class Libdummy {
public:
  static Libdummy& instance() {
    static Libdummy lib;
    return lib;
  }
  void finalise() {
    traceChannel_.reset( new eckit::Channel() );
  }
  eckit::Channel& traceChannel() {
    if( traceChannel_ ) return *traceChannel_;
    traceChannel_.reset( new eckit::Channel(
        new eckit::PrefixTarget("FCKIT_TRACE",new eckit::OStreamTarget(eckit::Log::info()))));
    return *traceChannel_;
}
private:
  std::unique_ptr<eckit::Channel> traceChannel_;
};
}
}


int main(int argc, char* argv[])
{
  Main::initialize(argc,argv);
  Main::instance().taskID(eckit::mpi::comm().rank());

  test::Libdummy::instance().traceChannel() << "before configure of logging" << std::endl;

  if( Main::instance().taskID() == 0 ) {
    Log::setFortranUnit(Log::output_unit(),Log::TIMESTAMP);
    Log::addFile("fckit_test_cpp.log",Log::TIMESTAMP);
  } else {
    Log::reset();
  }
  test::Libdummy::instance().traceChannel() << "after configure of logging" << std::endl;


  run();
  Log::debug<Libfckit>() << "message from Libfckit" << std::endl;
  test::Libdummy::instance().finalise();
  Main::finalize();
  return 0;
}
