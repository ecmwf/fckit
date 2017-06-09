#include "fckit/Main.h"
#include "fckit/Log.h"
#include "eckit/mpi/Comm.h"
#include "fckit/Libfckit.h"

using namespace fckit;

extern "C" {
  void run();
}
int main(int argc, char* argv[])
{
  Main::initialize(argc,argv);
  Main::instance().taskID(eckit::mpi::comm().rank());

  if( Main::instance().taskID() == 0 ) {
    Log::setFortranUnit(Log::output_unit(),Log::TIMESTAMP);
    Log::addFile("fckit_test_cpp.log",Log::TIMESTAMP);
  } else {
    Log::reset();
  }
  run();
  Log::debug<Libfckit>() << "message from Libfckit" << std::endl;
  Main::finalize();
  return 0;
}
