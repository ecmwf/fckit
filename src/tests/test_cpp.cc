#include "fckit/runtime/Main.h"
#include "fckit/log/Log.h"
#include "eckit/mpi/Comm.h"
extern "C" {
  void run();
}
int main(int argc, char* argv[])
{
  fckit::Main::initialize(argc,argv);
  fckit::Main::instance().taskID(eckit::mpi::comm().rank());
  if( fckit::Main::instance().taskID() == 0 )
    fckit::Log::setFortranUnit(6,fckit::Log::TIMESTAMP);
  else
    fckit::Log::reset();
  run();
  fckit::Main::finalize();
  return 0;
}