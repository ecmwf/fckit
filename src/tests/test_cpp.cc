#include "fckit/Main.h"
#include "fckit/Log.h"
extern "C" {
  void run();
}
int main(int argc, char* argv[])
{
  fckit::Main::initialize(argc,argv);
  fckit::Log::setFortranUnit(6,fckit::Log::TIMESTAMP);
  run();
  fckit::Main::finalize();
  return 0;
}