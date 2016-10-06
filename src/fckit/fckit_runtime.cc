#ifndef fckit_resource_h
#define fckit_resource_h

#include "eckit/runtime/Main.h"

static int SUCCESS =  0;
static int ERROR   = -1;

extern "C"
{

  int fckit__runtime_main_init(int argc, char* argv[])
  {
    if( not eckit::Main::ready() ) eckit::Main::initialise(argc,argv);
    return SUCCESS;
  }

  int fckit__runtime_main_ready(int& ready)
  {
    ready = eckit::Main::ready();
    return SUCCESS;
  }

}

#endif
