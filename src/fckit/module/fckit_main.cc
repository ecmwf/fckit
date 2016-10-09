#include "fckit/runtime/Main.h"

static int SUCCESS =  0;
//static int ERROR   = -1;

extern "C"
{

  int fckit__main_init(
      int argc, char* argv[]
  )
  {
    if( not fckit::Main::ready() ) {
      fckit::Main::initialise(
          argc,argv
      );
    }

    return SUCCESS;
  }
  
  void fckit__main_finalise()
  {
    fckit::Main::finalise();
  }

  int fckit__main_ready(int& ready)
  {
    ready = fckit::Main::ready();
    return SUCCESS;
  }

  int fckit__main_taskID(int& taskID)
  {
    taskID = fckit::Main::instance().taskID();
    return SUCCESS;
  }
  
  int fckit__main_setTaskID(int taskID)
  {
    fckit::Main::instance().taskID(taskID);
    return SUCCESS;
  }
  

} // extern "C"

