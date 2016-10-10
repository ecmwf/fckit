#include "fckit/runtime/Main.h"
#include "fckit/log/Log.h"

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

  int fckit__main_debug()
  {
    return fckit::Log::debug()!=0;
  }

  int fckit__main_name(char* &name, int &size)
  {
    std::string v = fckit::Main::instance().name();
    size = v.size();
    name = new char[size+1];
    strcpy(name,v.c_str());
    return SUCCESS;
  }


} // extern "C"

