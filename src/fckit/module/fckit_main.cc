#include <string.h>
#include "eckit/config/LibEcKit.h"
#include "eckit/config/LibEcKit.h"
#include "fckit/Main.h"
#include "fckit/Log.h"

static int SUCCESS =  0;
//static int ERROR   = -1;


#include "eckit/os/BackTrace.h"
#include "eckit/mpi/Comm.h"
#include <csignal>
#include <exception>
#include <stdexcept>


void fckit_abort( const std::string& what, const eckit::CodeLocation& loc ) {
  throw eckit::Abort(what,loc);
}

void fckit_throw( const std::string& what, const eckit::CodeLocation& loc ) {
  if( loc )
    throw eckit::Exception(what,loc);
  else
    throw eckit::Exception(what);
}

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
    ::strcpy(name,v.c_str());
    return SUCCESS;
  }

  int fckit__main_displayname(char* &name, int &size)
  {
    std::string v = dynamic_cast<const fckit::Main&>(fckit::Main::instance()).displayName();
    size = v.size();
    name = new char[size+1];
    ::strcpy(name,v.c_str());
    return SUCCESS;
  }

  void fckit__set_abort_handler( eckit::abort_handler_t h )
  {
    eckit::LibEcKit::instance().setAbortHandler(h);  
  }

  void fckit__abort( const char* what, const char* file, int line, const char* function )
  {
    fckit_abort( what, eckit::CodeLocation(file,line,function) );
  }

  void fckit__exception_throw( const char* what, const char* file, int line, const char* function )
  {
    fckit_throw( what, eckit::CodeLocation(file,line,function) );
  }
  
  
  void fckit__set_signal_handler( int signum, fckit::signal_handler_t signal_handler )
  {
    fckit::Signals::instance().setSignalHandler( fckit::Signal(signum, signal_handler) );
  }
  
  void fckit__set_fckit_signal_handler( int signum )
  {
    fckit::Signals::instance().setSignalHandler( fckit::Signal(signum) );
  }

  void fckit__set_fckit_signal_handlers()
  {
    fckit::Signals::instance().setSignalHandlers();
  }

  void fckit__raise_signal( int signum )
  {
    std::raise( signum );
  }

  void fckit__restore_signal_handler( int signum )
  {
    fckit::Signals::instance().restoreSignalHandler( signum );
  }

  void fckit__restore_all_signal_handlers()
  {
    fckit::Signals::instance().restoreAllSignalHandlers();
  }

  int fckit__SIGABRT() { return SIGABRT; }
  int fckit__SIGKILL() { return SIGKILL; }
  int fckit__SIGINT()  { return SIGINT;  }
  int fckit__SIGALRM() { return SIGALRM; }
  int fckit__SIGFPE()  { return SIGFPE;  }
  int fckit__SIGTERM() { return SIGTERM; }
  int fckit__SIGSEGV() { return SIGSEGV; }
  int fckit__SIGILL()  { return SIGILL;  }

} // extern "C"

