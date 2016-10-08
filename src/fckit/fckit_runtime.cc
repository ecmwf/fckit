#include "fckit/fckit_runtime.h"
#include "fckit/fckit_log.h"
#include "eckit/log/TimeStampTarget.h"
#include "eckit/mpi/Comm.h"

namespace fckit{

static Main* instance_ = 0;
static int MPI_COMM_WORLD_RANK = -1;
static int ALL_TASKS = -1;

Main::Main(
    int argc, char **argv,
    const char* homeenv)
    : eckit::Main(argc,argv,homeenv)
{
    taskID(eckit::mpi::comm("world").rank());
    outputTask_ = 0;
    outputUnit_ = 6;
}

void Main::initialise(
    int argc, char** argv,
    const char* homeenv) {
    if (instance_ == 0) {
        instance_ = new Main(
              argc,argv,
              homeenv);
    }
}

eckit::LogTarget* Main::createInfoLogTarget() const {
  return createFortranUnitTarget(logTarget().c_str(),outputUnit_,"(I)");
}
eckit::LogTarget* Main::createWarningLogTarget() const {
  return createFortranUnitTarget(logTarget().c_str(),outputUnit_,"(W)");
}
eckit::LogTarget* Main::createErrorLogTarget() const {
    return createFortranUnitTarget(logTarget().c_str(),outputUnit_,"(E)");
}
eckit::LogTarget* Main::createDebugLogTarget() const {
  return createFortranUnitTarget(logTarget().c_str(),outputUnit_,"(D)");
}

} // namespace fckit


static int SUCCESS =  0;
//static int ERROR   = -1;

extern "C"
{

int fckit__runtime_main_init(
      int argc, char* argv[]
  )
  {
    if( not fckit::Main::ready() ) {
      fckit::Main::initialise(
          argc,argv
      );
    }

    if( fckit::Main::instance().outputTask() != fckit::ALL_TASKS ) {
        if( fckit::Main::instance().taskID() != fckit::Main::instance().outputTask() ) {
            eckit::Log::reset();
        }
    }

    return SUCCESS;
  }

  int fckit__runtime_main_ready(int& ready)
  {
    ready = fckit::Main::ready();
    return SUCCESS;
  }

  int fckit__runtime_main_output_task(int& output_task)
  {
    output_task = fckit::Main::instance().outputTask();
    return SUCCESS;
  }

  int fckit__runtime_main_taskID(int& taskID)
  {
    taskID = fckit::Main::instance().taskID();
    return SUCCESS;
  }

} // extern "C"

