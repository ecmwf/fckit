#include "fckit/fckit_runtime.h"
#include "fckit/fckit_log.h"
#include "eckit/log/TimeStampTarget.h"
#include "eckit/mpi/Comm.h"

namespace fckit{

static Main* instance_ = 0;
static int MPI_COMM_WORLD_RANK = -1;
static int COUT = -1;
//static int CERR = -1;
static int ALL_TASKS = -1;

Main::Main(
    int argc, char **argv,
    int task,
    int output_task,
    int output_unit,
    int error_unit,
    int log_simple,
    const char* homeenv)
    : eckit::Main(argc,argv,homeenv)
{
  if( task == MPI_COMM_WORLD_RANK ) {
    taskID(eckit::mpi::comm("world").rank());
  } else {
    taskID(task);
  }
  outputTask_ = output_task;
  outputUnit_ = output_unit;
  errorUnit_  = error_unit;
  logSimple_  = log_simple;
}

void Main::initialise(
    int argc, char** argv,
    int task,
    int output_task,
    int output_unit,
    int error_unit,
    int log_simple,
    const char* homeenv) {
    if (instance_ == 0) {
        instance_ = new Main(
              argc,argv,
              task,
              output_task,
              output_unit,
              error_unit,
              log_simple,
              homeenv);
    }
}

// TODO: make nicer
eckit::LogTarget* Main::createInfoLogTarget() const {
    if( logSimple() ) {
        if( outputUnit_ != COUT )
            return new fckit::FortranUnitTarget(outputUnit_);
        else
            return createDefaultLogTarget();
    } else {
        if( outputUnit_ != COUT )
            return new fckit::TimeStampFortranUnitTarget(outputUnit_,"(I fort)");
        else
            return new eckit::TimeStampTarget("(I)");
    }
}
eckit::LogTarget* Main::createWarningLogTarget() const {
  if( logSimple() ) {
      if( outputUnit_ != COUT )
          return new fckit::FortranUnitTarget(outputUnit_);
      else
          return createDefaultLogTarget();
  } else {
      if( outputUnit_ != COUT )
          return new fckit::TimeStampFortranUnitTarget(outputUnit_,"(W fort)");
      else
          return new eckit::TimeStampTarget("(W)");
  }
}
eckit::LogTarget* Main::createErrorLogTarget() const {
  if( logSimple() ) {
      if( outputUnit_ != COUT )
          return new fckit::FortranUnitTarget(outputUnit_);
      else
          return createDefaultLogTarget();
  } else {
      if( outputUnit_ != COUT )
          return new fckit::TimeStampFortranUnitTarget(outputUnit_,"(E fort)");
      else
          return new eckit::TimeStampTarget("(E)");
  }
}
eckit::LogTarget* Main::createDebugLogTarget() const {
  if( logSimple() ) {
      if( outputUnit_ != COUT )
          return new fckit::FortranUnitTarget(outputUnit_);
      else
          return createDefaultLogTarget();
  } else {
      if( outputUnit_ != COUT )
          return new fckit::TimeStampFortranUnitTarget(outputUnit_,"(D fort)");
      else
          return new eckit::TimeStampTarget("(D)");
  }
}

} // namespace fckit


static int SUCCESS =  0;
//static int ERROR   = -1;

extern "C"
{

int fckit__runtime_main_init(
      int argc, char* argv[],
      int task,
      int output_task,
      int output_unit,
      int error_unit,
      int log_simple )
  {
    if( not fckit::Main::ready() ) {
      fckit::Main::initialise(
          argc,argv,
          task,
          output_task,
          output_unit,
          error_unit,
          log_simple
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

} // extern "C"

