#include "fckit/Main.h"
#include "eckit/thread/AutoLock.h"
#include "eckit/thread/Mutex.h"
#include "eckit/thread/Once.h"
#include "eckit/log/Log.h"
#include "eckit/mpi/Comm.h"

// Temporary until ECKIT-166 is fixed, only included for MacOSX
#ifdef BUG_ECKIT_166
#include <mpi.h>
#endif

static eckit::Once<eckit::Mutex> local_mutex;

namespace fckit{

Main::Main(
    int argc, char **argv,
    const char* homeenv)
    : eckit::Main(argc,argv,homeenv)
{
  for( int j=0; j<argc; ++j )
  {
    std::string arg(argv[j]);
    if( arg.find("--displayname=") == 0 )
    {
       size_t pos = arg.find("--displayname=") + 14;
       displayName_ = arg.substr(pos);
    }
    if( arg == "--displayname" )
    {
      if( j+1 < argc ) displayName_ = argv[j+1];
    }
  }
  
  taskID(eckit::mpi::comm("world").rank());
  
}

const std::string& Main::displayName() const { return displayName_; }


void Main::initialise(
    int argc, char** argv,
    const char* homeenv)
{
    eckit::AutoLock<eckit::Mutex> lock(local_mutex);
    if( not ready() ) {
        new Main(argc,argv,homeenv);
    }
}

void Main::finalise()
{
// Temporary until ECKIT-166 is fixed, only included for MacOSX
#ifdef BUG_ECKIT_166
    bool using_mpi = (::getenv("OMPI_COMM_WORLD_SIZE") || ::getenv("ALPS_APP_PE"));
    if( using_mpi ) {
      int mpi_initialized = 1;
      MPI_Initialized(&mpi_initialized);
      int mpi_finalized = 1;
      MPI_Finalized(&mpi_finalized);
      if( not mpi_finalized and mpi_initialized ) {
        MPI_Finalize();
      }
    }
#endif

// Temporary until ECKIT-175 is fixed
    eckit::Log::reset();
}

} // namespace fckit

