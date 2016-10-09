#include "eckit/mpi/Comm.h"
#include "eckit/thread/AutoLock.h"
#include "eckit/thread/Mutex.h"
#include "eckit/thread/Once.h"
#include "fckit/Main.h"

// Temporary until ECKIT-166 is fixed, only included for MacOSX
#ifdef BUG_ECKIT_166
#include <mpi.h>
#endif

using eckit::mpi::Comm;
static eckit::Once<eckit::Mutex> local_mutex;

namespace fckit{

Main::Main(
    int argc, char **argv,
    const char* homeenv)
    : eckit::Main(argc,argv,homeenv)
{
    taskID(eckit::mpi::comm("world").rank());
}

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
      int finalized = 1;
      MPI_Finalized(&finalized);
      if( not finalized ) {
        MPI_Finalize();
      }
    }
#endif
}

} // namespace fckit

