#include <sstream>
#include "eckit/mpi/Comm.h"


// Temporary until ECKIT-166 is fixed
#ifdef BUG_ECKIT_166
#include <mpi.h>
#endif

using eckit::mpi::Comm;

namespace fckit {
static bool areMPIVarsSet() {
    return (::getenv("OMPI_COMM_WORLD_SIZE") || ::getenv("ALPS_APP_PE"));
}
}

extern "C" {

// Temporary until ECKIT-166 is fixed
  void eckit__mpi__finalize() {
#ifdef BUG_ECKIT_166
    if( fckit::areMPIVarsSet() ) {
      int finalized = 1;
      MPI_Finalized(&finalized);
      if( not finalized ) {
        MPI_Finalize();
      }
    }
#endif
  }

  const Comm* eckit__mpi__comm_default() {
    return &eckit::mpi::comm();
  }

  const Comm* eckit__mpi__comm(const char* name) {
    return &eckit::mpi::comm(name);
  }

  const Comm* eckit__mpi__comm_wrap(int comm) {
    std::ostringstream s; s << "fort."<<comm;
    std::string name = s.str();
    if( not eckit::mpi::hasComm(name.c_str()) ) {
      eckit::mpi::addComm(name.c_str(), comm);
    }
    return &eckit::mpi::comm(name.c_str());
  }

  void eckit__mpi__setCommDefault_int(int comm) {
    std::ostringstream s; s << "fort."<<comm;
    std::string name = s.str();
    if( not eckit::mpi::hasComm(name.c_str()) ) {
      eckit::mpi::addComm(name.c_str(), comm);
    }
    eckit::mpi::setCommDefault(name.c_str());
  }

  void eckit__mpi__setCommDefault_name(const char* name) {
    eckit::mpi::setCommDefault(name);
  }

  int eckit__mpi__size(const Comm* comm) {
    return comm->size();
  }

  int eckit__mpi__rank(const Comm* comm) {
    return comm->rank();
  }

  void eckit__mpi__barrier(const Comm* comm) {
    return comm->barrier();
  }

}

