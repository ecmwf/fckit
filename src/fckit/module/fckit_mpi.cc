#include <sstream>
#include "eckit/mpi/Comm.h"

using eckit::mpi::Comm;

extern "C" {

  const Comm* fckit__mpi__comm_default() {
    return &eckit::mpi::comm();
  }

  const Comm* fckit__mpi__comm(const char* name) {
    return &eckit::mpi::comm(name);
  }

  const Comm* fckit__mpi__comm_wrap(int comm) {
    std::ostringstream s; s << "fort."<<comm;
    std::string name = s.str();
    if( not eckit::mpi::hasComm(name.c_str()) ) {
      eckit::mpi::addComm(name.c_str(), comm);
    }
    return &eckit::mpi::comm(name.c_str());
  }

  void fckit__mpi__setCommDefault_int(int comm) {
    std::ostringstream s; s << "fort."<<comm;
    std::string name = s.str();
    if( not eckit::mpi::hasComm(name.c_str()) ) {
      eckit::mpi::addComm(name.c_str(), comm);
    }
    eckit::mpi::setCommDefault(name.c_str());
  }

  void fckit__mpi__setCommDefault_name(const char* name) {
    eckit::mpi::setCommDefault(name);
  }

  int fckit__mpi__comm_tag(const Comm* comm) {
    if( comm )
      return comm->tag();
    else
      return eckit::mpi::comm().tag();
  }

  int fckit__mpi__size(const Comm* comm) {
    if( comm )
      return comm->size();
    else
      return eckit::mpi::comm().size();
  }

  int fckit__mpi__rank(const Comm* comm) {
    if( comm )
      return comm->rank();
    else
      return eckit::mpi::comm().rank();
  }

  void fckit__mpi__barrier(const Comm* comm) {
    if( comm )
      return comm->barrier();
    else
      return eckit::mpi::comm().barrier();
  }

  void fckit__mpi__abort(const Comm* comm, int error_code) {
    if( comm )
      return comm->abort(error_code);
    else
      return eckit::mpi::comm().abort(error_code);
  }


  int fckit__mpi__sum()     { return int(eckit::mpi::sum());    }
  int fckit__mpi__prod()    { return int(eckit::mpi::prod());   }
  int fckit__mpi__max()     { return int(eckit::mpi::max());    }
  int fckit__mpi__min()     { return int(eckit::mpi::min());    }
  int fckit__mpi__maxloc()  { return int(eckit::mpi::maxloc()); }
  int fckit__mpi__minloc()  { return int(eckit::mpi::minloc()); }

  void fckit__mpi__allreduce_int32(const Comm* comm, const int* in, int* out, size_t count, int operation)
  {
    if( comm ) 
      comm->allReduce(in,out,count,eckit::mpi::Operation::Code(operation));
    else
      eckit::mpi::comm().allReduce(in,out,count,eckit::mpi::Operation::Code(operation));
  }

  void fckit__mpi__allreduce_int64(const Comm* comm, const long* in, long* out, size_t count, int operation)
  {
    if( comm ) 
      comm->allReduce(in,out,count,eckit::mpi::Operation::Code(operation));
    else
      eckit::mpi::comm().allReduce(in,out,count,eckit::mpi::Operation::Code(operation));
  }

  void fckit__mpi__allreduce_real32(const Comm* comm, const float* in, float* out, size_t count, int operation)
  {
    if( comm ) 
      comm->allReduce(in,out,count,eckit::mpi::Operation::Code(operation));
    else
      eckit::mpi::comm().allReduce(in,out,count,eckit::mpi::Operation::Code(operation));
  }
  
  void fckit__mpi__allreduce_real64(const Comm* comm, const double* in, double* out, size_t count, int operation)
  {
    if( comm ) 
      comm->allReduce(in,out,count,eckit::mpi::Operation::Code(operation));
    else
      eckit::mpi::comm().allReduce(in,out,count,eckit::mpi::Operation::Code(operation));
  }
  
  void fckit__mpi__allreduce_inplace_int32(const Comm* comm, int* inout, size_t count, int operation)
  {
    if( comm ) 
      comm->allReduceInPlace(inout,count,eckit::mpi::Operation::Code(operation));
    else
      eckit::mpi::comm().allReduceInPlace(inout,count,eckit::mpi::Operation::Code(operation));
  }

  void fckit__mpi__allreduce_inplace_int64(const Comm* comm, long* inout, size_t count, int operation)
  {
    if( comm ) 
      comm->allReduceInPlace(inout,count,eckit::mpi::Operation::Code(operation));
    else
      eckit::mpi::comm().allReduceInPlace(inout,count,eckit::mpi::Operation::Code(operation));
  }

  void fckit__mpi__allreduce_inplace_real32(const Comm* comm, float* inout, size_t count, int operation)
  {
    if( comm ) 
      comm->allReduceInPlace(inout,count,eckit::mpi::Operation::Code(operation));
    else
      eckit::mpi::comm().allReduceInPlace(inout,count,eckit::mpi::Operation::Code(operation));
  }
  
  void fckit__mpi__allreduce_inplace_real64(const Comm* comm, double* inout, size_t count, int operation)
  {
    if( comm ) 
      comm->allReduceInPlace(inout,count,eckit::mpi::Operation::Code(operation));
    else
      eckit::mpi::comm().allReduceInPlace(inout,count,eckit::mpi::Operation::Code(operation));
  }

  void fckit__mpi__broadcast_int32(const Comm* comm, int* buffer, size_t count, size_t root)
  {
    if( comm ) 
      comm->broadcast(buffer,count,root);
    else
      eckit::mpi::comm().broadcast(buffer,count,root);
  }

  void fckit__mpi__broadcast_int64(const Comm* comm, long* buffer, size_t count, size_t root)
  {
    if( comm ) 
      comm->broadcast(buffer,count,root);
    else
      eckit::mpi::comm().broadcast(buffer,count,root);
  }

  void fckit__mpi__broadcast_real32(const Comm* comm, float* buffer, size_t count, size_t root)
  {
    if( comm ) 
      comm->broadcast(buffer,count,root);
    else
      eckit::mpi::comm().broadcast(buffer,count,root);
  }

  void fckit__mpi__broadcast_real64(const Comm* comm, double* buffer, size_t count, size_t root)
  {
    if( comm ) 
      comm->broadcast(buffer,count,root);
    else
      eckit::mpi::comm().broadcast(buffer,count,root);
  }
  
  void fckit__mpi__send_real64(const Comm* comm, double* buffer, size_t count, int dest, int tag)
  {
    if( comm )
      comm->send(buffer,count,dest,tag);
    else
      eckit::mpi::comm().send(buffer,count,dest,tag);
  }

  void fckit__mpi__receive_real64(const Comm* comm, double* buffer, size_t count, int source, int tag, int* status)
  {
    eckit::mpi::Status _status;
    if( comm )
      _status = comm->receive(buffer,count,source,tag);
    else
      _status = eckit::mpi::comm().receive(buffer,count,source,tag);
    status[0] = _status.source();
    status[1] = _status.tag();
    status[2] = _status.error();
  }


}

