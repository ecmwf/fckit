/*
 * (C) Copyright 2013 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include <cstdint>
#include <cstring>
#include <sstream>

#include "fckit/fckit.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/log/CodeLocation.h"
#include "eckit/mpi/Comm.h"
#if FCKIT_HAVE_ECKIT_MPI_PARALLEL
#include "eckit/mpi/Parallel.h"
// Note that this also exports "mpi.h" transitively
#endif

using eckit::mpi::Comm;
using int32  = std::int32_t;
using int64  = std::int64_t;
using size_t = std::size_t;

namespace {
static void assert_int64_support( const eckit::CodeLocation& here ) {
    if ( sizeof( long ) != sizeof( int64 ) ) {
        std::stringstream msg;
        msg << "MPI support for int64 on 32bit architectures currently not "
               "implemented in eckit. "
            << eckit::newl
            << "Please check eckit issue ECKIT-349 to see if this check can be "
               "removed.";
        throw eckit::NotImplemented( msg.str(), here );
    }
}
}  // namespace

extern "C" {

const Comm* fckit__mpi__comm_default() {
    return &eckit::mpi::comm();
}

const Comm* fckit__mpi__comm( const char* name ) {
    return &eckit::mpi::comm( name );
}

const Comm* fckit__mpi__comm_wrap( int32 comm ) {
    std::ostringstream s;
    s << "fort." << comm;
    std::string name = s.str();
    if ( not eckit::mpi::hasComm( name.c_str() ) ) {
        eckit::mpi::addComm( name.c_str(), comm );
    }
    return &eckit::mpi::comm( name.c_str() );
}

void fckit__mpi__addComm_int( int32 comm ) {
    std::ostringstream s;
    s << "fort." << comm;
    std::string name = s.str();
    eckit::mpi::addComm( name.c_str(), comm );
}

void fckit__mpi__addComm_name_int( const char* name, int32 comm ) {
    eckit::mpi::addComm( name, comm );
}

void fckit__mpi__setCommDefault_int( int32 comm ) {
    std::ostringstream s;
    s << "fort." << comm;
    std::string name = s.str();
    if ( not eckit::mpi::hasComm( name.c_str() ) ) {
        eckit::mpi::addComm( name.c_str(), comm );
    }
    eckit::mpi::setCommDefault( name.c_str() );
}

void fckit__mpi__setCommDefault_name( const char* name ) {
    eckit::mpi::setCommDefault( name );
}

void fckit__mpi__comm_set_default( Comm* comm ) {
    eckit::mpi::setCommDefault( comm->name().c_str() );
}

void fckit__mpi__comm_name( Comm* comm, char*& name, int32& size ) {
    std::string s = ( comm ? comm->name() : eckit::mpi::comm().name() );
    size          = int32( s.size() ) + 1;
    name          = new char[size];
    std::strcpy( name, s.c_str() );
}

void fckit__mpi__comm_delete( Comm* comm ) {
    if ( comm ) {
        eckit::mpi::deleteComm( comm->name().c_str() );
    }
}

Comm* fckit__mpi__comm_split( Comm* comm, int32 colour, const char* name ) {
    if ( comm )
        return &( comm->split( colour, name ) );
    else
        return &( eckit::mpi::comm().split( colour, name ) );
}

int fckit__mpi__comm_communicator( const Comm* comm ) {
    if ( comm )
        return comm->communicator();
    else
        return eckit::mpi::comm().communicator();
}

int fckit__mpi__size( const Comm* comm ) {
    if ( comm )
        return comm->size();
    else
        return eckit::mpi::comm().size();
}

int32 fckit__mpi__rank( const Comm* comm ) {
    if ( comm )
        return comm->rank();
    else
        return eckit::mpi::comm().rank();
}

void fckit__mpi__barrier( const Comm* comm ) {
    if ( comm )
        return comm->barrier();
    else
        return eckit::mpi::comm().barrier();
}

void fckit__mpi__abort( const Comm* comm, int32 error_code ) {
    if ( comm )
        return comm->abort( error_code );
    else
        return eckit::mpi::comm().abort( error_code );
}

int32 fckit__mpi__sum() {
    return int( eckit::mpi::sum() );
}
int32 fckit__mpi__prod() {
    return int( eckit::mpi::prod() );
}
int32 fckit__mpi__max() {
    return int( eckit::mpi::max() );
}
int32 fckit__mpi__min() {
    return int( eckit::mpi::min() );
}
int32 fckit__mpi__maxloc() {
    return int( eckit::mpi::maxloc() );
}
int32 fckit__mpi__minloc() {
    return int( eckit::mpi::minloc() );
}

void fckit__mpi__allreduce_int32( const Comm* comm, const int32* in, int32* out, size_t count, int32 operation ) {
    if ( comm )
        comm->allReduce( in, out, count, eckit::mpi::Operation::Code( operation ) );
    else
        eckit::mpi::comm().allReduce( in, out, count, eckit::mpi::Operation::Code( operation ) );
}

void fckit__mpi__allreduce_int64( const Comm* comm, const int64* in, int64* out, size_t count, int32 operation ) {
    // TODO: Use int64 when ECKIT-349 is finished
    assert_int64_support( Here() );
    if ( comm )
        comm->allReduce( (long*)in, (long*)out, count, eckit::mpi::Operation::Code( operation ) );
    else
        eckit::mpi::comm().allReduce( (long*)in, (long*)out, count, eckit::mpi::Operation::Code( operation ) );
}

void fckit__mpi__allreduce_real32( const Comm* comm, const float* in, float* out, size_t count, int32 operation ) {
    if ( comm )
        comm->allReduce( in, out, count, eckit::mpi::Operation::Code( operation ) );
    else
        eckit::mpi::comm().allReduce( in, out, count, eckit::mpi::Operation::Code( operation ) );
}

void fckit__mpi__allreduce_real64( const Comm* comm, const double* in, double* out, size_t count, int32 operation ) {
    if ( comm )
        comm->allReduce( in, out, count, eckit::mpi::Operation::Code( operation ) );
    else
        eckit::mpi::comm().allReduce( in, out, count, eckit::mpi::Operation::Code( operation ) );
}

void fckit__mpi__allreduce_logical( const Comm* comm, const int32* in, int32* out, size_t count, int32 operation ) {
    if ( comm )
        comm->allReduce( in, out, count, eckit::mpi::Operation::Code( operation ) );
    else
        eckit::mpi::comm().allReduce( in, out, count, eckit::mpi::Operation::Code( operation ) );
}

void fckit__mpi__allreduce_inplace_int32( const Comm* comm, int32* inout, size_t count, int32 operation ) {
    if ( comm )
        comm->allReduceInPlace( inout, count, eckit::mpi::Operation::Code( operation ) );
    else
        eckit::mpi::comm().allReduceInPlace( inout, count, eckit::mpi::Operation::Code( operation ) );
}

void fckit__mpi__allreduce_inplace_int64( const Comm* comm, int64* inout, size_t count, int32 operation ) {
    // TODO: Use int64 when ECKIT-349 is finished
    assert_int64_support( Here() );
    if ( comm )
        comm->allReduceInPlace( (long*)inout, count, eckit::mpi::Operation::Code( operation ) );
    else
        eckit::mpi::comm().allReduceInPlace( (long*)inout, count, eckit::mpi::Operation::Code( operation ) );
}

void fckit__mpi__allreduce_inplace_real32( const Comm* comm, float* inout, size_t count, int32 operation ) {
    if ( comm )
        comm->allReduceInPlace( inout, count, eckit::mpi::Operation::Code( operation ) );
    else
        eckit::mpi::comm().allReduceInPlace( inout, count, eckit::mpi::Operation::Code( operation ) );
}

void fckit__mpi__allreduce_inplace_real64( const Comm* comm, double* inout, size_t count, int32 operation ) {
    if ( comm )
        comm->allReduceInPlace( inout, count, eckit::mpi::Operation::Code( operation ) );
    else
        eckit::mpi::comm().allReduceInPlace( inout, count, eckit::mpi::Operation::Code( operation ) );
}

void fckit__mpi__allreduce_inplace_logical( const Comm* comm, int32* inout, size_t count, int32 operation ) {
    if ( comm )
        comm->allReduceInPlace( inout, count, eckit::mpi::Operation::Code( operation ) );
    else
        eckit::mpi::comm().allReduceInPlace( inout, count, eckit::mpi::Operation::Code( operation ) );
}

void fckit__mpi__allgather_int32( const Comm* comm, const int32* in, int32* out ) {
    if ( comm )
        comm->allGather( *in, out, out + comm->size() );
    else
        eckit::mpi::comm().allGather( *in, out, out + eckit::mpi::comm().size() );
}

void fckit__mpi__allgather_int64( const Comm* comm, const int64* in, int64* out ) {
    if ( comm )
        comm->allGather( *in, out, out + comm->size() );
    else
        eckit::mpi::comm().allGather( *in, out, out + eckit::mpi::comm().size() );
}

void fckit__mpi__allgather_real32( const Comm* comm, const float* in, float* out ) {
    if ( comm )
        comm->allGather( *in, out, out + comm->size() );
    else
        eckit::mpi::comm().allGather( *in, out, out + eckit::mpi::comm().size() );
}

void fckit__mpi__allgather_real64( const Comm* comm, const double* in, double* out ) {
    if ( comm )
        comm->allGather( *in, out, out + comm->size() );
    else
        eckit::mpi::comm().allGather( *in, out, out + eckit::mpi::comm().size() );
}

void fckit__mpi__allgather_logical( const Comm* comm, const int32* in, int32* out ) {
    if ( comm )
        comm->allGather( *in, out, out + comm->size() );
    else
        eckit::mpi::comm().allGather( *in, out, out + eckit::mpi::comm().size() );
}

void fckit__mpi__allgatherv_int32( const Comm* comm, const int32* in, int32* out, size_t sendcount, int32* recvcounts,
                                   int32* displs ) {
    if ( comm )
        comm->allGatherv( in, in + sendcount, out, recvcounts, displs );
    else
        eckit::mpi::comm().allGatherv( in, in + sendcount, out, recvcounts, displs );
}

void fckit__mpi__allgatherv_int64( const Comm* comm, const int64* in, int64* out, size_t sendcount, int32* recvcounts,
                                   int32* displs ) {
    if ( comm )
        comm->allGatherv( in, in + sendcount, out, recvcounts, displs );
    else
        eckit::mpi::comm().allGatherv( in, in + sendcount, out, recvcounts, displs );
}

void fckit__mpi__allgatherv_real32( const Comm* comm, const float* in, float* out, size_t sendcount, int32* recvcounts,
                                    int32* displs ) {
    if ( comm )
        comm->allGatherv( in, in + sendcount, out, recvcounts, displs );
    else
        eckit::mpi::comm().allGatherv( in, in + sendcount, out, recvcounts, displs );
}

void fckit__mpi__allgatherv_real64( const Comm* comm, const double* in, double* out, size_t sendcount,
                                    int32* recvcounts, int32* displs ) {
    if ( comm )
        comm->allGatherv( in, in + sendcount, out, recvcounts, displs );
    else
        eckit::mpi::comm().allGatherv( in, in + sendcount, out, recvcounts, displs );
}

void fckit__mpi__allgatherv_logical( const Comm* comm, const int32* in, int32* out, size_t sendcount, int32* recvcounts,
                                     int32* displs ) {
    if ( comm )
        comm->allGatherv( in, in + sendcount, out, recvcounts, displs );
    else
        eckit::mpi::comm().allGatherv( in, in + sendcount, out, recvcounts, displs );
}

void fckit__mpi__broadcast_int32( const Comm* comm, int32* buffer, size_t count, size_t root ) {
    if ( comm )
        comm->broadcast( buffer, count, root );
    else
        eckit::mpi::comm().broadcast( buffer, count, root );
}

void fckit__mpi__broadcast_int64( const Comm* comm, int64* buffer, size_t count, size_t root ) {
    // TODO: Use int64 when ECKIT-349 is finished
    assert_int64_support( Here() );
    if ( comm )
        comm->broadcast( (long*)buffer, count, root );
    else
        eckit::mpi::comm().broadcast( (long*)buffer, count, root );
}

void fckit__mpi__broadcast_real32( const Comm* comm, float* buffer, size_t count, size_t root ) {
    if ( comm )
        comm->broadcast( buffer, count, root );
    else
        eckit::mpi::comm().broadcast( buffer, count, root );
}

void fckit__mpi__broadcast_real64( const Comm* comm, double* buffer, size_t count, size_t root ) {
    if ( comm )
        comm->broadcast( buffer, count, root );
    else
        eckit::mpi::comm().broadcast( buffer, count, root );
}

void fckit__mpi__broadcast_logical( const Comm* comm, int32* buffer, size_t count, size_t root ) {
    if ( comm )
        comm->broadcast( buffer, count, root );
    else
        eckit::mpi::comm().broadcast( buffer, count, root );
}

void fckit__mpi__broadcast_string( const Comm* comm, char* buffer, size_t count, size_t root ) {
    if ( comm )
        comm->broadcast( buffer, buffer + count, root );
    else
        eckit::mpi::comm().broadcast( buffer, buffer + count, root );
}

static eckit::CountedBuffer* extract_buffer( const eckit::SharedBuffer& cb ) {
    eckit::CountedBuffer* buf = const_cast<eckit::SharedBuffer&>( cb ).operator->();
    buf->attach();
    return buf;
}

eckit::CountedBuffer* fckit__mpi__broadcast_file( const Comm* comm, const char* path, size_t root ) {
    if ( comm )
        return extract_buffer( comm->broadcastFile( path, root ) );
    else
        return extract_buffer( eckit::mpi::comm().broadcastFile( path, root ) );
}

int fckit__mpi__anytag( const Comm* comm ) {
    if ( comm )
        return comm->anyTag();
    else
        return eckit::mpi::comm().anyTag();
}

int fckit__mpi__anysource( const Comm* comm ) {
    if ( comm )
        return comm->anySource();
    else
        return eckit::mpi::comm().anySource();
}

void fckit__mpi__send_int32( const Comm* comm, int32* buffer, size_t count, int32 dest, int32 tag ) {
    if ( comm )
        comm->send( buffer, count, dest, tag );
    else
        eckit::mpi::comm().send( buffer, count, dest, tag );
}

void fckit__mpi__send_int64( const Comm* comm, int64* buffer, size_t count, int32 dest, int32 tag ) {
    // TODO: Use int64 when ECKIT-349 is finished
    assert_int64_support( Here() );
    if ( comm )
        comm->send( (long*)buffer, count, dest, tag );
    else
        eckit::mpi::comm().send( (long*)buffer, count, dest, tag );
}

void fckit__mpi__send_real32( const Comm* comm, float* buffer, size_t count, int32 dest, int32 tag ) {
    if ( comm )
        comm->send( buffer, count, dest, tag );
    else
        eckit::mpi::comm().send( buffer, count, dest, tag );
}

void fckit__mpi__send_real64( const Comm* comm, double* buffer, size_t count, int32 dest, int32 tag ) {
    if ( comm )
        comm->send( buffer, count, dest, tag );
    else
        eckit::mpi::comm().send( buffer, count, dest, tag );
}

void fckit__mpi__send_logical( const Comm* comm, int32* buffer, size_t count, int32 dest, int32 tag ) {
    if ( comm )
        comm->send( buffer, count, dest, tag );
    else
        eckit::mpi::comm().send( buffer, count, dest, tag );
}

void fckit__mpi__receive_int32( const Comm* comm, int32* buffer, size_t count, int32 source, int32 tag,
                                int32* status ) {
    eckit::mpi::Status _status;
    if ( comm )
        _status = comm->receive( buffer, count, source, tag );
    else
        _status = eckit::mpi::comm().receive( buffer, count, source, tag );
    status[0] = _status.source();
    status[1] = _status.tag();
    status[2] = _status.error();
}

void fckit__mpi__receive_int64( const Comm* comm, int64* buffer, size_t count, int32 source, int32 tag,
                                int32* status ) {
    // TODO: Use int64 when ECKIT-349 is finished
    assert_int64_support( Here() );
    eckit::mpi::Status _status;
    if ( comm )
        _status = comm->receive( (long*)buffer, count, source, tag );
    else
        _status = eckit::mpi::comm().receive( (long*)buffer, count, source, tag );
    status[0] = _status.source();
    status[1] = _status.tag();
    status[2] = _status.error();
}

void fckit__mpi__receive_real32( const Comm* comm, float* buffer, size_t count, int32 source, int32 tag,
                                 int32* status ) {
    eckit::mpi::Status _status;
    if ( comm )
        _status = comm->receive( buffer, count, source, tag );
    else
        _status = eckit::mpi::comm().receive( buffer, count, source, tag );
    status[0] = _status.source();
    status[1] = _status.tag();
    status[2] = _status.error();
}

void fckit__mpi__receive_real64( const Comm* comm, double* buffer, size_t count, int32 source, int32 tag,
                                 int32* status ) {
    eckit::mpi::Status _status;
    if ( comm )
        _status = comm->receive( buffer, count, source, tag );
    else
        _status = eckit::mpi::comm().receive( buffer, count, source, tag );
    status[0] = _status.source();
    status[1] = _status.tag();
    status[2] = _status.error();
}

void fckit__mpi__receive_logical( const Comm* comm, int32* buffer, size_t count, int32 source, int32 tag,
                                  int32* status ) {
    eckit::mpi::Status _status;
    if ( comm )
        _status = comm->receive( buffer, count, source, tag );
    else
        _status = eckit::mpi::comm().receive( buffer, count, source, tag );
    status[0] = _status.source();
    status[1] = _status.tag();
    status[2] = _status.error();
}

int fckit__mpi__isend_int32( const Comm* comm, int32* buffer, size_t count, int32 dest, int32 tag ) {
    if ( comm )
        return comm->iSend( buffer, count, dest, tag ).request();
    else
        return eckit::mpi::comm().iSend( buffer, count, dest, tag ).request();
}

int fckit__mpi__isend_int64( const Comm* comm, int64* buffer, size_t count, int32 dest, int32 tag ) {
    // TODO: Use int64 when ECKIT-349 is finished
    assert_int64_support( Here() );
    if ( comm )
        return comm->iSend( (long*)buffer, count, dest, tag ).request();
    else
        return eckit::mpi::comm().iSend( (long*)buffer, count, dest, tag ).request();
}

int fckit__mpi__isend_real32( const Comm* comm, float* buffer, size_t count, int32 dest, int32 tag ) {
    if ( comm )
        return comm->iSend( buffer, count, dest, tag ).request();
    else
        return eckit::mpi::comm().iSend( buffer, count, dest, tag ).request();
}

int fckit__mpi__isend_real64( const Comm* comm, double* buffer, size_t count, int32 dest, int32 tag ) {
    if ( comm )
        return comm->iSend( buffer, count, dest, tag ).request();
    else
        return eckit::mpi::comm().iSend( buffer, count, dest, tag ).request();
}

int fckit__mpi__isend_logical( const Comm* comm, int32* buffer, size_t count, int32 dest, int32 tag ) {
    if ( comm )
        return comm->iSend( buffer, count, dest, tag ).request();
    else
        return eckit::mpi::comm().iSend( buffer, count, dest, tag ).request();
}

int fckit__mpi__ireceive_int32( const Comm* comm, int32* buffer, size_t count, int32 source, int32 tag ) {
    if ( comm )
        return comm->iReceive( buffer, count, source, tag ).request();
    else
        return eckit::mpi::comm().iReceive( buffer, count, source, tag ).request();
}

int fckit__mpi__ireceive_int64( const Comm* comm, int64* buffer, size_t count, int32 source, int32 tag ) {
    // TODO: Use int64 when ECKIT-349 is finished
    assert_int64_support( Here() );
    if ( comm )
        return comm->iReceive( (long*)buffer, count, source, tag ).request();
    else
        return eckit::mpi::comm().iReceive( (long*)buffer, count, source, tag ).request();
}

int fckit__mpi__ireceive_real32( const Comm* comm, float* buffer, size_t count, int32 source, int32 tag ) {
    if ( comm )
        return comm->iReceive( buffer, count, source, tag ).request();
    else
        return eckit::mpi::comm().iReceive( buffer, count, source, tag ).request();
}

int fckit__mpi__ireceive_real64( const Comm* comm, double* buffer, size_t count, int32 source, int32 tag ) {
    if ( comm )
        return comm->iReceive( buffer, count, source, tag ).request();
    else
        return eckit::mpi::comm().iReceive( buffer, count, source, tag ).request();
}

int fckit__mpi__ireceive_logical( const Comm* comm, int32* buffer, size_t count, int32 source, int32 tag ) {
    if ( comm )
        return comm->iReceive( buffer, count, source, tag ).request();
    else
        return eckit::mpi::comm().iReceive( buffer, count, source, tag ).request();
}

void fckit__mpi__alltoallv_real32( const Comm* comm, const float* in, int32* scounts, int32* sdispl, float* out,
                                   int32* rcounts, int32* rdispl ) {
    if ( comm )
        comm->allToAllv( in, scounts, sdispl, out, rcounts, rdispl );
    else
        eckit::mpi::comm().allToAllv( in, scounts, sdispl, out, rcounts, rdispl );
}

void fckit__mpi__alltoallv_real64( const Comm* comm, const double* in, int32* scounts, int32* sdispl, double* out,
                                   int32* rcounts, int32* rdispl ) {
    if ( comm )
        comm->allToAllv( in, scounts, sdispl, out, rcounts, rdispl );
    else
        eckit::mpi::comm().allToAllv( in, scounts, sdispl, out, rcounts, rdispl );
}

void fckit__mpi__alltoallv_int64( const Comm* comm, const int64* in, int32* scounts, int32* sdispl, int64* out,
                                  int32* rcounts, int32* rdispl ) {
    if ( comm )
        comm->allToAllv( in, scounts, sdispl, out, rcounts, rdispl );
    else
        eckit::mpi::comm().allToAllv( in, scounts, sdispl, out, rcounts, rdispl );
}

void fckit__mpi__alltoallv_int32( const Comm* comm, const int32* in, int32* scounts, int32* sdispl, int32* out,
                                  int32* rcounts, int32* rdispl ) {
    if ( comm )
        comm->allToAllv( in, scounts, sdispl, out, rcounts, rdispl );
    else
        eckit::mpi::comm().allToAllv( in, scounts, sdispl, out, rcounts, rdispl );
}

void fckit__mpi__alltoallv_logical( const Comm* comm, const int32* in, int32* scounts, int32* sdispl, int32* out,
                                    int32* rcounts, int32* rdispl ) {
    if ( comm )
        comm->allToAllv( in, scounts, sdispl, out, rcounts, rdispl );
    else
        eckit::mpi::comm().allToAllv( in, scounts, sdispl, out, rcounts, rdispl );
}

void fckit__mpi__wait( const Comm* comm, int32 request, int32* status ) {
    eckit::mpi::Status _status;
    eckit::mpi::Request req( request );
    if ( comm )
        _status = comm->wait( req );
    else
        _status = eckit::mpi::comm().wait( req );
    status[0] = _status.source();
    status[1] = _status.tag();
    status[2] = _status.error();
}

int32 fckit__mpi__mpi_info_null() {
#if FCKIT_HAVE_ECKIT_MPI_PARALLEL
    int mpi_initialized;
    if ( MPI_Initialized( &mpi_initialized ) == MPI_SUCCESS ) {
        if ( mpi_initialized ) {
            return MPI_Info_c2f( MPI_INFO_NULL );
        }
    }
    // The case when eckit is compiled with MPI support, but using "serial" MPI backend
    return 0;
#else
    // The case when eckit is not compiled with MPI support
    return 0;
#endif
}

void fckit__mpi__finaliseAllComms() {
    eckit::mpi::finaliseAllComms();
}

}  // extern "C"
