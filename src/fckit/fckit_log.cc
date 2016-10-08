#include "eckit/log/Log.h"
#include "eckit/log/OStreamTarget.h"
#include "fckit/fckit_log.h"
#include "fckit/fckit_runtime.h"

using eckit::Log;
using eckit::Channel;

extern "C" { void fckit_write_to_fortran_unit(int unit, const char* msg); }

namespace {

static void write_to_fortran_unit( void* ctxt, const char* msg ) {
  fckit_write_to_fortran_unit( *static_cast<int*>(ctxt), msg );
}

} // namespace

namespace fckit {

TimeStampFortranUnitTarget::TimeStampFortranUnitTarget(int unit, const char* tag) :
    eckit::TimeStampTarget( tag, new eckit::CallbackTarget(&write_to_fortran_unit,&unit_) ),
    unit_(unit) {}

PrefixFortranUnitTarget::PrefixFortranUnitTarget(int unit, const char* prefix) :
    eckit::PrefixTarget( prefix, new eckit::CallbackTarget(&write_to_fortran_unit,&unit_) ),
    unit_(unit) {}

SimpleFortranUnitTarget::SimpleFortranUnitTarget(int unit) :
    eckit::CallbackTarget(&write_to_fortran_unit,&unit_),
    unit_(unit) {}
    
eckit::LogTarget* createFortranUnitTarget(const char* name, int unit, const char* prefix)
{
  if( std::string(name) == "simple"    ) return new SimpleFortranUnitTarget(unit);
  if( std::string(name) == "prefix"    ) return new PrefixFortranUnitTarget(unit,prefix);
  if( std::string(name) == "timestamp" ) return new TimeStampFortranUnitTarget(unit,prefix);
  NOTIMP;
  return 0;
}
    

} // namespace fckit

extern "C" {

void fckit__log(Channel* channel, char *msg, int newl, int flush)
{
  *channel << msg;
  if( newl )
    *channel << eckit::newl;
  if( flush )
    *channel << std::flush;
}

void fckit__log_debug(char *msg, int newl, int flush)
{
  fckit__log( &Log::debug(), msg, newl, flush );
}

void fckit__log_info(char *msg, int newl, int flush)
{
  fckit__log( &Log::info(), msg, newl, flush );
}

void fckit__log_warning(char *msg, int newl, int flush)
{
  fckit__log( &Log::warning(), msg, newl, flush );
}

void fckit__log_error(char *msg, int newl, int flush)
{
  fckit__log( &Log::error(), msg, newl, flush );
}


void fckit__log_add_fortran_unit(int unit, const char* target)
{
    Log::info().    addTarget( fckit::createFortranUnitTarget(target,unit,"(I)") );
    Log::warning(). addTarget( fckit::createFortranUnitTarget(target,unit,"(W)") );
    Log::error().   addTarget( fckit::createFortranUnitTarget(target,unit,"(E)") );
    Log::debug().   addTarget( fckit::createFortranUnitTarget(target,unit,"(D)") );
}

void fckit__log_set_fortran_unit(int unit, const char* target)
{
    Log::info().    setTarget( fckit::createFortranUnitTarget(target,unit,"(I)") );
    Log::warning(). setTarget( fckit::createFortranUnitTarget(target,unit,"(W)") );
    Log::error().   setTarget( fckit::createFortranUnitTarget(target,unit,"(E)") );
    Log::debug().   setTarget( fckit::createFortranUnitTarget(target,unit,"(D)") );
}

void fckit__log_reset()
{
    Log::reset();
}


} // extern "C"
