#include "eckit/log/Log.h"
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


FortranUnitTarget::FortranUnitTarget(int unit) :
    eckit::CallbackTarget(&write_to_fortran_unit,&unit_),
    unit_(unit) {}

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

static int ALL_TASKS=-1;

void fckit__log_add_fortran_unit(int unit, int output_task)
{
  NOTIMP;
}

void fckit__log_set_fortran_unit(int unit, int output_task)
{
// TODO: make nicer
  if( fckit::Main::ready()) {
          if( output_task == ALL_TASKS ||
            output_task == fckit::Main::instance().taskID() ) {
              if( fckit::Main::instance().logSimple() ) {
                  Log::info().setTarget( new fckit::FortranUnitTarget(unit) );
                  Log::warning().setTarget( new fckit::FortranUnitTarget(unit) );
                  Log::error().setTarget( new fckit::FortranUnitTarget(unit) );
                  Log::debug().setTarget( new fckit::FortranUnitTarget(unit) );
              } else {
                  Log::info().setTarget( new fckit::TimeStampFortranUnitTarget(unit,"(I fort)") );
                  Log::warning().setTarget( new fckit::TimeStampFortranUnitTarget(unit,"(W fort)") );
                  Log::error().setTarget( new fckit::TimeStampFortranUnitTarget(unit,"(E fort)") );
                  Log::debug().setTarget( new fckit::TimeStampFortranUnitTarget(unit,"(D fort)") );
              }
          }
  } else {
      Log::info().setTarget( new fckit::FortranUnitTarget(unit) );
      Log::warning().setTarget( new fckit::FortranUnitTarget(unit) );
      Log::error().setTarget( new fckit::FortranUnitTarget(unit) );
      Log::debug().setTarget( new fckit::FortranUnitTarget(unit) );
  }
}

} // extern "C"
