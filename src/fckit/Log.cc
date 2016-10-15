#include "fckit/Log.h"
#include "eckit/log/TimeStampTarget.h"
#include "eckit/log/PrefixTarget.h"
#include "eckit/log/CallbackTarget.h"

using fckit::Log;
using eckit::Channel;

extern "C" {
  void fckit_write_to_fortran_unit(int unit, const char* msg);
  int fckit_fortranunit_stdout();
  int fckit_fortranunit_stderr();
}

namespace {

static void write_to_fortran_unit( void* ctxt, const char* msg ) {
  fckit_write_to_fortran_unit( *static_cast<int*>(ctxt), msg );
}

} // namespace

namespace fckit {

class TimeStampFortranUnitTarget: public eckit::TimeStampTarget {
public:
  TimeStampFortranUnitTarget(int unit, const char* tag = "");
private:
  int unit_;
};

class SimpleFortranUnitTarget: public eckit::CallbackTarget {
public:
  SimpleFortranUnitTarget(int unit);
private:
  int unit_;
};

class PrefixFortranUnitTarget: public eckit::PrefixTarget {
public:
  PrefixFortranUnitTarget(int unit,const char* prefix = "");
private:
  int unit_;
};

TimeStampFortranUnitTarget::TimeStampFortranUnitTarget(int unit, const char* tag) :
    eckit::TimeStampTarget( tag, new eckit::CallbackTarget(&write_to_fortran_unit,&unit_) ),
    unit_(unit) {}

PrefixFortranUnitTarget::PrefixFortranUnitTarget(int unit, const char* prefix) :
    eckit::PrefixTarget( prefix, new eckit::CallbackTarget(&write_to_fortran_unit,&unit_) ),
    unit_(unit) {}

SimpleFortranUnitTarget::SimpleFortranUnitTarget(int unit) :
    eckit::CallbackTarget(&write_to_fortran_unit,&unit_),
    unit_(unit) {}

eckit::LogTarget* createFortranUnitTarget(int unit, Log::Style style, const char* prefix)
{
  if( style == Log::SIMPLE    ) return new SimpleFortranUnitTarget(unit);
  if( style == Log::PREFIX    ) return new PrefixFortranUnitTarget(unit,prefix);
  if( style == Log::TIMESTAMP ) return new TimeStampFortranUnitTarget(unit,prefix);
  NOTIMP;
  return 0;
}

void Log::addFortranUnit(int unit, Style style, const char*) {
  info().    addTarget(createFortranUnitTarget(unit,style,"(I)"));
  warning(). addTarget(createFortranUnitTarget(unit,style,"(W)"));
  error().   addTarget(createFortranUnitTarget(unit,style,"(E)"));
  if (debug()) {
    debug().   addTarget(createFortranUnitTarget(unit,style,"(D)"));
  }
}
void Log::setFortranUnit(int unit, Style style, const char*) {
  info().    setTarget(createFortranUnitTarget(unit,style,"(I)"));
  warning(). setTarget(createFortranUnitTarget(unit,style,"(W)"));
  error().   setTarget(createFortranUnitTarget(unit,style,"(E)"));
  if (debug()) {
    debug().   setTarget(createFortranUnitTarget(unit,style,"(D)"));
  }
}

int Log::output_unit() {
  return fckit_fortranunit_stdout();
}
int Log::error_unit() {
  return fckit_fortranunit_stderr();
}

} // namespace fckit
