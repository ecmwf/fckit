#include "fckit/Log.h"
#include "eckit/log/TimeStampTarget.h"
#include "eckit/log/PrefixTarget.h"
#include "eckit/log/CallbackTarget.h"
#include "eckit/log/FileTarget.h"
#include "eckit/log/OStreamTarget.h"

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

class FortranUnitTarget: public eckit::CallbackTarget {
public:
  FortranUnitTarget(int unit);
private:
  int unit_;
};

FortranUnitTarget::FortranUnitTarget(int unit) :
    eckit::CallbackTarget(&write_to_fortran_unit,&unit_),
    unit_(unit) {}

eckit::LogTarget* createStyleTarget( eckit::LogTarget* target, Log::Style style, const char* prefix )
{
  if( style == Log::SIMPLE    ) return target;
  if( style == Log::PREFIX    ) return new eckit::PrefixTarget( prefix, target ); 
  if( style == Log::TIMESTAMP ) return new eckit::TimeStampTarget( prefix, target );
  NOTIMP;
  return 0;
}

void Log::addFortranUnit(int unit, Style style, const char*) {
  eckit::LogTarget* funit = new FortranUnitTarget(unit);
  info().    addTarget( createStyleTarget(funit,style,"(I)") );
  warning(). addTarget( createStyleTarget(funit,style,"(W)") );
  error().   addTarget( createStyleTarget(funit,style,"(E)") );
  if (debug()) {
    debug().   addTarget( createStyleTarget(funit,style,"(D)") );
  }
}
void Log::setFortranUnit(int unit, Style style, const char*) {
  eckit::LogTarget* funit = new FortranUnitTarget(unit);
  info().    setTarget( createStyleTarget(funit,style,"(I)") );
  warning(). setTarget( createStyleTarget(funit,style,"(W)") );
  error().   setTarget( createStyleTarget(funit,style,"(E)") );
  if (debug()) {
    debug(). setTarget( createStyleTarget(funit,style,"(D)") );
  }
}

void Log::addFile(const char* path, Style style, const char*) {
  eckit::LogTarget* file = new eckit::FileTarget(path);
  info().    addTarget( createStyleTarget(file,style,"(I)") );
  warning(). addTarget( createStyleTarget(file,style,"(W)") );
  error().   addTarget( createStyleTarget(file,style,"(E)") );
  if (debug()) {
    debug(). addTarget( createStyleTarget(file,style,"(D)") );
  }
}
void Log::setFile(const char* path, Style style, const char*) {
  eckit::LogTarget* file = new eckit::FileTarget(path);
  info().    setTarget( createStyleTarget(file,style,"(I)") );
  warning(). setTarget( createStyleTarget(file,style,"(W)") );
  error().   setTarget( createStyleTarget(file,style,"(E)") );
  if (debug()) {
    debug(). setTarget( createStyleTarget(file,style,"(D)") );
  }
}

void Log::addStdOut(Style style, const char*) {
  eckit::LogTarget* stdout = new eckit::OStreamTarget(std::cout);
  info().    addTarget( createStyleTarget(stdout,style,"(I)") );
  warning(). addTarget( createStyleTarget(stdout,style,"(W)") );
  error().   addTarget( createStyleTarget(stdout,style,"(E)") );
  if (debug()) {
    debug(). addTarget( createStyleTarget(stdout,style,"(D)") );
  }
}

void Log::setStdOut(Style style, const char*) {
  eckit::LogTarget* stdout = new eckit::OStreamTarget(std::cout);
  info().    setTarget( createStyleTarget(stdout,style,"(I)") );
  warning(). setTarget( createStyleTarget(stdout,style,"(W)") );
  error().   setTarget( createStyleTarget(stdout,style,"(E)") );
  if (debug()) {
    debug(). setTarget( createStyleTarget(stdout,style,"(D)") );
  }
}

int Log::output_unit() {
  return fckit_fortranunit_stdout();
}
int Log::error_unit() {
  return fckit_fortranunit_stderr();
}

} // namespace fckit
