#include "eckit/log/Log.h"
#include <map>

using eckit::Log;
using eckit::Channel;

extern "C" { void fckit_write_to_fortran_unit(int unit, const char* msg); }

namespace {

class FortranUnit {
public:

  FortranUnit(int unit)
  {
    unit_ = unit;
  }

  int unit() const { return unit_; }

private:
  int unit_;
};

static void write_to_fortran_unit( void* ctxt, const char* msg ) {
  fckit_write_to_fortran_unit( *static_cast<int*>(ctxt), msg );
}

class FortranUnits {
public:

    typedef std::map<int,FortranUnit*> Units;

    static FortranUnits& instance() {
        static FortranUnits instance_;
        return instance_;
    }
    
    FortranUnit& create(int unit)
    {
        if( units_.find(unit) == units_.end() )
        {
            units_[unit] = new FortranUnit(unit);
        }
        return *units_[unit];
    }

private:

    FortranUnits() {}

    ~FortranUnits() {
        for(Units::iterator itr = units_.begin() ; itr != units_.end() ; ++itr) {
            delete itr->second;
        }
    }
    std::map<int,FortranUnit*> units_;
};

} // namespace

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

void fckit__log_add_fortran_unit(int unit)
{
  FortranUnit& funit = FortranUnits::instance().create(unit);
  eckit::Log::addCallback(&write_to_fortran_unit,&funit);
}

void fckit__log_set_fortran_unit(int unit)
{
  FortranUnit& funit = FortranUnits::instance().create(unit);
  eckit::Log::setCallback(&write_to_fortran_unit,&funit);
}

} // extern "C"
