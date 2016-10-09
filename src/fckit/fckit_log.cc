#include "eckit/log/Channel.h"
#include "fckit/Log.h"

using fckit::Log;
using eckit::Channel;

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
  std::map<std::string,Log::Style> style;
  style["simple"]=Log::SIMPLE;
  style["prefix"]=Log::PREFIX;
  style["timestamp"]=Log::TIMESTAMP;
  Log::addFortranUnit(unit,style[target]);
}

void fckit__log_set_fortran_unit(int unit, const char* target)
{
  std::map<std::string,Log::Style> style;
  style["simple"]=Log::SIMPLE;
  style["prefix"]=Log::PREFIX;
  style["timestamp"]=Log::TIMESTAMP;
  Log::setFortranUnit(unit,style[target]);
}

void fckit__log_reset()
{
    Log::reset();
}


} // extern "C"
