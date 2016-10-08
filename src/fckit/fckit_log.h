#ifndef fckit_log_h
#define fckit_log_h

#include "eckit/log/TimeStampTarget.h"
#include "eckit/log/PrefixTarget.h"
#include "eckit/log/CallbackTarget.h"

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

eckit::LogTarget* createFortranUnitTarget(const char* name, int unit, const char* prefix="");

} // namespace fckit
#endif
