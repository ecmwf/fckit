#ifndef fckit_log_h
#define fckit_log_h

#include "eckit/log/TimeStampTarget.h"
#include "eckit/log/CallbackTarget.h"

namespace fckit {

class TimeStampFortranUnitTarget: public eckit::TimeStampTarget {
public:
  TimeStampFortranUnitTarget(int unit, const char* tag = "");
private:
  int unit_;
};


class FortranUnitTarget: public eckit::CallbackTarget {
public:
  FortranUnitTarget(int unit);
private:
  int unit_;
};


} // namespace fckit
#endif
