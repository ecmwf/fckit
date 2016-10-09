#ifndef fckit_log_h
#define fckit_log_h

#include "eckit/log/TimeStampTarget.h"
#include "eckit/log/PrefixTarget.h"
#include "eckit/log/CallbackTarget.h"
#include "eckit/log/Log.h"

namespace fckit {

class Log : public eckit::Log {
public:
  enum Style {
    SIMPLE=0,PREFIX=1,TIMESTAMP=2
  };
  static void addFortranUnit(int unit, Style=PREFIX, const char* prefix="");
  static void setFortranUnit(int unit, Style=PREFIX, const char* prefix="");
};

} // namespace fckit

#endif
