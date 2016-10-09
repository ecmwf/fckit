#ifndef fckit_log_Log_h
#define fckit_log_Log_h

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
