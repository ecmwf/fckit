#ifndef fckit_runtime_h
#define fckit_runtime_h

#include "eckit/runtime/Main.h"

namespace fckit{

class Main : public eckit::Main {
public:

  Main(
      int argc, char **argv,
      int task,
      int output_task,
      int output_unit,
      int error_unit,
      int log_simple,
      const char* homeenv = 0);

  static void initialise(
      int argc, char** argv,
      int task,
      int output_task,
      int output_unit,
      int error_unit,
      int log_simple,
      const char* homeenv = 0);

  static Main& instance() {
      return dynamic_cast<Main&>( eckit::Main::instance() );
  }

  virtual eckit::LogTarget* createInfoLogTarget()    const;
  virtual eckit::LogTarget* createWarningLogTarget() const;
  virtual eckit::LogTarget* createErrorLogTarget()   const;
  virtual eckit::LogTarget* createDebugLogTarget()   const;

  virtual int outputTask() const {
    return outputTask_;
  }

  virtual bool logSimple() const {
    return logSimple_;
  }

private:
  int outputTask_;
  int outputUnit_;
  int errorUnit_;
  int logSimple_;
};

} // namespace fckit

#endif
