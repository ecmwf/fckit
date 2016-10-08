#ifndef fckit_runtime_h
#define fckit_runtime_h

#include "eckit/runtime/Main.h"

namespace fckit{

class Main : public eckit::Main {
public:

  Main(
      int argc, char **argv,
      const char* homeenv = 0);

  static void initialise(
      int argc, char** argv,
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

  std::string logTarget() const {
    return "prefix";
  }

private:
  int outputTask_;
  int outputUnit_;
};

} // namespace fckit

#endif
