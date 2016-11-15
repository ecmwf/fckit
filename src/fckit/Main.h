#ifndef fckit_runtime_Main_h
#define fckit_runtime_Main_h

#include "eckit/runtime/Main.h"

namespace fckit{

class Main : public eckit::Main {
public:

  Main(
      int argc, char **argv,
      const char* homeenv = 0);

  static void initialise( int argc, char** argv, const char* homeenv = 0);
  static void initialize( int argc, char** argv, const char* homeenv = 0){
    initialise(argc,argv,homeenv);
  }

  static void finalise();
  static void finalize() {
    finalise();
  }

  virtual const std::string& displayName() const;

};

} // namespace fckit

#endif
