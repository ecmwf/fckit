#include "eckit/config/LocalConfiguration.h"

using namespace eckit;

// Fortran binding
extern "C" {
  int c_get_a(const Configuration*);
}

int get_a(const Configuration& conf) {
  return c_get_a(&conf);
}

int main( int argc, char **argv ) {
  LocalConfiguration conf;
  conf.set("a",10l);
  
  int a = get_a(conf);

  if( a != 10 ) {
    std::cout << "a != 10" << std::endl;
    return 1;
  }
  return 0;
}