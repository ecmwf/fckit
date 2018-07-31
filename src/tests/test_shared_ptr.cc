#include <iostream>
#include <sstream>
#include <stdio.h>

static int destructor_called = 0;
static int destructor_called_after_scope = 0;
static int scope_ended = 0;

extern "C" {
  void fckit_write_to_fortran_unit(int unit, const char* msg);
  int fckit_fortranunit_stdout();
  int fckit_fortranunit_stderr();
}

class Object {
public:
  Object(int i) : i_(i) {
    std::stringstream out;
    out << "constructing Object " << i_ ;
    fckit_write_to_fortran_unit(fckit_fortranunit_stderr(),out.str().c_str());
  }
  ~Object() { 
    std::stringstream out;
    out << "destructing Object " << i_ ;
    fckit_write_to_fortran_unit(fckit_fortranunit_stderr(),out.str().c_str());
    destructor_called += 1;
    if( scope_ended )
      destructor_called_after_scope += 1;
  }
  int id() const { return i_; }
private:
  int i_;
};

extern "C" {
  Object* new_Object(int i)          { return new Object(i); }
  void delete_Object(Object* p)      { delete p; }
  int Object__id( const Object* p )  { return p->id(); }
  
  int cxx_destructor_called()             { return destructor_called; }
  int cxx_destructor_called_after_scope() { return destructor_called_after_scope; }
  void cxx_reset_counters() {
    destructor_called = 0; 
    destructor_called_after_scope = 0; 
    scope_ended = 0;
  }
  void cxx_end_scope() { scope_ended = 1; }
}
