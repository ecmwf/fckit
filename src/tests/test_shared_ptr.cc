#include <iostream>
#include <stdio.h>

static int destructor_called = 0;
static int destructor_called_after_scope = 0;
static int scope_ended = 0;

class Object {
public:
  Object(int i) : i_(i) { std::cout << "constructing Object " << i_ << std::endl; }
  ~Object() { 
    std::cout << "destructing Object " << i_ << std::endl;
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
