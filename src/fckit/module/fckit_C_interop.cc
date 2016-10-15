#include <stdint.h>
#include <stdlib.h>

extern "C"
{
  void fckit__cptr_free( void* ptr[] )
  {
    delete[] ptr;
    ptr=0;
  }

  int fckit__compare_cptr_equal( void* p1, void* p2 )
  {
    return (p1 == p2);
  }

  long fckit__cptr_to_loc( void* ptr )
  {
    intptr_t i = (intptr_t)ptr;
    return i;
  }
}
