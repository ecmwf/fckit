#include "eckit/memory/Owned.h"
extern "C" {

  int fckit__Owned__owners(const eckit::Owned* owned)
  {
    return owned->owners();
  }

  void fckit__Owned__attach(const eckit::Owned* owned)
  {
    owned->attach();
  }

  void fckit__Owned__detach(const eckit::Owned* owned)
  {
    owned->detach();
  }

  eckit::Owned* fckit__new_Owned()
  {
    eckit::Owned* owned = new eckit::Owned();
    return owned;
  }

  void fckit__delete_Owned(eckit::Owned* owned)
  {
    delete owned;
    owned = 0;
  }

}



