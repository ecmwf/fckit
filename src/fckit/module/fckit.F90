#include "fckit_defines.h"

module fckit_module
implicit none

public


! =============================================================================
CONTAINS
! =============================================================================

function fckit_version()
character(len=8) :: fckit_version
fckit_version = FCKIT_VERSION
end function

function fckit_git_sha1(length) result( sha1 )
character(len=40)        :: sha1
integer, optional :: length
integer           :: opt_length
opt_length = 7
if( present(length) ) opt_length = length
sha1 = FCKIT_GIT_SHA1
sha1 = sha1(1:min(opt_length, 40))
end function


! -----------------------------------------------------------------------------
end module

