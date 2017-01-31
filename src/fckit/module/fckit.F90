#include "fckit_defines.h"

module fckit_module
use fckit_main_module,     only: fckit_main
use fckit_log_module,      only: fckit_log
use fckit_resource_module, only: fckit_resource
use fckit_mpi_module,      only: fckit_mpi_comm
implicit none
private

public :: fckit_log
public :: fckit_version
public :: fckit_git_sha1
public :: fckit_main
public :: fckit_resource
public :: fckit_mpi_comm


! =============================================================================
CONTAINS
! =============================================================================

! -----------------------------------------------------------------------------

function fckit_version()
character(len=8) :: fckit_version
fckit_version = FCKIT_VERSION
end function

! -----------------------------------------------------------------------------

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

