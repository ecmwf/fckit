! (C) Copyright 2013 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.
#ifndef FORD
#include "fckit/fckit.h"
#endif

module fckit_module
  !! author: Willem Deconinck
  !!
  !! Interface to fckit, forwarding the most used types, variables, and functions
  !!

use fckit_main_module,      only: &
  fckit_main

use fckit_log_module,       only: &
  fckit_log, &
  log ! DEPRECATED. Use fckit_log instead

use fckit_resource_module,  only: &
  fckit_resource

use fckit_mpi_module,       only: &
  fckit_mpi_comm, &
  fckit_mpi

use fckit_exception_module, only: &
  fckit_exception, &
  fckit_exception_handler

use fckit_signal_module, only: &
  fckit_signal, &
  fckit_signal_handler

use fckit_pathname_module, only: &
  fckit_pathname

use fckit_configuration_module, only: &
  fckit_configuration, &
  fckit_YAMLConfiguration, &
  deallocate_fckit_configuration

use fckit_buffer_module, only: &
  fckit_buffer

use fckit_map_module, only: &
  fckit_map

use fckit_tensor_module, only: &
  fckit_tensor_real32, &
  fckit_tensor_real64

implicit none
private

public :: fckit_main                      !! - [[fckit_main_module:fckit_main(variable)]]
public :: fckit_log                       !! - [[fckit_log_module:fckit_log(variable)]]
public :: fckit_resource                  !! - [[fckit_resource_module:fckit_resource(interface)]]
public :: fckit_mpi_comm                  !! - [[fckit_mpi_module:fckit_mpi_comm(type)]]
public :: fckit_mpi                       !! - [[fckit_mpi_module:fckit_mpi(variable)]]
public :: fckit_exception                 !! - [[fckit_exception_module:fckit_exception(variable)]]
public :: fckit_exception_handler         !! - [[fckit_exception_module:fckit_exception_handler(interface)]]
public :: fckit_signal                    !! - [[fckit_signal_module:fckit_signal(variable)]]
public :: fckit_signal_handler            !! - [[fckit_signal_module:fckit_signal_handler(interface)]]
public :: fckit_pathname                  !! - [[fckit_pathname_module:fckit_pathname(type)]]
public :: fckit_configuration             !! - [[fckit_configuration_module:fckit_configuration(type)]]
public :: fckit_YAMLConfiguration         !! - [[fckit_configuration_module:fckit_YAMLConfiguration(interface)]]
public :: fckit_buffer                    !! - [[fckit_buffer_module:fckit_buffer(type)]]
public :: fckit_map                       !! - [[fckit_map_module:fckit_map(type)]]
public :: fckit_version                   !! - [[fckit_module:fckit_version(function)]]
public :: fckit_git_sha1                  !! - [[fckit_module:fckit_git_sha1(function)]]
public :: deallocate_fckit_configuration  !! - [[fckit_configuration_module:deallocate_fckit_configuration(subroutine)]]
public :: fckit_tensor_real32              !! - [[fckit_tensor_module:fckit_tensor_real32(type)]]
public :: fckit_tensor_real64             !! - [[fckit_tensor_module:fckit_tensor_real64(type)]]

public :: log ! DEPRECATED. Use fckit_log instead.

! =============================================================================
CONTAINS
! =============================================================================

! -----------------------------------------------------------------------------

function fckit_version()
  !! Function that returns the version of fckit

  use, intrinsic :: iso_c_binding, only : c_char
  character(kind=c_char,len=8) :: fckit_version
  fckit_version = FCKIT_VERSION
end function

! -----------------------------------------------------------------------------

function fckit_git_sha1(length) result( sha1 )
  !! Function that returns the git-sha1 of fckit, if compiled from git repository

  use, intrinsic :: iso_c_binding, only : c_char, c_int32_t
  character(kind=c_char,len=40) :: sha1
  integer(c_int32_t), optional :: length
    !! Truncate git sha1 to specified length. Default truncates to 7 chars.
  integer(c_int32_t)           :: opt_length
  opt_length = 7
  if( present(length) ) opt_length = length
  sha1 = FCKIT_GIT_SHA1
  sha1 = sha1(1:min(opt_length, 40))
end function

! -----------------------------------------------------------------------------

end module

