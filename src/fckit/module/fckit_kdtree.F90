! (C) Copyright 2019 UCAR
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 

module fckit_kdtree_module
  !! Fortran interface to eckit KDTree

use iso_c_binding, only: c_ptr
implicit none

type kdtree
  type(c_ptr) :: ptr
end type

private

public :: kdtree, kdtree_create, kdtree_destroy, kdtree_k_nearest_neighbors, kdtree_find_in_sphere

!------------------------------------------------------------------------------
interface
  ! eckit::KDTreeMemory<TreeTrait> fckit__kdtree_create(int n, double lon[], double lat[]) {
  function fckit__kdtree_create(n, lon, lat) result(kd) bind(C)
    use iso_c_binding, only: c_ptr, c_int, c_double
    type(c_ptr) :: kd
    integer(c_int) :: n
    real(c_double) :: lon(n)
    real(c_double) :: lat(n)
  end function

  ! void fckit__kdtree_destroy(eckit::KDTreeMemory<TreeTrait> * kd) {
  subroutine fckit__kdtree_destroy(kd) bind(C)
    use iso_c_binding, only: c_ptr
    type(c_ptr),value :: kd
  end subroutine

  ! void fckit__kdtree_k_nearest_neighbors(eckit::KDTreeMemory<TreeTrait> * kd, const double & lon, const double & lat, const int & nn, int * nn_index) {
  subroutine fckit__kdtree_k_nearest_neighbors(kd, lon, lat, nn, nn_index) bind(C)
    use iso_c_binding, only: c_ptr, c_int, c_double
    type(c_ptr),value :: kd
    real(c_double) :: lon
    real(c_double) :: lat
    integer(c_int) :: nn
    integer(c_int) :: nn_index(nn)
  end subroutine

  ! void fckit__kdtree_find_in_sphere(eckit::KDTreeMemory<TreeTrait> * kd, const double & lon, const double & lat, const double & r, int & nn) {
  subroutine fckit__kdtree_find_in_sphere(kd, lon, lat, r, nn) bind(C)
    use iso_c_binding, only: c_ptr, c_int, c_double
    type(c_ptr),value :: kd
    real(c_double) :: lon
    real(c_double) :: lat
    real(c_double) :: r
    integer(c_int) :: nn
  end subroutine
end interface

! =============================================================================
contains
! =============================================================================

function kdtree_create(n, lon, lat) result(kd)
  use iso_c_binding, only:  c_int, c_double
  type(kdtree) :: kd
  integer(c_int), intent(in) :: n
  real(c_double), intent(in) :: lon(n)
  real(c_double), intent(in) :: lat(n)
  kd%ptr = fckit__kdtree_create(n, lon, lat)
end function

subroutine kdtree_destroy(kd)
  type(kdtree),intent(inout) :: kd
  call fckit__kdtree_destroy(kd%ptr)
end subroutine

subroutine kdtree_k_nearest_neighbors(kd, lon, lat, nn, nn_index)
  use iso_c_binding, only: c_int, c_double
  type(kdtree),intent(in) :: kd
  real(c_double),intent(in) :: lon
  real(c_double),intent(in) :: lat
  integer(c_int),intent(in) :: nn
  integer(c_int),intent(out) :: nn_index(nn)
  call fckit__kdtree_k_nearest_neighbors(kd%ptr, lon, lat, nn, nn_index)
  nn_index = nn_index+1
end subroutine

subroutine kdtree_find_in_sphere(kd, lon, lat, r, nn)
  use iso_c_binding, only: c_ptr, c_int, c_double
  type(kdtree),intent(in) :: kd
  real(c_double),intent(in) :: lon
  real(c_double),intent(in) :: lat
  real(c_double),intent(in) :: r
  integer(c_int),intent(out) :: nn
  call fckit__kdtree_find_in_sphere(kd%ptr, lon, lat, r, nn)
end subroutine

end module fckit_kdtree_module
