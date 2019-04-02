/*
 * (C) Copyright 2019 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "eckit/geometry/Point2.h"
#include "eckit/geometry/Point3.h"
#include "eckit/geometry/UnitSphere.h"

namespace fckit {

extern "C" {

double fckit__sphere_distance(const double & lonA, const double & latA, const double & lonB, const double & latB) {
  const eckit::geometry::Point2 A(lonA, latA);
  const eckit::geometry::Point2 B(lonB, latB);
  return eckit::geometry::UnitSphere::distance(A, B);
}

void fckit__sphere_lonlat2xyz(const double & lon, const double & lat, double & x, double & y, double & z) {
  const eckit::geometry::Point2 lonlat(lon, lat);
  eckit::geometry::Point3 xyz;
  eckit::geometry::UnitSphere::convertSphericalToCartesian(lonlat, xyz);
  x = xyz[0];
  y = xyz[1];
  z = xyz[2];
}

void fckit__sphere_xyz2lonlat(const double & x, const double & y, const double & z, double & lon, double & lat) {
  const eckit::geometry::Point3 xyz(x, y, z);
  eckit::geometry::Point2 lonlat;
  eckit::geometry::UnitSphere::convertCartesianToSpherical(xyz, lonlat);
  lon = lonlat[0];
  lat = lonlat[1];
}

struct TreeTrait {
    typedef eckit::geometry::Point3 Point;
    typedef double                  Payload;
};

} // extern "C"

} // namespace fckit
