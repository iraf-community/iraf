      REAL FUNCTION slRVLK (R2000, D2000)
*+
*     - - - - - - -
*      R V L K
*     - - - - - - -
*
*  Velocity component in a given direction due to the Sun's motion
*  with respect to an adopted kinematic Local Standard of Rest.
*
*  (single precision)
*
*  Given:
*     R2000,D2000   r    J2000.0 mean RA,Dec (radians)
*
*  Result:
*     Component of "standard" solar motion in direction R2000,D2000 (km/s)
*
*  Sign convention:
*     The result is +ve when the Sun is receding from the given point on
*     the sky.
*
*  Note:  The Local Standard of Rest used here is one of several
*         "kinematical" LSRs in common use.  A kinematical LSR is the
*         mean standard of rest of specified star catalogues or stellar
*         populations.  The Sun's motion with respect to a kinematical
*         LSR is known as the "standard" solar motion.
*
*         There is another sort of LSR, the "dynamical" LSR, which is a
*         point in the vicinity of the Sun which is in a circular orbit
*         around the Galactic centre.  The Sun's motion with respect to
*         the dynamical LSR is called the "peculiar" solar motion.  To
*         obtain a radial velocity correction with respect to the
*         dynamical LSR use the routine slRVLD.
*
*  Reference:  Delhaye (1965), in "Stars and Stellar Systems", vol 5,
*              p73.
*
*  Called:
*     slCS2C, slVDV
*
*  P.T.Wallace   Starlink   11 March 1994
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*
*  License:
*    This program is free software; you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation; either version 2 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program (see SLA_CONDITIONS); if not, write to the
*    Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
*    Boston, MA  02110-1301  USA
*
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      REAL R2000,D2000

      REAL VA(3), VB(3)

      REAL slVDV

*
*  Standard solar motion (from Methods of Experimental Physics, ed Meeks,
*  vol 12, part C, sec 6.1.5.2, p281):
*
*  20 km/s towards RA 18h Dec +30d (1900).
*
*  The solar motion is expressed here in the form of a J2000.0
*  equatorial Cartesian vector:
*
*      VA(1) = X = -SPEED*COS(RA)*COS(DEC)
*      VA(2) = Y = -SPEED*SIN(RA)*COS(DEC)
*      VA(3) = Z = -SPEED*SIN(DEC)

      DATA VA / -0.29000, +17.31726, -10.00141 /



*  Convert given J2000 RA,Dec to x,y,z
      CALL slCS2C(R2000,D2000,VB)

*  Compute dot product with solar motion vector
      slRVLK=slVDV(VA,VB)

      END
