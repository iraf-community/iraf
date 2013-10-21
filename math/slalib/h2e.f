      SUBROUTINE slH2E ( AZ, EL, PHI, HA, DEC )
*+
*     - - - - -
*      D E 2 H
*     - - - - -
*
*  Horizon to equatorial coordinates:  Az,El to HA,Dec
*
*  (single precision)
*
*  Given:
*     AZ      r     azimuth
*     EL      r     elevation
*     PHI     r     observatory latitude
*
*  Returned:
*     HA      r     hour angle
*     DEC     r     declination
*
*  Notes:
*
*  1)  All the arguments are angles in radians.
*
*  2)  The sign convention for azimuth is north zero, east +pi/2.
*
*  3)  HA is returned in the range +/-pi.  Declination is returned
*      in the range +/-pi/2.
*
*  4)  The latitude is (in principle) geodetic.  In critical
*      applications, corrections for polar motion should be applied.
*
*  5)  In some applications it will be important to specify the
*      correct type of elevation in order to produce the required
*      type of HA,Dec.  In particular, it may be important to
*      distinguish between the elevation as affected by refraction,
*      which will yield the "observed" HA,Dec, and the elevation
*      in vacuo, which will yield the "topocentric" HA,Dec.  If the
*      effects of diurnal aberration can be neglected, the
*      topocentric HA,Dec may be used as an approximation to the
*      "apparent" HA,Dec.
*
*  6)  No range checking of arguments is done.
*
*  7)  In applications which involve many such calculations, rather
*      than calling the present routine it will be more efficient to
*      use inline code, having previously computed fixed terms such
*      as sine and cosine of latitude.
*
*  Last revision:   11 September 2005
*
*  Copyright P.T.Wallace.  All rights reserved.
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

      REAL AZ, EL, PHI, HA, DEC

      DOUBLE PRECISION SA, CA, SE, CE, SP, CP, X, Y, Z, R


*  Useful trig functions.
      SA = SIN(AZ)
      CA = COS(AZ)
      SE = SIN(EL)
      CE = COS(EL)
      SP = SIN(PHI)
      CP = COS(PHI)

*  HA,Dec as x,y,z.
      X = -CA*CE*SP+SE*CP
      Y = -SA*CE
      Z = CA*CE*CP+SE*SP

*  To HA,Dec.
      R = SQRT(X*X+Y*Y)
      IF (R.EQ.0.0) THEN
         HA = 0.0
      ELSE
         HA = REAL(ATAN2(Y,X))
      END IF
      DEC = REAL(ATAN2(Z,R))

      END
