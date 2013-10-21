      REAL FUNCTION slSEP (A1, B1, A2, B2)
*+
*     - - - -
*      S E P
*     - - - -
*
*  Angle between two points on a sphere.
*
*  (single precision)
*
*  Given:
*     A1,B1    r     spherical coordinates of one point
*     A2,B2    r     spherical coordinates of the other point
*
*  (The spherical coordinates are [RA,Dec], [Long,Lat] etc, in radians.)
*
*  The result is the angle, in radians, between the two points.  It
*  is always positive.
*
*  Called:  slDSEP
*
*  Last revision:   7 May 2000
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

      REAL A1,B1,A2,B2

      DOUBLE PRECISION slDSEP



*  Use double precision version.
      slSEP = REAL(slDSEP(DBLE(A1),DBLE(B1),DBLE(A2),DBLE(B2)))

      END
