      REAL FUNCTION slRA1P (ANGLE)
*+
*     - - - - - -
*      R A 1 P
*     - - - - - -
*
*  Normalize angle into range +/- pi  (single precision)
*
*  Given:
*     ANGLE     dp      the angle in radians
*
*  The result is ANGLE expressed in the +/- pi (single
*  precision).
*
*  P.T.Wallace   Starlink   23 November 1995
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

      REAL ANGLE

      REAL API,A2PI
      PARAMETER (API=3.141592653589793238462643)
      PARAMETER (A2PI=6.283185307179586476925287)


      slRA1P=MOD(ANGLE,A2PI)
      IF (ABS(slRA1P).GE.API)
     :          slRA1P=slRA1P-SIGN(A2PI,ANGLE)

      END
