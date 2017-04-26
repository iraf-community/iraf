      DOUBLE PRECISION FUNCTION slDA2P (ANGLE)
*+
*     - - - - - - -
*      D A 2 P
*     - - - - - - -
*
*  Normalize angle into range 0-2 pi  (double precision)
*
*  Given:
*     ANGLE     dp      the angle in radians
*
*  The result is ANGLE expressed in the range 0-2 pi.
*
*  Last revision:   22 July 2004
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

      DOUBLE PRECISION ANGLE

      DOUBLE PRECISION D2PI
      PARAMETER (D2PI=6.283185307179586476925286766559D0)


      slDA2P = MOD(ANGLE,D2PI)
      IF (slDA2P.LT.0D0) slDA2P = slDA2P+D2PI

      END
