      SUBROUTINE slTP2S (XI, ETA, RAZ, DECZ, RA, DEC)
*+
*     - - - - -
*      T P 2 S
*     - - - - -
*
*  Transform tangent plane coordinates into spherical
*  (single precision)
*
*  Given:
*     XI,ETA      real  tangent plane rectangular coordinates
*     RAZ,DECZ    real  spherical coordinates of tangent point
*
*  Returned:
*     RA,DEC      real  spherical coordinates (0-2pi,+/-pi/2)
*
*  Called:        slRA2P
*
*  P.T.Wallace   Starlink   24 July 1995
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

      REAL XI,ETA,RAZ,DECZ,RA,DEC

      REAL slRA2P

      REAL SDECZ,CDECZ,DENOM



      SDECZ=SIN(DECZ)
      CDECZ=COS(DECZ)

      DENOM=CDECZ-ETA*SDECZ

      RA=slRA2P(ATAN2(XI,DENOM)+RAZ)
      DEC=ATAN2(SDECZ+ETA*CDECZ,SQRT(XI*XI+DENOM*DENOM))

      END
