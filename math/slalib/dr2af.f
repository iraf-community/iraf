      SUBROUTINE slDRAF (NDP, ANGLE, SIGN, IDMSF)
*+
*     - - - - - -
*      D R A F
*     - - - - - -
*
*  Convert an angle in radians to degrees, arcminutes, arcseconds
*  (double precision)
*
*  Given:
*     NDP      i      number of decimal places of arcseconds
*     ANGLE    d      angle in radians
*
*  Returned:
*     SIGN     c      '+' or '-'
*     IDMSF    i(4)   degrees, arcminutes, arcseconds, fraction
*
*  Notes:
*
*     1)  NDP less than zero is interpreted as zero.
*
*     2)  The largest useful value for NDP is determined by the size
*         of ANGLE, the format of DOUBLE PRECISION floating-point
*         numbers on the target machine, and the risk of overflowing
*         IDMSF(4).  On some architectures, for ANGLE up to 2pi, the
*         available floating-point precision corresponds roughly to
*         NDP=12.  However, the practical limit is NDP=9, set by the
*         capacity of a typical 32-bit IDMSF(4).
*
*     3)  The absolute value of ANGLE may exceed 2pi.  In cases where it
*         does not, it is up to the caller to test for and handle the
*         case where ANGLE is very nearly 2pi and rounds up to 360 deg,
*         by testing for IDMSF(1)=360 and setting IDMSF(1-4) to zero.
*
*  Called:  slDDTF
*
*  Last revision:   26 December 2004
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

      INTEGER NDP
      DOUBLE PRECISION ANGLE
      CHARACTER SIGN*(*)
      INTEGER IDMSF(4)

*  Hours to degrees * radians to turns
      DOUBLE PRECISION F
      PARAMETER (F=15D0/6.283185307179586476925287D0)



*  Scale then use days to h,m,s routine
      CALL slDDTF(NDP,ANGLE*F,SIGN,IDMSF)

      END
