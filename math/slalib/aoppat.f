      SUBROUTINE slAOPT (DATE, AOPRMS)
*+
*     - - - - - - -
*      A O P T
*     - - - - - - -
*
*  Recompute the sidereal time in the apparent to observed place
*  star-independent parameter block.
*
*  Given:
*     DATE   d      UTC date/time (modified Julian Date, JD-2400000.5)
*                   (see AOPPA source for comments on leap seconds)
*
*     AOPRMS d(14)  star-independent apparent-to-observed parameters
*
*       (1-12)   not required
*       (13)     longitude + eqn of equinoxes + sidereal DUT
*       (14)     not required
*
*  Returned:
*     AOPRMS d(14)  star-independent apparent-to-observed parameters:
*
*       (1-13)   not changed
*       (14)     local apparent sidereal time (radians)
*
*  For more information, see slAOPA.
*
*  Called:  slGMST
*
*  P.T.Wallace   Starlink   1 July 1993
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

      DOUBLE PRECISION DATE,AOPRMS(14)

      DOUBLE PRECISION slGMST



      AOPRMS(14) = slGMST(DATE)+AOPRMS(13)

      END
