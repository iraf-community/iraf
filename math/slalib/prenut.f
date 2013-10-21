      SUBROUTINE slPRNU (EPOCH, DATE, RMATPN)
*+
*     - - - - - - -
*      P R N U
*     - - - - - - -
*
*  Form the matrix of precession and nutation (SF2001)
*  (double precision)
*
*  Given:
*     EPOCH   dp         Julian Epoch for mean coordinates
*     DATE    dp         Modified Julian Date (JD-2400000.5)
*                        for true coordinates
*
*  Returned:
*     RMATPN  dp(3,3)    combined precession/nutation matrix
*
*  Called:  slPREC, slEPJ, slNUT, slDMXM
*
*  Notes:
*
*  1)  The epoch and date are TDB (loosely ET).  TT will do, or even
*      UTC.
*
*  2)  The matrix is in the sense   V(true) = RMATPN * V(mean)
*
*  Last revision:   3 December 2005
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

      DOUBLE PRECISION EPOCH,DATE,RMATPN(3,3)

      DOUBLE PRECISION RMATP(3,3),RMATN(3,3),slEPJ



*  Precession
      CALL slPREC(EPOCH,slEPJ(DATE),RMATP)

*  Nutation
      CALL slNUT(DATE,RMATN)

*  Combine the matrices:  PN = N x P
      CALL slDMXM(RMATN,RMATP,RMATPN)

      END
