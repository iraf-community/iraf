      SUBROUTINE slEQEC (DR, DD, DATE, DL, DB)
*+
*     - - - - - -
*      E Q E C
*     - - - - - -
*
*  Transformation from J2000.0 equatorial coordinates to
*  ecliptic coordinates (double precision)
*
*  Given:
*     DR,DD       dp      J2000.0 mean RA,Dec (radians)
*     DATE        dp      TDB (loosely ET) as Modified Julian Date
*                                              (JD-2400000.5)
*  Returned:
*     DL,DB       dp      ecliptic longitude and latitude
*                         (mean of date, IAU 1980 theory, radians)
*
*  Called:
*     slDS2C, slPREC, slEPJ, slDMXV, slECMA, slDC2S,
*     slDA2P, slDA1P
*
*  P.T.Wallace   Starlink   March 1986
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

      DOUBLE PRECISION DR,DD,DATE,DL,DB

      DOUBLE PRECISION slEPJ,slDA2P,slDA1P

      DOUBLE PRECISION RMAT(3,3),V1(3),V2(3)



*  Spherical to Cartesian
      CALL slDS2C(DR,DD,V1)

*  Mean J2000 to mean of date
      CALL slPREC(2000D0,slEPJ(DATE),RMAT)
      CALL slDMXV(RMAT,V1,V2)

*  Equatorial to ecliptic
      CALL slECMA(DATE,RMAT)
      CALL slDMXV(RMAT,V2,V1)

*  Cartesian to spherical
      CALL slDC2S(V1,DL,DB)

*  Express in conventional ranges
      DL=slDA2P(DL)
      DB=slDA1P(DB)

      END
