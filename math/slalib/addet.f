      SUBROUTINE slADET (RM, DM, EQ, RC, DC)
*+
*     - - - - - -
*      A D E T
*     - - - - - -
*
*  Add the E-terms (elliptic component of annual aberration)
*  to a pre IAU 1976 mean place to conform to the old
*  catalogue convention (double precision)
*
*  Given:
*     RM,DM     dp     RA,Dec (radians) without E-terms
*     EQ        dp     Besselian epoch of mean equator and equinox
*
*  Returned:
*     RC,DC     dp     RA,Dec (radians) with E-terms included
*
*  Note:
*
*     Most star positions from pre-1984 optical catalogues (or
*     derived from astrometry using such stars) embody the
*     E-terms.  If it is necessary to convert a formal mean
*     place (for example a pulsar timing position) to one
*     consistent with such a star catalogue, then the RA,Dec
*     should be adjusted using this routine.
*
*  Reference:
*     Explanatory Supplement to the Astronomical Ephemeris,
*     section 2D, page 48.
*
*  Called:  slETRM, slDS2C, slDC2S, slDA2P, slDA1P
*
*  P.T.Wallace   Starlink   18 March 1999
*
*  Copyright (C) 1999 Rutherford Appleton Laboratory
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

      DOUBLE PRECISION RM,DM,EQ,RC,DC

      DOUBLE PRECISION slDA2P

      DOUBLE PRECISION A(3),V(3)

      INTEGER I



*  E-terms vector
      CALL slETRM(EQ,A)

*  Spherical to Cartesian
      CALL slDS2C(RM,DM,V)

*  Include the E-terms
      DO I=1,3
         V(I)=V(I)+A(I)
      END DO

*  Cartesian to spherical
      CALL slDC2S(V,RC,DC)

*  Bring RA into conventional range
      RC=slDA2P(RC)

      END
