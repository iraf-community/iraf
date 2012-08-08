      SUBROUTINE slSUET (RC, DC, EQ, RM, DM)
*+
*     - - - - - -
*      S U E T
*     - - - - - -
*
*  Remove the E-terms (elliptic component of annual aberration)
*  from a pre IAU 1976 catalogue RA,Dec to give a mean place
*  (double precision)
*
*  Given:
*     RC,DC     dp     RA,Dec (radians) with E-terms included
*     EQ        dp     Besselian epoch of mean equator and equinox
*
*  Returned:
*     RM,DM     dp     RA,Dec (radians) without E-terms
*
*  Called:
*     slETRM, slDS2C, sla_,DVDV, slDC2S, slDA2P
*
*  Explanation:
*     Most star positions from pre-1984 optical catalogues (or
*     derived from astrometry using such stars) embody the
*     E-terms.  This routine converts such a position to a
*     formal mean place (allowing, for example, comparison with a
*     pulsar timing position).
*
*  Reference:
*     Explanatory Supplement to the Astronomical Ephemeris,
*     section 2D, page 48.
*
*  P.T.Wallace   Starlink   10 May 1990
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION RC,DC,EQ,RM,DM

      DOUBLE PRECISION slDA2P,slDVDV
      DOUBLE PRECISION A(3),V(3),F

      INTEGER I



*  E-terms
      CALL slETRM(EQ,A)

*  Spherical to Cartesian
      CALL slDS2C(RC,DC,V)

*  Include the E-terms
      F=1D0+slDVDV(V,A)
      DO I=1,3
         V(I)=F*V(I)-A(I)
      END DO

*  Cartesian to spherical
      CALL slDC2S(V,RM,DM)

*  Bring RA into conventional range
      RM=slDA2P(RM)

      END
