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
