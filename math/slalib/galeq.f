      SUBROUTINE slGAEQ (DL, DB, DR, DD)
*+
*     - - - - - -
*      G A E Q
*     - - - - - -
*
*  Transformation from IAU 1958 galactic coordinates to
*  J2000.0 equatorial coordinates (double precision)
*
*  Given:
*     DL,DB       dp       galactic longitude and latitude L2,B2
*
*  Returned:
*     DR,DD       dp       J2000.0 RA,Dec
*
*  (all arguments are radians)
*
*  Called:
*     slDS2C, slDIMV, slDC2S, slDA2P, slDA1P
*
*  Note:
*     The equatorial coordinates are J2000.0.  Use the routine
*     slGE50 if conversion to B1950.0 'FK4' coordinates is
*     required.
*
*  Reference:
*     Blaauw et al, Mon.Not.R.Astron.Soc.,121,123 (1960)
*
*  P.T.Wallace   Starlink   November 1988
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION DL,DB,DR,DD

      DOUBLE PRECISION slDA2P,slDA1P

      DOUBLE PRECISION V1(3),V2(3)

*
*  L2,B2 system of galactic coordinates
*
*  P = 192.25       RA of galactic north pole (mean B1950.0)
*  Q =  62.6        inclination of galactic to mean B1950.0 equator
*  R =  33          longitude of ascending node
*
*  P,Q,R are degrees
*
*  Equatorial to galactic rotation matrix (J2000.0), obtained by
*  applying the standard FK4 to FK5 transformation, for inertially
*  zero proper motion, to the columns of the B1950 equatorial to
*  galactic rotation matrix:
*
      DOUBLE PRECISION RMAT(3,3)
      DATA RMAT(1,1),RMAT(1,2),RMAT(1,3),
     :     RMAT(2,1),RMAT(2,2),RMAT(2,3),
     :     RMAT(3,1),RMAT(3,2),RMAT(3,3)/
     : -0.054875539726D0,-0.873437108010D0,-0.483834985808D0,
     : +0.494109453312D0,-0.444829589425D0,+0.746982251810D0,
     : -0.867666135858D0,-0.198076386122D0,+0.455983795705D0/



*  Spherical to Cartesian
      CALL slDS2C(DL,DB,V1)

*  Galactic to equatorial
      CALL slDIMV(RMAT,V1,V2)

*  Cartesian to spherical
      CALL slDC2S(V2,DR,DD)

*  Express in conventional ranges
      DR=slDA2P(DR)
      DD=slDA1P(DD)

      END
