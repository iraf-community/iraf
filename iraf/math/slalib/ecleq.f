      SUBROUTINE slECEQ (DL, DB, DATE, DR, DD)
*+
*     - - - - - -
*      E C E Q
*     - - - - - -
*
*  Transformation from ecliptic coordinates to
*  J2000.0 equatorial coordinates (double precision)
*
*  Given:
*     DL,DB       dp      ecliptic longitude and latitude
*                           (mean of date, IAU 1980 theory, radians)
*     DATE        dp      TDB (loosely ET) as Modified Julian Date
*                                              (JD-2400000.5)
*  Returned:
*     DR,DD       dp      J2000.0 mean RA,Dec (radians)
*
*  Called:
*     slDS2C, slECMA, slDIMV, slPREC, slEPJ, slDC2S,
*     slDA2P, slDA1P
*
*  P.T.Wallace   Starlink   March 1986
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION DL,DB,DATE,DR,DD

      DOUBLE PRECISION slEPJ,slDA2P,slDA1P

      DOUBLE PRECISION RMAT(3,3),V1(3),V2(3)



*  Spherical to Cartesian
      CALL slDS2C(DL,DB,V1)

*  Ecliptic to equatorial
      CALL slECMA(DATE,RMAT)
      CALL slDIMV(RMAT,V1,V2)

*  Mean of date to J2000
      CALL slPREC(2000D0,slEPJ(DATE),RMAT)
      CALL slDIMV(RMAT,V2,V1)

*  Cartesian to spherical
      CALL slDC2S(V1,DR,DD)

*  Express in conventional ranges
      DR=slDA2P(DR)
      DD=slDA1P(DD)

      END
