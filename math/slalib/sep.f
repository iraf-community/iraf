      REAL FUNCTION slSEP (A1, B1, A2, B2)
*+
*     - - - -
*      S E P
*     - - - -
*
*  Angle between two points on a sphere (single precision)
*
*  Given:
*     A1,B1    real    spherical coordinates of one point
*     A2,B2    real    spherical coordinates of the other point
*
*  (The spherical coordinates are RA,Dec, Long,Lat etc, in radians.)
*
*  The result is the angle, in radians, between the two points.  It
*  is always positive.
*
*  Called:  slCS2C
*
*  P.T.Wallace   Starlink   April 1985
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      REAL A1,B1,A2,B2

      INTEGER I
      REAL V1(3),V2(3),W



*  Convert coordinates from spherical to Cartesian
      CALL slCS2C(A1,B1,V1)
      CALL slCS2C(A2,B2,V2)

*  Modulus squared of half the difference vector
      W=0.0
      DO I=1,3
         W=W+(V1(I)-V2(I))**2
      END DO
      W=W/4.0

*  Angle between the vectors
      slSEP=2.0*ATAN2(SQRT(W),SQRT(MAX(0.0,1.0-W)))

      END
