      REAL FUNCTION slRA1P (ANGLE)
*+
*     - - - - - -
*      R A 1 P
*     - - - - - -
*
*  Normalise angle into range +/- pi  (single precision)
*
*  Given:
*     ANGLE     dp      the angle in radians
*
*  The result is ANGLE expressed in the +/- pi (single
*  precision).
*
*  P.T.Wallace   Starlink   December 1984
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      REAL ANGLE

      REAL API,A2PI
      PARAMETER (API=3.141592653589793238462643)
      PARAMETER (A2PI=6.283185307179586476925287)


      slRA1P=MOD(ANGLE,A2PI)
      IF (ABS(slRA1P).GE.API)
     :          slRA1P=slRA1P-SIGN(A2PI,ANGLE)

      END
