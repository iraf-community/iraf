      DOUBLE PRECISION FUNCTION slDA1P (ANGLE)
*+
*     - - - - - - -
*      D A 1 P
*     - - - - - - -
*
*  Normalise angle into range +/- pi  (double precision)
*
*  Given:
*     ANGLE     dp      the angle in radians
*
*  The result (double precision) is ANGLE expressed in the range +/- pi.
*
*  P.T.Wallace   Starlink   6 April 1990
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION ANGLE

      DOUBLE PRECISION DPI,D2PI
      PARAMETER (DPI=3.141592653589793238462643D0)
      PARAMETER (D2PI=6.283185307179586476925287D0)


      slDA1P=MOD(ANGLE,D2PI)
      IF (ABS(slDA1P).GE.DPI)
     :          slDA1P=slDA1P-SIGN(D2PI,ANGLE)

      END
