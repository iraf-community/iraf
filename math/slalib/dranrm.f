      DOUBLE PRECISION FUNCTION slDA2P (ANGLE)
*+
*     - - - - - - -
*      D A 2 P
*     - - - - - - -
*
*  Normalise angle into range 0-2 pi  (double precision)
*
*  Given:
*     ANGLE     dp      the angle in radians
*
*  The result is ANGLE expressed in the range 0-2 pi (double
*  precision).
*
*  P.T.Wallace   Starlink   10 December 1993
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION ANGLE

      DOUBLE PRECISION D2PI
      PARAMETER (D2PI=6.283185307179586476925286766559D0)


      slDA2P=MOD(ANGLE,D2PI)
      IF (slDA2P.LT.0D0) slDA2P=slDA2P+D2PI

      END
