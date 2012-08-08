      REAL FUNCTION slRA2P (ANGLE)
*+
*     - - - - - - -
*      R A 2 P
*     - - - - - - -
*
*  Normalize angle into range 0-2 pi  (single precision)
*
*  Given:
*     ANGLE     dp      the angle in radians
*
*  The result is ANGLE expressed in the range 0-2 pi (single
*  precision).
*
*  P.T.Wallace   Starlink   23 November 1995
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      REAL ANGLE

      REAL A2PI
      PARAMETER (A2PI=6.283185307179586476925287)


      slRA2P=MOD(ANGLE,A2PI)
      IF (slRA2P.LT.0.0) slRA2P=slRA2P+A2PI

      END
