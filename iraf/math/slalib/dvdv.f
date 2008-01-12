      DOUBLE PRECISION FUNCTION slDVDV (VA, VB)
*+
*     - - - - -
*      D V D V
*     - - - - -
*
*  Scalar product of two 3-vectors  (double precision)
*
*  Given:
*      VA      dp(3)     first vector
*      VB      dp(3)     second vector
*
*  The result is the scalar product VA.VB (double precision)
*
*  P.T.Wallace   Starlink   November 1984
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION VA(3),VB(3)


      slDVDV=VA(1)*VB(1)+VA(2)*VB(2)+VA(3)*VB(3)

      END
