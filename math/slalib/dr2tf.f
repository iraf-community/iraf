      SUBROUTINE slDRTF (NDP, ANGLE, SIGN, IHMSF)
*+
*     - - - - - -
*      D R T F
*     - - - - - -
*
*  Convert an angle in radians to hours, minutes, seconds
*  (double precision)
*
*  Given:
*     NDP       int      number of decimal places of seconds
*     ANGLE     dp       angle in radians
*
*  Returned:
*     SIGN      char     '+' or '-'
*     IHMSF     int(4)   hours, minutes, seconds, fraction
*
*  Notes:
*
*     1)  NDP less than zero is interpreted as zero.
*
*     2)  The largest useful value for NDP is determined by the size
*         of ANGLE, the format of DOUBLE PRECISION floating-point
*         numbers on the target machine, and the risk of overflowing
*         IHMSF(4).  For example, on the VAX, for ANGLE up to 2pi, the
*         available floating-point precision corresponds roughly to
*         NDP=12.  However, the practical limit is NDP=9, set by the
*         capacity of the 32-bit integer IHMSF(4).
*
*     3)  The absolute value of ANGLE may exceed 2pi.  In cases where it
*         does not, it is up to the caller to test for and handle the
*         case where ANGLE is very nearly 2pi and rounds up to 24 hours,
*         by testing for IHMSF(1)=24 and setting IHMSF(1-4) to zero.
*
*  Called:
*     slDDTF
*
*  P.T.Wallace   Starlink   11 September 1990
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      INTEGER NDP
      DOUBLE PRECISION ANGLE
      CHARACTER SIGN*(*)
      INTEGER IHMSF(4)

*  Turns to radians
      DOUBLE PRECISION T2R
      PARAMETER (T2R=6.283185307179586476925287D0)



*  Scale then use days to h,m,s routine
      CALL slDDTF(NDP,ANGLE/T2R,SIGN,IHMSF)

      END
