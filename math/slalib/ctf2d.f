      SUBROUTINE slCTFD (IHOUR, IMIN, SEC, DAYS, J)
*+
*     - - - - - -
*      C T F D
*     - - - - - -
*
*  Convert hours, minutes, seconds to days (single precision)
*
*  Given:
*     IHOUR       int       hours
*     IMIN        int       minutes
*     SEC         real      seconds
*
*  Returned:
*     DAYS        real      interval in days
*     J           int       status:  0 = OK
*                                    1 = IHOUR outside range 0-23
*                                    2 = IMIN outside range 0-59
*                                    3 = SEC outside range 0-59.999...
*
*  Notes:
*
*  1)  The result is computed even if any of the range checks
*      fail.
*
*  2)  The sign must be dealt with outside this routine.
*
*  P.T.Wallace   Starlink   November 1984
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      INTEGER IHOUR,IMIN
      REAL SEC,DAYS
      INTEGER J

*  Seconds per day
      REAL D2S
      PARAMETER (D2S=86400.0)



*  Preset status
      J=0

*  Validate sec, min, hour
      IF (SEC.LT.0.0.OR.SEC.GE.60.0) J=3
      IF (IMIN.LT.0.OR.IMIN.GT.59) J=2
      IF (IHOUR.LT.0.OR.IHOUR.GT.23) J=1

*  Compute interval
      DAYS=(60.0*(60.0*REAL(IHOUR)+REAL(IMIN))+SEC)/D2S

      END
