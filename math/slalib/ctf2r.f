      SUBROUTINE slCTFR (IHOUR, IMIN, SEC, RAD, J)
*+
*     - - - - - -
*      C T F R
*     - - - - - -
*
*  Convert hours, minutes, seconds to radians (single precision)
*
*  Given:
*     IHOUR       int       hours
*     IMIN        int       minutes
*     SEC         real      seconds
*
*  Returned:
*     RAD         real      angle in radians
*     J           int       status:  0 = OK
*                                    1 = IHOUR outside range 0-23
*                                    2 = IMIN outside range 0-59
*                                    3 = SEC outside range 0-59.999...
*
*  Called:
*     slCTFD
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
      REAL SEC,RAD
      INTEGER J

      REAL TURNS

*  Turns to radians
      REAL T2R
      PARAMETER (T2R=6.283185307179586476925287)



*  Convert to turns then radians
      CALL slCTFD(IHOUR,IMIN,SEC,TURNS,J)
      RAD=T2R*TURNS

      END
