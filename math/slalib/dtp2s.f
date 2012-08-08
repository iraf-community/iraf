      SUBROUTINE slDTPS (XI, ETA, RAZ, DECZ, RA, DEC)
*+
*     - - - - - -
*      D T P S
*     - - - - - -
*
*  Transform tangent plane coordinates into spherical
*  (double precision)
*
*  Given:
*     XI,ETA      dp   tangent plane rectangular coordinates
*     RAZ,DECZ    dp   spherical coordinates of tangent point
*
*  Returned:
*     RA,DEC      dp   spherical coordinates (0-2pi,+/-pi/2)
*
*  Called:        slDA2P
*
*  P.T.Wallace   Starlink   24 July 1995
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION XI,ETA,RAZ,DECZ,RA,DEC

      DOUBLE PRECISION slDA2P

      DOUBLE PRECISION SDECZ,CDECZ,DENOM



      SDECZ=SIN(DECZ)
      CDECZ=COS(DECZ)

      DENOM=CDECZ-ETA*SDECZ

      RA=slDA2P(ATAN2(XI,DENOM)+RAZ)
      DEC=ATAN2(SDECZ+ETA*CDECZ,SQRT(XI*XI+DENOM*DENOM))

      END
