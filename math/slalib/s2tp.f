      SUBROUTINE slS2TP (RA, DEC, RAZ, DECZ, XI, ETA, J)
*+
*     - - - - -
*      S 2 T P
*     - - - - -
*
*  Projection of spherical coordinates onto tangent plane:
*  "gnomonic" projection - "standard coordinates"
*  (single precision)
*
*  Given:
*     RA,DEC      real  spherical coordinates of point to be projected
*     RAZ,DECZ    real  spherical coordinates of tangent point
*
*  Returned:
*     XI,ETA      real  rectangular coordinates on tangent plane
*     J           int   status:   0 = OK, star on tangent plane
*                                 1 = error, star too far from axis
*                                 2 = error, antistar too far from axis
*                                 3 = error, antistar on tangent plane
*
*  P.T.Wallace   Starlink   March 1986
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      REAL RA,DEC,RAZ,DECZ,XI,ETA
      INTEGER J

      REAL SDECZ,SDEC,CDECZ,CDEC,RADIF,SRADIF,CRADIF,DENOM

      REAL TINY
      PARAMETER (TINY=1E-6)


*  Trig functions
      SDECZ=SIN(DECZ)
      SDEC=SIN(DEC)
      CDECZ=COS(DECZ)
      CDEC=COS(DEC)
      RADIF=RA-RAZ
      SRADIF=SIN(RADIF)
      CRADIF=COS(RADIF)

*  Reciprocal of star vector length to tangent plane
      DENOM=SDEC*SDECZ+CDEC*CDECZ*CRADIF

*  Handle vectors too far from axis
      IF (DENOM.GT.TINY) THEN
         J=0
      ELSE IF (DENOM.GE.0.0) THEN
         J=1
         DENOM=TINY
      ELSE IF (DENOM.GT.-TINY) THEN
         J=2
         DENOM=-TINY
      ELSE
         J=3
      END IF

*  Compute tangent plane coordinates (even in dubious cases)
      XI=CDEC*SRADIF/DENOM
      ETA=(SDEC*CDECZ-CDEC*SDECZ*CRADIF)/DENOM

      END
