      SUBROUTINE slPM (R0, D0, PR, PD, PX, RV, EP0, EP1, R1, D1)
*+
*     - - -
*      P M
*     - - -
*
*  Apply corrections for proper motion to a star RA,Dec
*  (double precision)
*
*  References:
*     1984 Astronomical Almanac, pp B39-B41.
*     (also Lederle & Schwan, Astron. Astrophys. 134,
*      1-6, 1984)
*
*  Given:
*     R0,D0    dp     RA,Dec at epoch EP0 (rad)
*     PR,PD    dp     proper motions:  RA,Dec changes per year of epoch
*     PX       dp     parallax (arcsec)
*     RV       dp     radial velocity (km/sec, +ve if receding)
*     EP0      dp     start epoch in years (e.g. Julian epoch)
*     EP1      dp     end epoch in years (same system as EP0)
*
*  Returned:
*     R1,D1    dp     RA,Dec at epoch EP1 (rad)
*
*  Called:
*     slDS2C       spherical to Cartesian
*     slDC2S       Cartesian to spherical
*     slDA2P      normalise angle 0-2Pi
*
*  Note:
*     The proper motions in RA are dRA/dt rather than
*     cos(Dec)*dRA/dt, and are in the same coordinate
*     system as R0,D0.
*
*  P.T.Wallace   Starlink   12 April 1990
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION R0,D0,PR,PD,PX,RV,EP0,EP1,R1,D1

*  Km/s to AU/year multiplied by arc seconds to radians
      DOUBLE PRECISION VFR
      PARAMETER (VFR=0.21094502D0*0.4848136811095359949D-05)

      INTEGER I
      DOUBLE PRECISION slDA2P
      DOUBLE PRECISION W,EM(3),T,P(3)



*  Spherical to Cartesian
      CALL slDS2C(R0,D0,P)

*  Space motion (radians per year)
      W=VFR*RV*PX
      EM(1)=-PR*P(2)-PD*COS(R0)*SIN(D0)+W*P(1)
      EM(2)= PR*P(1)-PD*SIN(R0)*SIN(D0)+W*P(2)
      EM(3)=         PD*COS(D0)        +W*P(3)

*  Apply the motion
      T=EP1-EP0
      DO I=1,3
         P(I)=P(I)+T*EM(I)
      END DO

*  Cartesian to spherical
      CALL slDC2S(P,R1,D1)
      R1=slDA2P(R1)

      END
