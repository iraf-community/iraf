      SUBROUTINE slHF5Z (RH,DH,EPOCH,R5,D5,DR5,DD5)
*+
*     - - - - - -
*      H F 5 Z
*     - - - - - -
*
*  Transform a Hipparcos star position into FK5 J2000, assuming
*  zero Hipparcos proper motion.
*
*  (double precision)
*
*  Given:
*     RH        d      Hipparcos RA (radians)
*     DH        d      Hipparcos Dec (radians)
*     EPOCH     d      Julian epoch (TDB)
*
*  Returned (all FK5, equinox J2000, epoch EPOCH):
*     R5        d      RA (radians)
*     D5        d      Dec (radians)
*
*  Called:  slDS2C, slDAVM, slDMXV, slDAVM, slDMXM,
*           slDIMV, slDVXV, slDC6S, slDA2P
*
*  Notes:
*
*  1)  The proper motion in RA is dRA/dt rather than cos(Dec)*dRA/dt.
*
*  2)  The FK5 to Hipparcos transformation consists of a pure
*      rotation and spin;  zonal errors in the FK5 catalogue are
*      not taken into account.
*
*  3)  The published orientation and spin components are interpreted
*      as "axial vectors".  An axial vector points at the pole of the
*      rotation and its length is the amount of rotation in radians.
*
*  4)  It was the intention that Hipparcos should be a close
*      approximation to an inertial frame, so that distant objects
*      have zero proper motion;  such objects have (in general)
*      non-zero proper motion in FK5, and this routine returns those
*      fictitious proper motions.
*
*  5)  The position returned by this routine is in the FK5 J2000
*      reference frame but at Julian epoch EPOCH.
*
*  6)  See also slFK5H, slHFK5, slF5HZ.
*
*  Reference:
*
*     M.Feissel & F.Mignard, Astron. Astrophys. 331, L33-L36 (1998).
*
*  P.T.Wallace   Starlink   7 October 1998
*
*  Copyright (C) 1998 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION RH,DH,EPOCH,R5,D5,DR5,DD5

      DOUBLE PRECISION slDA2P

      DOUBLE PRECISION AS2R
      PARAMETER (AS2R=0.484813681109535994D-5)

*  FK5 to Hipparcos orientation and spin (radians, radians/year)
      DOUBLE PRECISION EPX,EPY,EPZ
      DOUBLE PRECISION OMX,OMY,OMZ

      PARAMETER ( EPX = -19.9D-3 * AS2R,
     :            EPY =  -9.1D-3 * AS2R,
     :            EPZ = +22.9D-3 * AS2R )

      PARAMETER ( OMX = -0.30D-3 * AS2R,
     :            OMY = +0.60D-3 * AS2R,
     :            OMZ = +0.70D-3 * AS2R )

      DOUBLE PRECISION PH(6),ORTN(3),R5H(3,3),S5(3),SH(3),T,VST(3),
     :                 RST(3,3),R5HT(3,3),PV5E(6),VV(3),R,V,W



*  Hipparcos barycentric position vector (normalized).
      CALL slDS2C(RH,DH,PH)

*  FK5 to Hipparcos orientation matrix.
      ORTN(1) = EPX
      ORTN(2) = EPY
      ORTN(3) = EPZ
      CALL slDAVM(ORTN,R5H)

*  Hipparcos wrt FK5 spin vector.
      S5(1) = OMX
      S5(2) = OMY
      S5(3) = OMZ

*  Rotate the spin vector into the Hipparcos frame.
      CALL slDMXV(R5H,S5,SH)

*  Time interval from J2000 to epoch.
      T = EPOCH-2000D0

*  Axial vector:  accumulated Hipparcos wrt FK5 spin over that interval.
      VST(1) = OMX*T
      VST(2) = OMY*T
      VST(3) = OMZ*T

*  Express the accumulated spin as a rotation matrix.
      CALL slDAVM(VST,RST)

*  Rotation matrix:  accumulated spin, then FK5 to Hipparcos.
      CALL slDMXM(R5H,RST,R5HT)

*  De-orient & de-spin the vector into FK5 J2000 at epoch.
      CALL slDIMV(R5HT,PH,PV5E)
      CALL slDVXV(SH,PH,VV)
      CALL slDIMV(R5HT,VV,PV5E(4))

*  FK5 position/velocity 6-vector to spherical.
      CALL slDC6S(PV5E,W,D5,R,DR5,DD5,V)
      R5 = slDA2P(W)

      END
