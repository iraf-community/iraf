      SUBROUTINE slPRBN (BEP0, BEP1, RMATP)
*+
*     - - - - - -
*      P R B N
*     - - - - - -
*
*  Generate the matrix of precession between two epochs,
*  using the old, pre-IAU1976, Bessel-Newcomb model, using
*  Kinoshita's formulation (double precision)
*
*  Given:
*     BEP0    dp         beginning Besselian epoch
*     BEP1    dp         ending Besselian epoch
*
*  Returned:
*     RMATP  dp(3,3)    precession matrix
*
*  The matrix is in the sense   V(BEP1)  =  RMATP * V(BEP0)
*
*  Reference:
*     Kinoshita, H. (1975) 'Formulas for precession', SAO Special
*     Report No. 364, Smithsonian Institution Astrophysical
*     Observatory, Cambridge, Massachusetts.
*
*  Called:  slDEUL
*
*  P.T.Wallace   Starlink   30 December 1992
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION BEP0,BEP1,RMATP(3,3)

*  Arc seconds to radians
      DOUBLE PRECISION AS2R
      PARAMETER (AS2R=0.4848136811095359949D-05)

      DOUBLE PRECISION BIGT,T,TAS2R,W,ZETA,Z,THETA



*  Interval between basic epoch B1850.0 and beginning epoch in TC
      BIGT = (BEP0-1850D0)/100D0

*  Interval over which precession required, in tropical centuries
      T = (BEP1-BEP0)/100D0

*  Euler angles
      TAS2R = T*AS2R
      W = 2303.5548D0+(1.39720D0+0.000059D0*BIGT)*BIGT

      ZETA = (W+(0.30242D0-0.000269D0*BIGT+0.017996D0*T)*T)*TAS2R
      Z = (W+(1.09478D0+0.000387D0*BIGT+0.018324D0*T)*T)*TAS2R
      THETA = (2005.1125D0+(-0.85294D0-0.000365D0*BIGT)*BIGT+
     :        (-0.42647D0-0.000365D0*BIGT-0.041802D0*T)*T)*TAS2R

*  Rotation matrix
      CALL slDEUL('ZYZ',-ZETA,THETA,-Z,RMATP)

      END
