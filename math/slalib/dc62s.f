      SUBROUTINE slDC6S (V, A, B, R, AD, BD, RD)
*+
*     - - - - - -
*      D C 6 S
*     - - - - - -
*
*  Conversion of position & velocity in Cartesian coordinates
*  to spherical coordinates (double precision)
*
*  Given:
*     V     dp(6)   Cartesian position & velocity vector
*
*  Returned:
*     A     dp      longitude (radians)
*     B     dp      latitude (radians)
*     R     dp      radial coordinate
*     AD    dp      longitude derivative (radians per unit time)
*     BD    dp      latitude derivative (radians per unit time)
*     RD    dp      radial derivative
*
*  P.T.Wallace   Starlink   September 1984
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION V(6),A,B,R,AD,BD,RD

      DOUBLE PRECISION X,Y,Z,XD,YD,ZD,RXY2,RXY,R2,XYP



*  Components of vector
      X=V(1)
      Y=V(2)
      Z=V(3)
      XD=V(4)
      YD=V(5)
      ZD=V(6)

*  Component of R in XY plane squared (with crude precaution
*   against polar problems and daft units)
      RXY2=MAX(X*X+Y*Y,(XD*XD+YD*YD+ZD*ZD)/1D30)

*  Other useful functions
      RXY=SQRT(RXY2)
      R2=RXY2+Z*Z
      XYP=X*XD+Y*YD

*  Position in spherical coordinates
      A=ATAN2(Y,X)
      B=ATAN2(Z,RXY)
      R=SQRT(R2)

*  Velocity in spherical coordinates
      AD=(X*YD-Y*XD)/RXY2
      BD=(ZD*RXY2-Z*XYP)/(R2*RXY)
      RD=(XYP+Z*ZD)/R

      END
