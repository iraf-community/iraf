      SUBROUTINE slC62S (V, A, B, R, AD, BD, RD)
*+
*     - - - - - -
*      C 6 2 S
*     - - - - - -
*
*  Conversion of position & velocity in Cartesian coordinates
*  to spherical coordinates (single precision)
*
*  Given:
*     V     r(6)   Cartesian position & velocity vector
*
*  Returned:
*     A     r      longitude (radians)
*     B     r      latitude (radians)
*     R     r      radial coordinate
*     AD    r      longitude derivative (radians per unit time)
*     BD    r      latitude derivative (radians per unit time)
*     RD    r      radial derivative
*
*  P.T.Wallace   Starlink   June 1989
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      REAL V(6),A,B,R,AD,BD,RD

      REAL X,Y,Z,XD,YD,ZD,RXY2,RXY,R2,XYP



*  Components of vector
      X=V(1)
      Y=V(2)
      Z=V(3)
      XD=V(4)
      YD=V(5)
      ZD=V(6)

*  Component of R in XY plane squared (with crude precaution
*   against polar problems and daft units)
      RXY2=MAX(X*X+Y*Y,(XD*XD+YD*YD+ZD*ZD)/1E30)

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
