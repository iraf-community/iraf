      SUBROUTINE slECMA (DATE, RMAT)
*+
*     - - - - - -
*      E C M A
*     - - - - - -
*
*  Form the equatorial to ecliptic rotation matrix - IAU 1980 theory
*  (double precision)
*
*  Given:
*     DATE     dp         TDB (loosely ET) as Modified Julian Date
*                                            (JD-2400000.5)
*  Returned:
*     RMAT     dp(3,3)    matrix
*
*  Reference:
*     Murray,C.A., Vectorial Astrometry, section 4.3.
*
*  Note:
*    The matrix is in the sense   V(ecl)  =  RMAT * V(equ);  the
*    equator, equinox and ecliptic are mean of date.
*
*  Called:  slDEUL
*
*  P.T.Wallace   Starlink   23 August 1996
*
*  Copyright (C) 1996 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION DATE,RMAT(3,3)

*  Arc seconds to radians
      DOUBLE PRECISION AS2R
      PARAMETER (AS2R=0.484813681109535994D-5)

      DOUBLE PRECISION T,EPS0



*  Interval between basic epoch J2000.0 and current epoch (JC)
      T = (DATE-51544.5D0)/36525D0

*  Mean obliquity
      EPS0 = AS2R*
     :   (84381.448D0+(-46.8150D0+(-0.00059D0+0.001813D0*T)*T)*T)

*  Matrix
      CALL slDEUL('X',EPS0,0D0,0D0,RMAT)

      END
