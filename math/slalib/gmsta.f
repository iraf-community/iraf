      DOUBLE PRECISION FUNCTION slGMSA (DATE, UT)
*+
*     - - - - - -
*      G M S A
*     - - - - - -
*
*  Conversion from universal time to sidereal time with
*  rounding errors minimized.
*
*  double precision
*
*  Given:
*    DATE    d      UT1 date (MJD: integer part of JD-2400000.5))
*    UT      d      UT1 time (fraction of a day)
*
*  The result is the Greenwich mean sidereal time (double
*  precision, radians).
*
*  The algorithm is derived from the IAU 1982 expression (see page
*  S15 of the 1984 Astronomical Almanac).
*
*  There is no restriction on how the UT is apportioned between the
*  DATE and UT arguments.  Either of the two arguments could, for
*  example, be zero and the entire date+time supplied in the other.
*  However, the routine is designed to deliver maximum accuracy when
*  the DATE argument is a whole number and the UT lies in the range
*  0 to 1.
*
*  See also the routine slGMST, which accepts the UT as a single
*  argument.  Compared with slGMST, the extra numerical precision
*  delivered by the present routine is unlikely to be important in
*  an absolute sense, but may be useful when critically comparing
*  algorithms and in applications where two sidereal times close
*  together are differenced.
*
*  Called:  slDA2P
*
*  P.T.Wallace   Starlink   14 September 1995
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION DATE,UT

      DOUBLE PRECISION slDA2P

      DOUBLE PRECISION D2PI,S2R
      PARAMETER (D2PI=6.283185307179586476925286766559D0,
     :           S2R=7.272205216643039903848711535369D-5)

      DOUBLE PRECISION F,D,T



*  Fractional part of date (if any)
      F=DMOD(DATE,1D0)

*  Days from fundamental epoch J2000 to 0h UT on this date
      D=DATE-51544.5D0

*  Julian centuries from fundamental epoch J2000 to this UT
      T=(D+UT)/36525D0

*  GMST at this UT
      slGMSA=slDA2P(S2R*(24110.54841D0
     :                         +86636.555367909D0*UT
     :                           +236.555367909D0*D
     :                         +86400D0*F
     :                      +(0.093104D0-6.2D-6*T)*T*T))

      END
