      DOUBLE PRECISION FUNCTION slEQEX (DATE)
*+
*     - - - - - -
*      E Q E X
*     - - - - - -
*
*  Equation of the equinoxes  (IAU 1994, double precision)
*
*  Given:
*     DATE    dp      TDB (loosely ET) as Modified Julian Date
*                                          (JD-2400000.5)
*
*  The result is the equation of the equinoxes (double precision)
*  in radians:
*
*     Greenwich apparent ST = GMST + slEQEX
*
*  References:  IAU Resolution C7, Recommendation 3 (1994)
*               Capitaine, N. & Gontier, A.-M., Astron. Astrophys.,
*               275, 645-650 (1993)
*               
*  Called:  slNUTC
*
*  Patrick Wallace   Starlink   21 November 1994
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION DATE

*  Turns to arc seconds and arc seconds to radians
      DOUBLE PRECISION T2AS,AS2R
      PARAMETER (T2AS=1296000D0,
     :           AS2R=0.4848136811095359949D-05)

      DOUBLE PRECISION T,OM,DPSI,DEPS,EPS0



*  Interval between basic epoch J2000.0 and current epoch (JC)
      T=(DATE-51544.5D0)/36525D0

*  Longitude of the mean ascending node of the lunar orbit on the
*   ecliptic, measured from the mean equinox of date
      OM=AS2R*(450160.280D0+(-5D0*T2AS-482890.539D0
     :         +(7.455D0+0.008D0*T)*T)*T)

*  Nutation
      CALL slNUTC(DATE,DPSI,DEPS,EPS0)

*  Equation of the equinoxes
      slEQEX=DPSI*COS(EPS0)+AS2R*(0.00264D0*SIN(OM)+
     :                               0.000063D0*SIN(OM+OM))

      END
