      DOUBLE PRECISION FUNCTION slDAT (UTC)
*+
*     - - - -
*      D A T
*     - - - -
*
*  Increment to be applied to Coordinated Universal Time UTC to give
*  International Atomic Time TAI (double precision)
*
*  Given:
*     UTC      d      UTC date as a modified JD (JD-2400000.5)
*
*  Result:  TAI-UTC in seconds
*
*  Notes:
*
*  1  The UTC is specified to be a date rather than a time to indicate
*     that care needs to be taken not to specify an instant which lies
*     within a leap second.  Though in most cases UTC can include the
*     fractional part, correct behaviour on the day of a leap second
*     can only be guaranteed up to the end of the second 23:59:59.
*
*  2  Pre 1972 January 1 a fixed value of 10 sec is returned.
*
*     :-----------------------------------------:
*     :                                         :
*     :                IMPORTANT                :
*     :                                         :
*     :  This routine must be updated on each   :
*     :     occasion that a leap second is      :
*     :                announced                :
*     :                                         :
*     :  Latest leap second:  1996 January 1    :
*     :                                         :
*     :-----------------------------------------:
*
*  P.T.Wallace   Starlink   14 November 1995
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION UTC

      DOUBLE PRECISION DT



      DT = 10D0

*  1972 July 1
      IF (UTC.GE.41499D0) DT=11D0

*  1973 January 1
      IF (UTC.GE.41683D0) DT=12D0

*  1974 January 1
      IF (UTC.GE.42048D0) DT=13D0

*  1975 January 1
      IF (UTC.GE.42413D0) DT=14D0

*  1976 January 1
      IF (UTC.GE.42778D0) DT=15D0

*  1977 January 1
      IF (UTC.GE.43144D0) DT=16D0

*  1978 January 1
      IF (UTC.GE.43509D0) DT=17D0

*  1979 January 1
      IF (UTC.GE.43874D0) DT=18D0

*  1980 January 1
      IF (UTC.GE.44239D0) DT=19D0

*  1981 July 1
      IF (UTC.GE.44786D0) DT=20D0

*  1982 July 1
      IF (UTC.GE.45151D0) DT=21D0

*  1983 July 1
      IF (UTC.GE.45516D0) DT=22D0

*  1985 July 1
      IF (UTC.GE.46247D0) DT=23D0

*  1988 January 1
      IF (UTC.GE.47161D0) DT=24D0

*  1990 January 1
      IF (UTC.GE.47892D0) DT=25D0

*  1991 January 1
      IF (UTC.GE.48257D0) DT=26D0

*  1992 July 1
      IF (UTC.GE.48804D0) DT=27D0

*  1993 July 1
      IF (UTC.GE.49169D0) DT=28D0

*  1994 July 1
      IF (UTC.GE.49534D0) DT=29D0

*  1996 January 1
      IF (UTC.GE.50083D0) DT=30D0

* - - - - - - - - - - - - - - - - - - - - - - - - - - *
*  (Add a new pair of lines to the above set on each  *
*      occasion that a leap second is announced)      *
* - - - - - - - - - - - - - - - - - - - - - - - - - - *

      slDAT=DT

      END
