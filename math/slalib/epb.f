      DOUBLE PRECISION FUNCTION slEPB (DATE)
*+
*     - - - -
*      E P B
*     - - - -
*
*  Conversion of Modified Julian Date to Besselian Epoch
*  (double precision)
*
*  Given:
*     DATE     dp       Modified Julian Date (JD - 2400000.5)
*
*  The result is the Besselian Epoch.
*
*  Reference:
*     Lieske,J.H., 1979. Astron.Astrophys.,73,282.
*
*  P.T.Wallace   Starlink   February 1984
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION DATE


      slEPB = 1900D0 + (DATE-15019.81352D0)/365.242198781D0

      END
