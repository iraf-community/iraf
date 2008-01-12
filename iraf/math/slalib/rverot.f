      REAL FUNCTION slRVER (PHI, RA, DA, ST)
*+
*     - - - - - - -
*      R V E R
*     - - - - - - -
*
*  Velocity component in a given direction due to Earth rotation
*  (single precision)
*
*  Given:
*     PHI     real    latitude of observing station (geodetic)
*     RA,DA   real    apparent RA,DEC
*     ST      real    local apparent sidereal time
*
*  PHI, RA, DEC and ST are all in radians.
*
*  Result:
*     Component of Earth rotation in direction RA,DA (km/s)
*
*  Sign convention:
*     The result is +ve when the observatory is receding from the
*     given point on the sky.
*
*  Accuracy:
*     The simple algorithm used assumes a spherical Earth, of
*     a radius chosen to give results accurate to about 0.0005 km/s
*     for observing stations at typical latitudes and heights.  For
*     applications requiring greater precision, use the routine
*     slPVOB.
*
*  P.T.Wallace   Starlink   20 July 1994
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      REAL PHI,RA,DA,ST

*  Nominal mean sidereal speed of Earth equator in km/s (the actual
*  value is about 0.4651)
      REAL ESPEED
      PARAMETER (ESPEED=0.4655)


      slRVER=ESPEED*COS(PHI)*SIN(ST-RA)*COS(DA)

      END
