      SUBROUTINE slAOP ( RAP, DAP, DATE, DUT, ELONGM, PHIM, HM,
     :                     XP, YP, TDK, PMB, RH, WL, TLR,
     :                     AOB, ZOB, HOB, DOB, ROB )
*+
*     - - - -
*      A O P
*     - - - -
*
*  Apparent to observed place, for sources distant from the solar
*  system.
*
*  Given:
*     RAP    d      geocentric apparent right ascension
*     DAP    d      geocentric apparent declination
*     DATE   d      UTC date/time (Modified Julian Date, JD-2400000.5)
*     DUT    d      delta UT:  UT1-UTC (UTC seconds)
*     ELONGM d      mean longitude of the observer (radians, east +ve)
*     PHIM   d      mean geodetic latitude of the observer (radians)
*     HM     d      observer's height above sea level (metres)
*     XP     d      polar motion x-coordinate (radians)
*     YP     d      polar motion y-coordinate (radians)
*     TDK    d      local ambient temperature (K; std=273.15D0)
*     PMB    d      local atmospheric pressure (mb; std=1013.25D0)
*     RH     d      local relative humidity (in the range 0D0-1D0)
*     WL     d      effective wavelength (micron, e.g. 0.55D0)
*     TLR    d      tropospheric lapse rate (K/metre, e.g. 0.0065D0)
*
*  Returned:
*     AOB    d      observed azimuth (radians: N=0,E=90)
*     ZOB    d      observed zenith distance (radians)
*     HOB    d      observed Hour Angle (radians)
*     DOB    d      observed Declination (radians)
*     ROB    d      observed Right Ascension (radians)
*
*  Notes:
*
*   1)  This routine returns zenith distance rather than elevation
*       in order to reflect the fact that no allowance is made for
*       depression of the horizon.
*
*   2)  The accuracy of the result is limited by the corrections for
*       refraction.  Providing the meteorological parameters are
*       known accurately and there are no gross local effects, the
*       predicted apparent RA,Dec should be within about 0.1 arcsec
*       for a zenith distance of less than 70 degrees.  Even at a
*       topocentric zenith distance of 90 degrees, the accuracy in
*       elevation should be better than 1 arcmin;  useful results
*       are available for a further 3 degrees, beyond which the
*       slRFRO routine returns a fixed value of the refraction.
*       The complementary routines slAOP (or slAOPQ) and slOAP
*       (or slOAPQ) are self-consistent to better than 1 micro-
*       arcsecond all over the celestial sphere.
*
*   3)  It is advisable to take great care with units, as even
*       unlikely values of the input parameters are accepted and
*       processed in accordance with the models used.
*
*   4)  "Apparent" place means the geocentric apparent right ascension
*       and declination, which is obtained from a catalogue mean place
*       by allowing for space motion, parallax, precession, nutation,
*       annual aberration, and the Sun's gravitational lens effect.  For
*       star positions in the FK5 system (i.e. J2000), these effects can
*       be applied by means of the slMAP etc routines.  Starting from
*       other mean place systems, additional transformations will be
*       needed;  for example, FK4 (i.e. B1950) mean places would first
*       have to be converted to FK5, which can be done with the
*       slFK45 etc routines.
*
*   5)  "Observed" Az,El means the position that would be seen by a
*       perfect theodolite located at the observer.  This is obtained
*       from the geocentric apparent RA,Dec by allowing for Earth
*       orientation and diurnal aberration, rotating from equator
*       to horizon coordinates, and then adjusting for refraction.
*       The HA,Dec is obtained by rotating back into equatorial
*       coordinates, using the geodetic latitude corrected for polar
*       motion, and is the position that would be seen by a perfect
*       equatorial located at the observer and with its polar axis
*       aligned to the Earth's axis of rotation (n.b. not to the
*       refracted pole).  Finally, the RA is obtained by subtracting
*       the HA from the local apparent ST.
*
*   6)  To predict the required setting of a real telescope, the
*       observed place produced by this routine would have to be
*       adjusted for the tilt of the azimuth or polar axis of the
*       mounting (with appropriate corrections for mount flexures),
*       for non-perpendicularity between the mounting axes, for the
*       position of the rotator axis and the pointing axis relative
*       to it, for tube flexure, for gear and encoder errors, and
*       finally for encoder zero points.  Some telescopes would, of
*       course, exhibit other properties which would need to be
*       accounted for at the appropriate point in the sequence.
*
*   7)  This routine takes time to execute, due mainly to the
*       rigorous integration used to evaluate the refraction.
*       For processing multiple stars for one location and time,
*       call slAOPA once followed by one call per star to slAOPQ.
*       Where a range of times within a limited period of a few hours
*       is involved, and the highest precision is not required, call
*       slAOPA once, followed by a call to slAOPT each time the
*       time changes, followed by one call per star to slAOPQ.
*
*   8)  The DATE argument is UTC expressed as an MJD.  This is,
*       strictly speaking, wrong, because of leap seconds.  However,
*       as long as the delta UT and the UTC are consistent there
*       are no difficulties, except during a leap second.  In this
*       case, the start of the 61st second of the final minute should
*       begin a new MJD day and the old pre-leap delta UT should
*       continue to be used.  As the 61st second completes, the MJD
*       should revert to the start of the day as, simultaneously,
*       the delta UTC changes by one second to its post-leap new value.
*
*   9)  The delta UT (UT1-UTC) is tabulated in IERS circulars and
*       elsewhere.  It increases by exactly one second at the end of
*       each UTC leap second, introduced in order to keep delta UT
*       within +/- 0.9 seconds.
*
*  10)  IMPORTANT -- TAKE CARE WITH THE LONGITUDE SIGN CONVENTION.
*       The longitude required by the present routine is east-positive,
*       in accordance with geographical convention (and right-handed).
*       In particular, note that the longitudes returned by the
*       slOBS routine are west-positive, following astronomical
*       usage, and must be reversed in sign before use in the present
*       routine.
*
*  11)  The polar coordinates XP,YP can be obtained from IERS
*       circulars and equivalent publications.  The maximum amplitude
*       is about 0.3 arcseconds.  If XP,YP values are unavailable,
*       use XP=YP=0D0.  See page B60 of the 1988 Astronomical Almanac
*       for a definition of the two angles.
*
*  12)  The height above sea level of the observing station, HM,
*       can be obtained from the Astronomical Almanac (Section J
*       in the 1988 edition), or via the routine slOBS.  If P,
*       the pressure in millibars, is available, an adequate
*       estimate of HM can be obtained from the expression
*
*             HM ~ -29.3D0*TSL*LOG(P/1013.25D0).
*
*       where TSL is the approximate sea-level air temperature in K
*       (see Astrophysical Quantities, C.W.Allen, 3rd edition,
*       section 52).  Similarly, if the pressure P is not known,
*       it can be estimated from the height of the observing
*       station, HM, as follows:
*
*             P ~ 1013.25D0*EXP(-HM/(29.3D0*TSL)).
*
*       Note, however, that the refraction is nearly proportional to the
*       pressure and that an accurate P value is important for precise
*       work.
*
*  13)  The azimuths etc produced by the present routine are with
*       respect to the celestial pole.  Corrections to the terrestrial
*       pole can be computed using slPLMO.
*
*  Called:  slAOPA, slAOPQ
*
*  Last revision:   2 December 2005
*
*  Copyright P.T.Wallace.   All rights reserved.
*
*  License:
*    This program is free software; you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation; either version 2 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program (see SLA_CONDITIONS); if not, write to the
*    Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
*    Boston, MA  02110-1301  USA
*
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION RAP,DAP,DATE,DUT,ELONGM,PHIM,HM,
     :                 XP,YP,TDK,PMB,RH,WL,TLR,AOB,ZOB,HOB,DOB,ROB

      DOUBLE PRECISION AOPRMS(14)


      CALL slAOPA(DATE,DUT,ELONGM,PHIM,HM,XP,YP,TDK,PMB,RH,WL,TLR,
     :               AOPRMS)
      CALL slAOPQ(RAP,DAP,AOPRMS,AOB,ZOB,HOB,DOB,ROB)

      END
