      SUBROUTINE slRFCO (HM, TDK, PMB, RH, WL, PHI, TLR, EPS,
     :                      REFA, REFB)
*+
*     - - - - - -
*      R F C O
*     - - - - - -
*
*  Determine constants A and B in atmospheric refraction
*  model dZ = A tan Z + B tan**3 Z.
*
*  Z is the "observed" zenith distance (i.e. affected by
*  refraction) and dZ is what to add to Z to give the "topocentric"
*  (i.e. in vacuo) zenith distance.
*
*  The constants are such that the model agrees precisely with
*  the full integration performed by the slRFRO routine at
*  zenith distances arctan(1) and arctan(4).
*
*  Given:
*    HM    dp    height of the observer above sea level (metre)
*    TDK   dp    ambient temperature at the observer (deg K)
*    PMB   dp    pressure at the observer (millibar)
*    RH    dp    relative humidity at the observer (range 0-1)
*    WL    dp    effective wavelength of the source (micrometre)
*    PHI   dp    latitude of the observer (radian, astronomical)
*    TLR   dp    temperature lapse rate in the troposphere (degK/metre)
*    EPS   dp    precision required to terminate iteration (radian)
*
*  Returned:
*    REFA  dp    tan Z coefficient (radian)
*    REFB  dp    tan**3 Z coefficient (radian)
*
*  Called:  slRFRO
*
*  Typical values for the TLR and EPS arguments might be 0.0065D0 and
*  1D-10 respectively.
*
*  The radio refraction is chosen by specifying WL > 100 micrometres.
*
*  Relative to the comprehensive refraction model used by this
*  routine, the simple A tan Z + B tan**3 Z formula achieves
*  0.5 arcsec accuracy for ZD < 80 deg, 0.01 arcsec accuracy for
*  ZD < 60 deg, and 0.001 arcsec accuracy for ZD < 45 deg.
*
*  P.T.Wallace   Starlink   9 December 1993
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION HM,TDK,PMB,RH,WL,PHI,TLR,EPS,REFA,REFB

      DOUBLE PRECISION ATN1,ATN4,R1,R2

*  Sample zenith distances: arctan(1) and arctan(4)
      PARAMETER (ATN1=0.7853981633974483D0,
     :           ATN4=1.325817663668033D0)



*  Determine refraction for the two sample zenith distances
      CALL slRFRO(ATN1,HM,TDK,PMB,RH,WL,PHI,TLR,EPS,R1)
      CALL slRFRO(ATN4,HM,TDK,PMB,RH,WL,PHI,TLR,EPS,R2)

*  Solve for refraction constants
      REFA = (64D0*R1-R2)/60D0
      REFB = (R2-4D0*R1)/60D0

      END
