      SUBROUTINE slATMD (TDK, PMB, RH, WL1, A1, B1, WL2, A2, B2)
*+
*     - - - - - - -
*      A T M D
*     - - - - - - -
*
*  Apply atmospheric-dispersion adjustments to refraction coefficients.
*
*  Given:
*     TDK       d       ambient temperature, degrees K
*     PMB       d       ambient pressure, millibars
*     RH        d       ambient relative humidity, 0-1
*     WL1       d       reference wavelength, micrometre (0.4D0 recommended)
*     A1        d       refraction coefficient A for wavelength WL1 (radians)
*     B1        d       refraction coefficient B for wavelength WL1 (radians)
*     WL2       d       wavelength for which adjusted A,B required
*
*  Returned:
*     A2        d       refraction coefficient A for wavelength WL2 (radians)
*     B2        d       refraction coefficient B for wavelength WL2 (radians)
*
*  Notes:
*
*  1  To use this routine, first call slRFCO specifying WL1 as the
*     wavelength.  This yields refraction coefficients A1,B1, correct
*     for that wavelength.  Subsequently, calls to slATMD specifying
*     different wavelengths will produce new, slightly adjusted
*     refraction coefficients which apply to the specified wavelength.
*
*  2  Most of the atmospheric dispersion happens between 0.7 micrometre
*     and the UV atmospheric cutoff, and the effect increases strongly
*     towards the UV end.  For this reason a blue reference wavelength
*     is recommended, for example 0.4 micrometres.
*
*  3  The accuracy, for this set of conditions:
*
*        height above sea level    2000 m
*                      latitude    29 deg
*                      pressure    793 mB
*                   temperature    17 degC
*                      humidity    50%
*                    lapse rate    0.0065 degC/m
*          reference wavelength    0.4 micrometre
*                star elevation    15 deg
*
*     is about 2.5 mas RMS between 0.3 and 1.0 micrometres, and stays
*     within 4 mas for the whole range longward of 0.3 micrometres
*     (compared with a total dispersion from 0.3 to 20.0 micrometres
*     of about 11 arcsec).  These errors are typical for ordinary
*     conditions and the given elevation;  in extreme conditions values
*     a few times this size may occur, while at higher elevations the
*     errors become much smaller.
*
*  4  If either wavelength exceeds 100 micrometres, the radio case
*     is assumed and the returned refraction coefficients are the
*     same as the given ones.
*
*  5  The algorithm consists of calculation of the refractivity of the
*     air at the observer for the two wavelengths, using the methods
*     of the slRFRO routine, and then scaling of the two refraction
*     coefficients according to classical refraction theory.  This
*     amounts to scaling the A coefficient in proportion to (n-1) and
*     the B coefficient almost in the same ratio (see R.M.Green,
*     "Spherical Astronomy", Cambridge University Press, 1985).
*
*  P.T.Wallace   Starlink   6 October 1995
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION TDK,PMB,RH,WL1,A1,B1,WL2,A2,B2

      DOUBLE PRECISION F,TDKOK,PMBOK,RHOK,
     :                 PSAT,PWO,W1,WLOK,WLSQ,W2,DN1,DN2


*  Check for radio wavelengths
      IF (WL1.GT.100D0.OR.WL2.GT.100D0) THEN

*     Radio: no dispersion
         A2 = A1
         B2 = B1
      ELSE

*     Optical: keep arguments within safe bounds
         TDKOK = MIN(MAX(TDK,100D0),500D0)
         PMBOK = MIN(MAX(PMB,0D0),10000D0)
         RHOK = MIN(MAX(RH,0D0),1D0)

*     Atmosphere parameters at the observer
         PSAT = 10D0**(-8.7115D0+0.03477D0*TDKOK)
         PWO = RHOK*PSAT
         W1 = 11.2684D-6*PWO

*     Refractivity at the observer for first wavelength
         WLOK = MAX(WL1,0.1D0)
         WLSQ = WLOK*WLOK
         W2 = 77.5317D-6+(0.43909D-6+0.00367D-6/WLSQ)/WLSQ
         DN1 = (W2*PMBOK-W1)/TDKOK

*     Refractivity at the observer for second wavelength
         WLOK = MAX(WL2,0.1D0)
         WLSQ = WLOK*WLOK
         W2 = 77.5317D-6+(0.43909D-6+0.00367D-6/WLSQ)/WLSQ
         DN2 = (W2*PMBOK-W1)/TDKOK

*     Scale the refraction coefficients (see Green 4.31, p93)
         IF (DN1.NE.0D0) THEN
            F = DN2/DN1
            A2 = A1*F
            B2 = B1*F
            IF (DN1.NE.A1) B2=B2*(1D0+DN1*(DN1-DN2)/(2D0*(DN1-A1)))
         ELSE
            A2 = A1
            B2 = B1
         END IF
      END IF

      END
