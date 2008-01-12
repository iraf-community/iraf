      SUBROUTINE slPRCS (SYSTEM, EP0, EP1, RA, DC)
*+
*     - - - - - - -
*      P R C E
*     - - - - - - -
*
*  Precession - either FK4 (Bessel-Newcomb, pre IAU 1976) or
*  FK5 (Fricke, post IAU 1976) as required.
*
*  Given:
*     SYSTEM     int    precession to be applied: 1 = FK4 or 2 = FK5
*     EP0,EP1    dp     starting and ending epoch
*     RA,DC      dp     RA,Dec, mean equator & equinox of epoch EP0
*
*  Returned:
*     RA,DC      dp     RA,Dec, mean equator & equinox of epoch EP1
*
*  Called:    slDA2P, slPRBN, slPREC, slDS2C,
*             slDMXV, slDC2S
*
*  Notes:
*
*     1)  Lowercase characters in SYSTEM are acceptable.
*
*     2)  The epochs are Besselian if SYSTEM=FK4 and Julian if FK5.
*         For example, to precess coordinates in the old system from
*         equinox 1900.0 to 1950.0 the call would be:
*             CALL slPRCS (1, 1900D0, 1950D0, RA, DC)
*
*     3)  This routine will NOT correctly convert between the old and
*         the new systems - for example conversion from B1950 to J2000.
*         For these purposes see slFK45, slFK54, slF45Z and
*         slF54Z.
*
*     4)  If an invalid SYSTEM is supplied, values of -99D0,-99D0 will
*         be returned for both RA and DC.
*
*  P.T.Wallace   Starlink   20 April 1990
*-

      IMPLICIT NONE

      INTEGER SYSTEM
      DOUBLE PRECISION EP0,EP1,RA,DC

      DOUBLE PRECISION PM(3,3),V1(3),V2(3)

      DOUBLE PRECISION slDA2P


*  Convert to uppercase and validate SYSTEM
      IF (SYSTEM.NE.1.AND.SYSTEM.NE.2) THEN
         RA=-99D0
         DC=-99D0
      ELSE

*     Generate appropriate precession matrix
         IF (SYSTEM.EQ.1) THEN
            CALL slPRBN(EP0,EP1,PM)
         ELSE
            CALL slPREC(EP0,EP1,PM)
         END IF

*     Convert RA,Dec to x,y,z
         CALL slDS2C(RA,DC,V1)

*     Precess
         CALL slDMXV(PM,V1,V2)

*     Back to RA,Dec
         CALL slDC2S(V2,RA,DC)
         RA=slDA2P(RA)

      END IF

      END
