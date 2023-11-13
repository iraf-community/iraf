C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C     *                                                               *
C     *                  copyright (c) 2011 by UCAR                   *
C     *                                                               *
C     *       University Corporation for Atmospheric Research         *
C     *                                                               *
C     *                      all rights reserved                      *
C     *                                                               *
C     *                     FFTPACK  version 5.1                      *
C     *                                                               *
C     *                 A Fortran Package of Fast Fourier             *
C     *                                                               *
C     *                Subroutines and Example Programs               *
C     *                                                               *
C     *                             by                                *
C     *                                                               *
C     *               Paul Swarztrauber and Dick Valent               *
C     *                                                               *
C     *                             of                                *
C     *                                                               *
C     *         the National Center for Atmospheric Research          *
C     *                                                               *
C     *                Boulder, Colorado  (80307)  U.S.A.             *
C     *                                                               *
C     *                   which is sponsored by                       *
C     *                                                               *
C     *              the National Science Foundation                  *
C     *                                                               *
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      SUBROUTINE COST1I (N, WSAVE, LENSAV, IER)
      INTEGER    N, LENSAV, IER
      REAL       WSAVE(LENSAV)
C
      IER = 0
C
      IF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('COST1I', 3)
        GO TO 300
      ENDIF
C
      IF (N .LE. 3) RETURN
      NM1 = N-1
      NP1 = N+1
      NS2 = N/2
      PI = 4.*ATAN(1.)
      DT = PI/FLOAT(NM1)
      FK = 0.
      DO 101 K=2,NS2
         KC = NP1-K
         FK = FK+1.
         WSAVE(K) = 2.*SIN(FK*DT)
         WSAVE(KC) = 2.*COS(FK*DT)
  101 CONTINUE
      LNSV = NM1 + INT(LOG(REAL(NM1))/LOG(2.)) +4
      CALL RFFT1I (NM1, WSAVE(N+1), LNSV, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COST1I',-5)
      ENDIF
  300 CONTINUE
      RETURN
      END
