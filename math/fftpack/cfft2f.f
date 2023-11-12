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
      SUBROUTINE CFFT2F (LDIM, L, M, C, WSAVE, LENSAV,
     1                     WORK, LENWRK, IER)
      INTEGER L, M, LDIM, LENSAV, LENWRK, IER
      COMPLEX C(LDIM,M)
      REAL WSAVE(LENSAV), WORK(LENWRK)
C
C Initialize error return
C
      IER = 0
C
      IF (L .GT. LDIM) THEN
        IER = 5
        CALL XERFFT ('CFFT2F', -2)
        GO TO 100
      ELSEIF (LENSAV .LT. 2*L + INT(LOG(REAL(L))/LOG(2.)) + 
     1                    2*M + INT(LOG(REAL(M))/LOG(2.)) +8) THEN
        IER = 2
        CALL XERFFT ('CFFT2F', 6)
        GO TO 100
      ELSEIF (LENWRK .LT. 2*L*M) THEN
        IER = 3
        CALL XERFFT ('CFFT2F', 8)
        GO TO 100
      ENDIF
C
C Transform X lines of C array
      IW = 2*L+INT(LOG(REAL(L))/LOG(2.)) + 3
      CALL CFFTMF(L, 1, M, LDIM, C, (L-1) + LDIM*(M-1) +1,
     1     WSAVE(IW), 2*M + INT(LOG(REAL(M))/LOG(2.)) + 4, 
     2     WORK, 2*L*M, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('CFFT2F',-5)
        GO TO 100
      ENDIF
C
C Transform Y lines of C array
      IW = 1
      CALL CFFTMF (M, LDIM, L, 1, C, (M-1)*LDIM + L,
     1     WSAVE(IW), 2*L + INT(LOG(REAL(L))/LOG(2.)) + 4, 
     2     WORK, 2*M*L, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('CFFT2F',-5)
      ENDIF
C
  100 CONTINUE
      RETURN
      END
