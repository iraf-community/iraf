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
      SUBROUTINE CFFT2I (L, M, WSAVE, LENSAV, IER)
      INTEGER L, M, IER
      REAL WSAVE(LENSAV)
C
C Initialize error return
C
      IER = 0
C
      IF (LENSAV .LT. 2*L + INT(LOG(REAL(L))/LOG(2.)) + 
     1                    2*M + INT(LOG(REAL(M))/LOG(2.)) +8) THEN
        IER = 2
        CALL XERFFT ('CFFT2I', 4)
        GO TO 100
      ENDIF
C
      CALL CFFTMI (L, WSAVE(1), 2*L + INT(LOG(REAL(L))/LOG(2.)) + 4, 
     1 IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('CFFT2I',-5)
        GO TO 100
      ENDIF
      CALL CFFTMI (M, WSAVE(2*L+INT(LOG(REAL(L))/LOG(2.)) + 3), 
     1            2*M + INT(LOG(REAL(M))/LOG(2.)) + 4, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('CFFT2I',-5)
      ENDIF
C
  100 CONTINUE
      RETURN
      END
