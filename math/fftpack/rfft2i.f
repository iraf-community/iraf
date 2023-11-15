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
      SUBROUTINE RFFT2I (L, M, WSAVE, LENSAV, IER)
      INTEGER L, M, LENSAV, IER
      INTEGER LWSAV,MWSAV,MMSAV
      REAL WSAVE(LENSAV)
C
C INITIALIZE IER
C
      IER = 0
C
C VERIFY LENSAV
C
      LWSAV =   L+INT(LOG(REAL(L))/LOG(2.))+4
      MWSAV =   2*M+INT(LOG(REAL(M))/LOG(2.))+4
      MMSAV =   M+INT(LOG(REAL(M))/LOG(2.))+4
      IF (LENSAV .LT. LWSAV+MWSAV+MMSAV) THEN
        IER = 2
        CALL XERFFT ('RFFT2I', 4)
        GO TO 100
      ENDIF
C
      CALL RFFTMI (L, WSAVE(1), LWSAV, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('RFFT2I',-5)
        GO TO 100
      ENDIF
      CALL CFFTMI (M, WSAVE(LWSAV+1),MWSAV,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('RFFT2I',-5)
      ENDIF
C
      CALL RFFTMI (M,WSAVE(LWSAV+MWSAV+1),MMSAV, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('RFFT2I',-5)
        GO TO 100
      END IF
C
  100 CONTINUE
      RETURN
      END
