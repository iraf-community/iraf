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
      PROGRAM TRFFT2
      IMPLICIT NONE
C
      INTEGER I, J, L, LDIM, M, LENSAV, IER, LENWRK
      PARAMETER(L=100, M=100, LDIM=L) 
C     PARAMETER(LENSAV= L + 3*M +   INT(LOG(REAL(L))/LOG(2.))
C    .                          + 2*INT(LOG(REAL(M))/LOG(2.)) + 12)
      PARAMETER(LENSAV= 430)
      PARAMETER(LENWRK=(L+1)*M)
      REAL R(L,M), RCOPY(L,M)
      REAL WSAVE(LENSAV), WORK(LENWRK), DIFF
C
C --- IDENTIFY TEST AND INITIALIZE FFT 
C
      WRITE(6,*) 'PROGRAM TRFFT2 AND RELATED MESSAGES:'
      CALL RFFT2I (L,M,WSAVE,LENSAV,IER)
      IF (IER.NE.0) THEN
         WRITE(6,*) 'ERROR ',IER,' IN ROUTINE RFFT2I'
         STOP
      END IF
C
C --- GENERATE TEST MATRIX FOR FORWARD-BACKWARD TEST
C
      CALL RANDOM_SEED()
      CALL RANDOM_NUMBER(R)
      RCOPY = R
C
C --- PERFORM FORWARD TRANSFORM
C
      CALL RFFT2F (LDIM,L,M,R,
     .             WSAVE,LENSAV,WORK,LENWRK,IER)
      IF (IER.NE.0) THEN
         WRITE(6,*) 'ERROR ',IER,' IN ROUTINE RFFT2F !'
         STOP
      END IF
C
C --- PERFORM BACKWARD TRANSFORM
C
      CALL RFFT2B (LDIM,L,M,R,
     .             WSAVE,LENSAV,WORK,LENWRK,IER)
      IF (IER.NE.0) THEN
         WRITE(6,*) 'ERROR ',IER,' IN ROUTINE RFFT2B !'
         STOP
      END IF
C
C
C --- PRINT TEST RESULTS
      IER = 0
      DIFF = 0.
      DO I=1,L
      DO J=1,M
         DIFF = MAX(DIFF,ABS(R(I,J)-RCOPY(I,J)))
      END DO
      END DO
      WRITE(6,*) 'RFFT2 FORWARD-BACKWARD MAX ERROR =',DIFF
C
C --- GENERATE TEST MATRIX FOR BACKWARD-FORWARD TEST
C
      CALL RANDOM_SEED()
      CALL RANDOM_NUMBER(R)
      RCOPY = R
C
C --- PERFORM BACKWARD TRANSFORM
C
      CALL RFFT2B (LDIM,L,M,R,
     .             WSAVE,LENSAV,WORK,LENWRK,IER)
      IF (IER.NE.0) THEN
         WRITE(6,*) 'ERROR ',IER,' IN ROUTINE RFFT2B !'
         STOP
      END IF
C
C --- PERFORM FORWARD TRANSFORM
C
      CALL RFFT2F (LDIM,L,M,R,
     .             WSAVE,LENSAV,WORK,LENWRK,IER)
      IF (IER.NE.0) THEN
         WRITE(6,*) 'ERROR ',IER,' IN ROUTINE RFFT2F !'
         STOP
      END IF
C
C
C --- PRINT TEST RESULTS
      IER = 0
      DIFF = 0.
      DO I=1,L
      DO J=1,M
         DIFF = MAX(DIFF,ABS(R(I,J)-RCOPY(I,J)))
      END DO
      END DO
      WRITE(6,*) 'RFFT2 BACKWARD-FORWARD MAX ERROR =',DIFF
C
      WRITE(6,'(A,/)') ' END PROGRAM TRFFT2 AND RELATED MESSAGES'
      STOP
      END
