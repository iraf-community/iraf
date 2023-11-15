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
      PROGRAM TCFFT2
      IMPLICIT NONE
C
      INTEGER I, J, L, LDIM, M, LENSAV, IER, LENWRK
      PARAMETER(L=100, M=100, LDIM=L) 
C     PARAMETER(LENSAV=2*(L+M) + INT(LOG(REAL(L))/LOG(2.))
C    .                         + INT(LOG(REAL(M))/LOG(2.)) + 8)
      PARAMETER(LENSAV=420)
      PARAMETER(LENWRK=2*L*M)
      COMPLEX C(L,M)
      REAL RR(L,M), RI(L,M)
      REAL WSAVE(LENSAV), WORK(LENWRK), DIFF
C
C --- IDENTIFY TEST AND INITIALIZE FFT 
C
      WRITE(6,*) 'PROGRAM TCFFT2 AND RELATED MESSAGES:'
      CALL CFFT2I (L,M,WSAVE,LENSAV,IER)
      IF (IER.NE.0) THEN
         WRITE(6,*) 'ERROR ',IER,' IN ROUTINE CFFT2I'
         STOP
      END IF
C
C --- GENERATE TEST MATRIX FOR FORWARD-BACKWARD TEST
C
      CALL RANDOM_SEED()
      CALL RANDOM_NUMBER(RR)
      CALL RANDOM_NUMBER(RI)
      C = CMPLX(RR,RI)
C
C --- PERFORM FORWARD TRANSFORM
C
      CALL CFFT2F (LDIM,L,M,C,
     .             WSAVE,LENSAV,WORK,LENWRK,IER)
      IF (IER.NE.0) THEN
         WRITE(6,*) 'ERROR ',IER,' IN ROUTINE CFFT2F !'
         STOP
      END IF
C
C --- PERFORM BACKWARD TRANSFORM
C
      CALL CFFT2B (LDIM,L,M,C,
     .             WSAVE,LENSAV,WORK,LENWRK,IER)
      IF (IER.NE.0) THEN
         WRITE(6,*) 'ERROR ',IER,' IN ROUTINE CFFT2B !'
         STOP
      END IF
C
C
C --- PRINT TEST RESULTS
      IER = 0
      DIFF = 0.
      DO I=1,L
      DO J=1,M
         DIFF = MAX(DIFF,ABS(C(I,J)-CMPLX(RR(I,J),RI(I,J))))
      END DO
      END DO
      WRITE(6,*) 'CFFT2 FORWARD-BACKWARD MAX ERROR =',DIFF
C
C --- GENERATE TEST MATRIX FOR BACKWARD-FORWARD TEST
C
      CALL RANDOM_SEED()
      CALL RANDOM_NUMBER(RR)
      CALL RANDOM_NUMBER(RI)
      C = CMPLX(RR,RI)
C
C --- PERFORM BACKWARD TRANSFORM
C
      CALL CFFT2B (LDIM,L,M,C,
     .             WSAVE,LENSAV,WORK,LENWRK,IER)
      IF (IER.NE.0) THEN
         WRITE(6,*) 'ERROR ',IER,' IN ROUTINE CFFT2B !'
         STOP
      END IF
C
C --- PERFORM FORWARD TRANSFORM
C
      CALL CFFT2F (LDIM,L,M,C,
     .             WSAVE,LENSAV,WORK,LENWRK,IER)
      IF (IER.NE.0) THEN
         WRITE(6,*) 'ERROR ',IER,' IN ROUTINE CFFT2F !'
         STOP
      END IF
C
C
C --- PRINT TEST RESULTS
      IER = 0
      DIFF = 0.
      DO I=1,L
      DO J=1,M
         DIFF = MAX(DIFF,ABS(C(I,J)-CMPLX(RR(I,J),RI(I,J))))
      END DO
      END DO
      WRITE(6,*) 'CFFT2 BACKWARD-FORWARD MAX ERROR =',DIFF
C
      WRITE(6,'(A,/)') ' END PROGRAM TCFFT2 AND RELATED MESSAGES'
      STOP
      END
