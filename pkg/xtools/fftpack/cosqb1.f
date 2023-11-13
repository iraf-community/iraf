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
      SUBROUTINE COSQB1 (N,INC,X,WSAVE,WORK,IER)
      DIMENSION       X(INC,*)     ,WSAVE(*)     ,WORK(*)
      IER = 0
      NS2 = (N+1)/2
      NP2 = N+2
      DO 101 I=3,N,2
         XIM1 = X(1,I-1)+X(1,I)
         X(1,I) = .5*(X(1,I-1)-X(1,I))
         X(1,I-1) = .5*XIM1
  101 CONTINUE
      X(1,1) = .5*X(1,1)
      MODN = MOD(N,2)
      IF (MODN .NE. 0) GO TO 302
      X(1,N) = .5*X(1,N)
  302 LENX = INC*(N-1)  + 1
      LNSV = N + INT(LOG(REAL(N))/LOG(2.)) + 4
      LNWK = N
C
      CALL RFFT1B(N,INC,X,LENX,WSAVE(N+1),LNSV,WORK,LNWK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COSQB1',-5)
        GO TO 400
      ENDIF
C
      DO 102 K=2,NS2
         KC = NP2-K
         WORK(K) = WSAVE(K-1)*X(1,KC)+WSAVE(KC-1)*X(1,K)
         WORK(KC) = WSAVE(K-1)*X(1,K)-WSAVE(KC-1)*X(1,KC)
  102 CONTINUE
      IF (MODN .NE. 0) GO TO 305
      X(1,NS2+1) = WSAVE(NS2)*(X(1,NS2+1)+X(1,NS2+1))
  305 DO 103 K=2,NS2
         KC = NP2-K
         X(1,K) = WORK(K)+WORK(KC)
         X(1,KC) = WORK(K)-WORK(KC)
  103 CONTINUE
      X(1,1) = X(1,1)+X(1,1)
  400 RETURN
      END
