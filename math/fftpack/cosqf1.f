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
      SUBROUTINE COSQF1 (N,INC,X,WSAVE,WORK,IER)
      DIMENSION       X(INC,*)      ,WSAVE(*)      ,WORK(*)
      IER = 0
      NS2 = (N+1)/2
      NP2 = N+2
      DO 101 K=2,NS2
         KC = NP2-K
         WORK(K)  = X(1,K)+X(1,KC)
         WORK(KC) = X(1,K)-X(1,KC)
  101 CONTINUE
      MODN = MOD(N,2)
      IF (MODN .NE. 0) GO TO 301
      WORK(NS2+1) = X(1,NS2+1)+X(1,NS2+1)
  301 DO 102 K=2,NS2
         KC = NP2-K
         X(1,K)  = WSAVE(K-1)*WORK(KC)+WSAVE(KC-1)*WORK(K)
         X(1,KC) = WSAVE(K-1)*WORK(K) -WSAVE(KC-1)*WORK(KC)
  102 CONTINUE
      IF (MODN .NE. 0) GO TO 303
      X(1,NS2+1) = WSAVE(NS2)*WORK(NS2+1)
  303 LENX = INC*(N-1)  + 1
      LNSV = N + INT(LOG(REAL(N))/LOG(2.)) + 4
      LNWK = N
C
      CALL RFFT1F(N,INC,X,LENX,WSAVE(N+1),LNSV,WORK,LNWK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COSQF1',-5)
        GO TO 400
      ENDIF
C
      DO 103 I=3,N,2
         XIM1 = .5*(X(1,I-1)+X(1,I))
         X(1,I) = .5*(X(1,I-1)-X(1,I))
         X(1,I-1) = XIM1
  103 CONTINUE
  400 RETURN
      END
