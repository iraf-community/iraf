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
      SUBROUTINE MCSQB1 (LOT,JUMP,N,INC,X,WSAVE,WORK,IER)
      DIMENSION       X(INC,*)     ,WSAVE(*)     ,WORK(LOT,*)
      IER = 0
      LJ = (LOT-1)*JUMP+1
      NS2 = (N+1)/2
      NP2 = N+2
      DO 101 I=3,N,2
         DO 201 M=1,LJ,JUMP
         XIM1 = X(M,I-1)+X(M,I)
         X(M,I) = .5*(X(M,I-1)-X(M,I))
         X(M,I-1) = .5*XIM1
 201     CONTINUE
  101 CONTINUE
      DO 301 M=1,LJ,JUMP
      X(M,1) = .5*X(M,1)
 301  CONTINUE
      MODN = MOD(N,2)
      IF (MODN .NE. 0) GO TO 302
      DO 303 M=1,LJ,JUMP
      X(M,N) = .5*X(M,N)
 303  CONTINUE
 302  CONTINUE
      LENX = (LOT-1)*JUMP + INC*(N-1)  + 1
      LNSV = N + INT(LOG(REAL(N))/LOG(2.)) + 4
      LNWK = LOT*N
C
      CALL RFFTMB(LOT,JUMP,N,INC,X,LENX,WSAVE(N+1),LNSV,WORK,LNWK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('MCSQB1',-5)
        GO TO 400
      ENDIF
C
      DO 102 K=2,NS2
         KC = NP2-K
         M1 = 0
         DO 202 M=1,LJ,JUMP
         M1 = M1 + 1
         WORK(M1,K) = WSAVE(K-1)*X(M,KC)+WSAVE(KC-1)*X(M,K)
         WORK(M1,KC) = WSAVE(K-1)*X(M,K)-WSAVE(KC-1)*X(M,KC)
 202     CONTINUE
  102 CONTINUE
      IF (MODN .NE. 0) GO TO 305
      DO 304 M=1,LJ,JUMP
         X(M,NS2+1) = WSAVE(NS2)*(X(M,NS2+1)+X(M,NS2+1))
 304     CONTINUE
 305  DO 103 K=2,NS2
         KC = NP2-K
         M1 = 0
         DO 203 M=1,LJ,JUMP
            M1 = M1 + 1
            X(M,K) = WORK(M1,K)+WORK(M1,KC)
            X(M,KC) = WORK(M1,K)-WORK(M1,KC)
 203     CONTINUE
  103 CONTINUE
      DO 104 M=1,LJ,JUMP
      X(M,1) = X(M,1)+X(M,1)
 104  CONTINUE
  400 CONTINUE
      RETURN
      END
