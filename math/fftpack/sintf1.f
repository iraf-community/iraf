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
      SUBROUTINE SINTF1(N,INC,X,WSAVE,XH,WORK,IER)
      REAL       X(INC,*)       ,WSAVE(*)   ,XH(*)
      DOUBLE PRECISION           DSUM
      IER = 0
      IF (N-2) 200,102,103
  102 SSQRT3 = 1./SQRT(3.)
      XHOLD = SSQRT3*(X(1,1)+X(1,2))
      X(1,2) = SSQRT3*(X(1,1)-X(1,2))
      X(1,1) = XHOLD
      GO TO 200
  103 NP1 = N+1
      NS2 = N/2
      DO 104 K=1,NS2
         KC = NP1-K
         T1 = X(1,K)-X(1,KC)
         T2 = WSAVE(K)*(X(1,K)+X(1,KC))
         XH(K+1) = T1+T2
         XH(KC+1) = T2-T1
  104 CONTINUE
      MODN = MOD(N,2)
      IF (MODN .EQ. 0) GO TO 124
      XH(NS2+2) = 4.*X(1,NS2+1)
  124 XH(1) = 0.
      LNXH = NP1
      LNSV = NP1 + INT(LOG(REAL(NP1))/LOG(2.)) + 4
      LNWK = NP1
C
      CALL RFFT1F(NP1,1,XH,LNXH,WSAVE(NS2+1),LNSV,WORK,
     1            LNWK,IER1)     
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('SINTF1',-5)
        GO TO 200
      ENDIF
C
      IF(MOD(NP1,2) .NE. 0) GO TO 30
      XH(NP1) = XH(NP1)+XH(NP1)
   30 SFNP1 = 1./FLOAT(NP1)
         X(1,1) = .5*XH(1)
         DSUM = X(1,1)
      DO 105 I=3,N,2
            X(1,I-1) = .5*XH(I)
            DSUM = DSUM+.5*XH(I-1)
            X(1,I) = DSUM
  105 CONTINUE
      IF (MODN .NE. 0) GO TO 200
      X(1,N) = .5*XH(N+1)
  200 RETURN
      END
