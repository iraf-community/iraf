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
      SUBROUTINE CMF2KB (LOT,IDO,L1,NA,CC,IM1,IN1,CH,IM2,IN2,WA)
      REAL  CC(2,IN1,L1,IDO,2),CH(2,IN2,L1,2,IDO),WA(IDO,1,2)
C
      M1D = (LOT-1)*IM1+1
      M2S = 1-IM2
      IF (IDO.GT.1 .OR. NA.EQ.1) GO TO 102
      DO 101 K=1,L1
         DO 101 M1=1,M1D,IM1
         CHOLD1 = CC(1,M1,K,1,1)+CC(1,M1,K,1,2)
         CC(1,M1,K,1,2) = CC(1,M1,K,1,1)-CC(1,M1,K,1,2)
         CC(1,M1,K,1,1) = CHOLD1
         CHOLD2 = CC(2,M1,K,1,1)+CC(2,M1,K,1,2)
         CC(2,M1,K,1,2) = CC(2,M1,K,1,1)-CC(2,M1,K,1,2)
         CC(2,M1,K,1,1) = CHOLD2
  101 CONTINUE
      RETURN
  102 DO 103 K=1,L1
         M2 = M2S
         DO 103 M1=1,M1D,IM1
         M2 = M2+IM2
         CH(1,M2,K,1,1) = CC(1,M1,K,1,1)+CC(1,M1,K,1,2)
         CH(1,M2,K,2,1) = CC(1,M1,K,1,1)-CC(1,M1,K,1,2)
         CH(2,M2,K,1,1) = CC(2,M1,K,1,1)+CC(2,M1,K,1,2)
         CH(2,M2,K,2,1) = CC(2,M1,K,1,1)-CC(2,M1,K,1,2)
  103 CONTINUE
      IF(IDO .EQ. 1) RETURN
      DO 105 I=2,IDO
         DO 104 K=1,L1
         M2 = M2S
         DO 104 M1=1,M1D,IM1
         M2 = M2+IM2
            CH(1,M2,K,1,I) = CC(1,M1,K,I,1)+CC(1,M1,K,I,2)
            TR2 = CC(1,M1,K,I,1)-CC(1,M1,K,I,2)
            CH(2,M2,K,1,I) = CC(2,M1,K,I,1)+CC(2,M1,K,I,2)
            TI2 = CC(2,M1,K,I,1)-CC(2,M1,K,I,2)
            CH(2,M2,K,2,I) = WA(I,1,1)*TI2+WA(I,1,2)*TR2
            CH(1,M2,K,2,I) = WA(I,1,1)*TR2-WA(I,1,2)*TI2
  104    CONTINUE
  105 CONTINUE
      RETURN
      END
