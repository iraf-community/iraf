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
      SUBROUTINE C1F3KB (IDO,L1,NA,CC,IN1,CH,IN2,WA)
      REAL  CC(IN1,L1,IDO,3),CH(IN2,L1,3,IDO),WA(IDO,2,2)
      DATA TAUR,TAUI /-.5,.866025403784439/
C
      IF (IDO.GT.1 .OR. NA.EQ.1) GO TO 102
      DO 101 K=1,L1
         TR2 = CC(1,K,1,2)+CC(1,K,1,3)
         CR2 = CC(1,K,1,1)+TAUR*TR2
         CC(1,K,1,1) = CC(1,K,1,1)+TR2
         TI2 = CC(2,K,1,2)+CC(2,K,1,3)
         CI2 = CC(2,K,1,1)+TAUR*TI2
         CC(2,K,1,1) = CC(2,K,1,1)+TI2
         CR3 = TAUI*(CC(1,K,1,2)-CC(1,K,1,3))
         CI3 = TAUI*(CC(2,K,1,2)-CC(2,K,1,3))
         CC(1,K,1,2) = CR2-CI3
         CC(1,K,1,3) = CR2+CI3
         CC(2,K,1,2) = CI2+CR3
         CC(2,K,1,3) = CI2-CR3
  101 CONTINUE
      RETURN
  102 DO 103 K=1,L1
         TR2 = CC(1,K,1,2)+CC(1,K,1,3)
         CR2 = CC(1,K,1,1)+TAUR*TR2
         CH(1,K,1,1) = CC(1,K,1,1)+TR2
         TI2 = CC(2,K,1,2)+CC(2,K,1,3)
         CI2 = CC(2,K,1,1)+TAUR*TI2
         CH(2,K,1,1) = CC(2,K,1,1)+TI2
         CR3 = TAUI*(CC(1,K,1,2)-CC(1,K,1,3))
         CI3 = TAUI*(CC(2,K,1,2)-CC(2,K,1,3))
         CH(1,K,2,1) = CR2-CI3
         CH(1,K,3,1) = CR2+CI3
         CH(2,K,2,1) = CI2+CR3
         CH(2,K,3,1) = CI2-CR3
  103 CONTINUE
      IF (IDO .EQ. 1) RETURN
      DO 105 I=2,IDO
        DO 104 K=1,L1
            TR2 = CC(1,K,I,2)+CC(1,K,I,3)
            CR2 = CC(1,K,I,1)+TAUR*TR2
            CH(1,K,1,I) = CC(1,K,I,1)+TR2
            TI2 = CC(2,K,I,2)+CC(2,K,I,3)
            CI2 = CC(2,K,I,1)+TAUR*TI2
            CH(2,K,1,I) = CC(2,K,I,1)+TI2
            CR3 = TAUI*(CC(1,K,I,2)-CC(1,K,I,3))
            CI3 = TAUI*(CC(2,K,I,2)-CC(2,K,I,3))
            DR2 = CR2-CI3
            DR3 = CR2+CI3
            DI2 = CI2+CR3
            DI3 = CI2-CR3
            CH(2,K,2,I) = WA(I,1,1)*DI2+WA(I,1,2)*DR2
            CH(1,K,2,I) = WA(I,1,1)*DR2-WA(I,1,2)*DI2
            CH(2,K,3,I) = WA(I,2,1)*DI3+WA(I,2,2)*DR3
            CH(1,K,3,I) = WA(I,2,1)*DR3-WA(I,2,2)*DI3
  104    CONTINUE
  105 CONTINUE
      RETURN
      END
