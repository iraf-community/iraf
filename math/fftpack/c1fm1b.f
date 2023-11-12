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
      SUBROUTINE C1FM1B (N,INC,C,CH,WA,FNF,FAC)
      COMPLEX       C(*)
      REAL       CH(*),     WA(*),     FAC(*)
C
C FFTPACK 5.1 auxiliary routine
C
      INC2 = INC+INC
      NF = FNF
      NA = 0
      L1 = 1
      IW = 1
      DO 125 K1=1,NF
         IP = FAC(K1)
         L2 = IP*L1
         IDO = N/L2
         LID = L1*IDO
         NBR = 1+NA+2*MIN(IP-2,4)
         GO TO (52,62,53,63,54,64,55,65,56,66),NBR
   52    CALL C1F2KB (IDO,L1,NA,C,INC2,CH,2,WA(IW))
         GO TO 120
   62    CALL C1F2KB (IDO,L1,NA,CH,2,C,INC2,WA(IW))
         GO TO 120
   53    CALL C1F3KB (IDO,L1,NA,C,INC2,CH,2,WA(IW))
         GO TO 120
   63    CALL C1F3KB (IDO,L1,NA,CH,2,C,INC2,WA(IW))
         GO TO 120
   54    CALL C1F4KB (IDO,L1,NA,C,INC2,CH,2,WA(IW))
         GO TO 120
   64    CALL C1F4KB (IDO,L1,NA,CH,2,C,INC2,WA(IW))
         GO TO 120
   55    CALL C1F5KB (IDO,L1,NA,C,INC2,CH,2,WA(IW))
         GO TO 120
   65    CALL C1F5KB (IDO,L1,NA,CH,2,C,INC2,WA(IW))
         GO TO 120
   56    CALL C1FGKB (IDO,IP,L1,LID,NA,C,C,INC2,CH,CH,2,
     1     WA(IW))
         GO TO 120
   66    CALL C1FGKB (IDO,IP,L1,LID,NA,CH,CH,2,C,C,
     1     INC2,WA(IW))
  120    L1 = L2
         IW = IW+(IP-1)*(IDO+IDO)
         IF(IP .LE. 5) NA = 1-NA
  125 CONTINUE
      RETURN
      END
