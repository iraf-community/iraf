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
      SUBROUTINE TABLES (IDO,IP,WA)
      REAL  WA(IDO,IP-1,2)
C
      TPI = 8.*ATAN(1.)
      ARGZ = TPI/REAL(IP)
      ARG1 = TPI/REAL(IDO*IP)
      DO 110 J=2,IP
         ARG2 = REAL(J-1)*ARG1
         DO 100 I=1,IDO
            ARG3 = REAL(I-1)*ARG2 
            WA(I,J-1,1) = COS(ARG3)
            WA(I,J-1,2) = SIN(ARG3)
  100    CONTINUE
         IF (IP .LE. 5) GO TO 110
         ARG4 = REAL(J-1)*ARGZ
         WA(1,J-1,1) = COS(ARG4)
         WA(1,J-1,2) = SIN(ARG4)
  110 CONTINUE
      RETURN
      END
