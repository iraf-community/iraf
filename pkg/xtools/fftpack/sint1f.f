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
      SUBROUTINE SINT1F ( N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
C
      IER = 0
      IF (LENX .LT. INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('SINT1F', 6)
        GO TO 100
      ELSEIF (LENSAV .LT. N/2 + N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('SINT1F', 8)
        GO TO 100
      ELSEIF (LENWRK .LT. (2*N+2)) THEN
        IER = 3
        CALL XERFFT ('SINT1F', 10)
        GO TO 100
      ENDIF
C
      CALL SINTF1(N,INC,X,WSAVE,WORK,WORK(N+2),IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('SINT1F',-5)
      ENDIF
  100 CONTINUE
      RETURN
      END
