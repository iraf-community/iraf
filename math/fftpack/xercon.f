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
      LOGICAL FUNCTION XERCON (INC,JUMP,N,LOT)
      INTEGER INC, JUMP, N, LOT
      INTEGER I, J, JNEW, LCM
C
C     Definition: positive integers INC, JUMP, N and LOT are consistent 
C                                                            ----------
C     if I1*INC + J1*JUMP = I2*INC + J2*JUMP for I1,I2 < N and J1,J2 
C     < LOT implies I1=I2 and J1=J2.
C
C     For multiple FFTs to execute correctly, input parameters INC, 
C     JUMP, N and LOT must be consistent ... otherwise at least one 
C     array element mistakenly is transformed more than once.
C
C     XERCON = .TRUE. if and only if INC, JUMP, N and LOT are 
C     consistent.
C
C     ------------------------------------------------------------------
C
C     Compute I = greatest common divisor (INC, JUMP)
C
      I = INC
      J = JUMP
   10 CONTINUE
      IF (J .NE. 0) THEN
        JNEW = MOD(I,J)
        I    = J
        J    = JNEW
        GO TO 10
      ENDIF
C
C Compute LCM = least common multiple (INC, JUMP)
C
      LCM = (INC*JUMP)/I
C
C Check consistency of INC, JUMP, N, LOT
C
      IF (LCM .LE. (N-1)*INC .AND. LCM .LE. (LOT-1)*JUMP) THEN
        XERCON = .FALSE.
      ELSE
        XERCON = .TRUE.
      ENDIF
C
      RETURN
      END
