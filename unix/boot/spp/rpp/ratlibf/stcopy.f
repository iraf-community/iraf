      SUBROUTINE STCOPY (IN, I, OUT, J)
      INTEGER IN (100), OUT (100)
      INTEGER I, J
      INTEGER K
      K = I
23000 IF (.NOT.(IN (K) .NE. -2))GOTO 23002
      OUT (J) = IN (K)
      J = J + 1
23001 K = K + 1
      GOTO 23000
23002 CONTINUE
      OUT(J) = -2
      RETURN
      END
