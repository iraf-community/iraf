      SUBROUTINE SCOPY (FROM, I, TO, J)
      INTEGER FROM (100), TO (100)
      INTEGER I, J
      INTEGER K1, K2
      K2 = J
      K1 = I
23000 IF (.NOT.(FROM (K1) .NE. -2))GOTO 23002
      TO (K2) = FROM (K1)
      K2 = K2 + 1
23001 K1 = K1 + 1
      GOTO 23000
23002 CONTINUE
      TO (K2) = -2
      RETURN
      END
