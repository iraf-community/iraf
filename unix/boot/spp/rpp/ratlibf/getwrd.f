      INTEGER FUNCTION GETWRD (IN, I, OUT)
      INTEGER IN (100), OUT (100)
      INTEGER I
      INTEGER J
23000 IF (.NOT.(IN (I) .EQ. 32 .OR. IN (I) .EQ. 9))GOTO 23001
      I = I + 1
      GOTO 23000
23001 CONTINUE
      J = 1
23002 IF (.NOT.(IN (I) .NE. -2 .AND. IN (I) .NE. 32 .AND. IN (I) .NE. 9 
     *.AND. IN (I) .NE. 10))GOTO 23003
      OUT (J) = IN (I)
      I = I + 1
      J = J + 1
      GOTO 23002
23003 CONTINUE
      OUT (J) = -2
      GETWRD = J - 1
      RETURN
      END
