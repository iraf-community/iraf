      INTEGER FUNCTION ITOC (INT, STR, SIZE)
      INTEGER INT, SIZE
      INTEGER STR (100)
      INTEGER MOD
      INTEGER D, I, INTVAL, J, K
      INTEGER DIGITS (11)
      DATA DIGITS (1) /48/, DIGITS (2) /49/, DIGITS (3) /50/, DIGITS (4)
     * /51/, DIGITS (5) /52/, DIGITS (6) /53/, DIGITS (7) /54/, DIGITS (
     *8) /55/, DIGITS (9) /56/, DIGITS (10) /57/, DIGITS (11) /-2/
      INTVAL = IABS (INT)
      STR (1) = -2
      I = 1
23000 CONTINUE
      I = I + 1
      D = MOD (INTVAL, 10)
      STR (I) = DIGITS (D+1)
      INTVAL = INTVAL / 10
23001 IF (.NOT.(INTVAL .EQ. 0 .OR. I .GE. SIZE))GOTO 23000
23002 CONTINUE
      IF (.NOT.(INT .LT. 0 .AND. I .LT. SIZE))GOTO 23003
      I = I + 1
      STR (I) = 45
23003 CONTINUE
      ITOC = I - 1
      J = 1
23005 IF (.NOT.(J .LT. I))GOTO 23007
      K = STR (I)
      STR (I) = STR (J)
      STR (J) = K
      I = I - 1
23006 J = J + 1
      GOTO 23005
23007 CONTINUE
      RETURN
      END
