      INTEGER FUNCTION MNTOC (BUF, P, DEFALT)
      INTEGER BUF (100), DEFALT
      INTEGER P
      INTEGER I, TP
      INTEGER EQUAL
      INTEGER C, TMP (128)
      INTEGER TEXT (170)
      DATA TEXT /  6, 97, 99, 107, -2, 7, 98, 101, 108, -2, 8, 98, 115, 
     *-2, -2, 24, 99, 97, 110, -2, 13, 99, 114, -2, -2, 17, 100, 99, 49,
     * -2, 18, 100, 99, 50, -2, 19, 100, 99, 51, -2, 20, 100, 99, 52, -2
     *, 127, 100, 101, 108, -2, 16, 100, 108, 101, -2, 25, 101, 109, -2,
     * -2, 5, 101, 110, 113, -2, 4, 101, 111, 116, -2, 27, 101, 115, 99,
     * -2, 23, 101, 116, 98, -2, 3, 101, 116, 120, -2, 12, 102, 102, -2,
     * -2, 28, 102, 115, -2, -2, 29, 103, 115, -2, -2, 9, 104, 116, -2, 
     *-2, 10, 108, 102, -2, -2, 21, 110, 97, 107, -2, 0, 110, 117, 108, 
     *-2, 30, 114, 115, -2, -2, 15, 115, 105, -2, -2, 14, 115, 111, -2, 
     *-2, 1, 115, 111, 104, -2, 32, 115, 112, -2, -2, 2, 115, 116, 120, 
     *-2, 26, 115, 117, 98, -2, 22, 115, 121, 110, -2, 31, 117, 115, -2,
     * -2, 11, 118, 116, -2, -2/
      TP = 1
23000 CONTINUE
      TMP (TP) = BUF (P)
      TP = TP + 1
      P = P + 1
23001 IF (.NOT.(.NOT. (((65.LE.BUF (P).AND.BUF (P).LE.90).OR.(97.LE.BUF 
     *(P).AND.BUF (P).LE.122)) .OR. (48.LE.BUF (P).AND.BUF (P).LE.57)) .
     *OR. TP .GE. 128))GOTO 23000
23002 CONTINUE
      TMP (TP) = -2
      IF (.NOT.(TP .EQ. 2))GOTO 23003
      C = TMP (1)
      GOTO 23004
23003 CONTINUE
      CALL LOWER (TMP)
      I = 1
23005 IF (.NOT.(I .LT. 170))GOTO 23007
      IF (.NOT.(EQUAL (TMP, TEXT (I + 1)) .EQ. 1))GOTO 23008
      GOTO 23007
23008 CONTINUE
23006 I = I + 5
      GOTO 23005
23007 CONTINUE
      IF (.NOT.(I .LT. 170))GOTO 23010
      C = TEXT (I)
      GOTO 23011
23010 CONTINUE
      C = DEFALT
23011 CONTINUE
23004 CONTINUE
      MNTOC=(C)
      RETURN
      END
