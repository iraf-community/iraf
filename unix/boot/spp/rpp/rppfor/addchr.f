      SUBROUTINE ADDCHR (C, BUF, BP, MAXSIZ)
      INTEGER BP, MAXSIZ
      INTEGER C, BUF (100)
      IF (.NOT.(BP .GT. MAXSIZ))GOTO 23000
      CALL BADERR (16Hbuffer overflow.)
23000 CONTINUE
      BUF (BP) = C
      BP = BP + 1
      RETURN
      END
