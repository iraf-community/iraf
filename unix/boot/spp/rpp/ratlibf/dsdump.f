      SUBROUTINE DSDUMP (FORM)
      INTEGER FORM
      INTEGER MEM( 1)
      COMMON/CDSMEM/MEM
      INTEGER P, T, Q
      T = 2
      CALL REMARK (27H** DYNAMIC STORAGE DUMP **.)
      CALL PUTINT (1, 5, 2)
      CALL PUTCH (32, 2)
      CALL PUTINT (2 + 1, 0, 2)
      CALL REMARK (14H words in use.)
      P = MEM (T + 1)
23000 IF (.NOT.(P .NE. 0))GOTO 23001
      CALL PUTINT (P, 5, 2)
      CALL PUTCH (32, 2)
      CALL PUTINT (MEM (P + 0), 0, 2)
      CALL REMARK (17H words available.)
      Q = P + MEM (P + 0)
23002 IF (.NOT.(Q .NE. MEM (P + 1) .AND. Q .LT. MEM (1)))GOTO 23003
      CALL DSDBIU (Q, FORM)
      GOTO 23002
23003 CONTINUE
      P = MEM (P + 1)
      GOTO 23000
23001 CONTINUE
      CALL REMARK (15H** END DUMP **.)
      RETURN
      END
