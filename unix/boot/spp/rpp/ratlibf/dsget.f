      INTEGER FUNCTION DSGET (W)
      INTEGER W
      INTEGER MEM( 1)
      COMMON/CDSMEM/MEM
      INTEGER P, Q, L
      INTEGER N, K, JUNK
      INTEGER GETLIN
      INTEGER C (10)
      N = W + 2
      Q = 2
23000 CONTINUE
      P = MEM (Q + 1)
      IF (.NOT.(P .EQ. 0))GOTO 23003
      CALL REMARK (31Hin dsget: out of storage space.)
      CALL REMARK (41Htype 'c' or 'i' for char or integer dump.)
      JUNK = GETLIN (C, 0)
      IF (.NOT.(C (1) .EQ. 99 .OR. C (1) .EQ. 67))GOTO 23005
      CALL DSDUMP (97)
      GOTO 23006
23005 CONTINUE
      IF (.NOT.(C (1) .EQ. 105 .OR. C (1) .EQ. 73))GOTO 23007
      CALL DSDUMP (48)
23007 CONTINUE
23006 CONTINUE
      CALL ERROR (19Hprogram terminated.)
23003 CONTINUE
      IF (.NOT.(MEM (P + 0) .GE. N))GOTO 23009
      GOTO 23002
23009 CONTINUE
      Q = P
23001 GOTO 23000
23002 CONTINUE
      K = MEM (P + 0) - N
      IF (.NOT.(K .GE. 8))GOTO 23011
      MEM (P + 0) = K
      L = P + K
      MEM (L + 0) = N
      GOTO 23012
23011 CONTINUE
      MEM (Q + 1) = MEM (P + 1)
      L = P
23012 CONTINUE
      DSGET=(L + 2)
      RETURN
      END
