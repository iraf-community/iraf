      SUBROUTINE DSFREE (BLOCK)
      INTEGER BLOCK
      INTEGER MEM( 1)
      COMMON/CDSMEM/MEM
      INTEGER P0, P, Q
      INTEGER N, JUNK
      INTEGER CON (10)
      P0 = BLOCK - 2
      N = MEM (P0 + 0)
      Q = 2
23000 CONTINUE
      P = MEM (Q + 1)
      IF (.NOT.(P .EQ. 0 .OR. P .GT. P0))GOTO 23003
      GOTO 23002
23003 CONTINUE
      Q = P
23001 GOTO 23000
23002 CONTINUE
      IF (.NOT.(Q + MEM (Q + 0) .GT. P0))GOTO 23005
      CALL REMARK (45Hin dsfree: attempt to free unallocated block.)
      CALL REMARK (21Htype 'c' to continue.)
      JUNK = GETLIN (CON, 0)
      IF (.NOT.(CON (1) .NE. 99 .AND. CON (1) .NE. 67))GOTO 23007
      CALL ENDST
23007 CONTINUE
      RETURN
23005 CONTINUE
      IF (.NOT.(P0 + N .EQ. P .AND. P .NE. 0))GOTO 23009
      N = N + MEM (P + 0)
      MEM (P0 + 1) = MEM (P + 1)
      GOTO 23010
23009 CONTINUE
      MEM (P0 + 1) = P
23010 CONTINUE
      IF (.NOT.(Q + MEM (Q + 0) .EQ. P0))GOTO 23011
      MEM (Q + 0) = MEM (Q + 0) + N
      MEM (Q + 1) = MEM (P0 + 1)
      GOTO 23012
23011 CONTINUE
      MEM (Q + 1) = P0
      MEM (P0 + 0) = N
23012 CONTINUE
      RETURN
      END
