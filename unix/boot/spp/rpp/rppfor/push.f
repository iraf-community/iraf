      INTEGER FUNCTION PUSH (EP, ARGSTK, AP)
      INTEGER AP, ARGSTK (100), EP
      IF (.NOT.(AP .GT. 100))GOTO 23000
      CALL BADERR (19Harg stack overflow.)
23000 CONTINUE
      ARGSTK (AP) = EP
      PUSH = AP + 1
      RETURN
      END
