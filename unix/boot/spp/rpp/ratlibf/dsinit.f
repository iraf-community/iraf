      SUBROUTINE DSINIT (W)
      INTEGER W
      INTEGER MEM( 1)
      COMMON/CDSMEM/MEM
      INTEGER T
      IF (.NOT.(W .LT. 2 * 2 + 2))GOTO 23000
      CALL ERROR (42Hin dsinit: unreasonably small memory size.)
23000 CONTINUE
      T = 2
      MEM (T + 0) = 0
      MEM (T + 1) = 2 + 2
      T = 2 + 2
      MEM (T + 0) = W - 2 - 1
      MEM (T + 1) = 0
      MEM (1) = W
      RETURN
      END
