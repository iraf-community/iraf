      integer function lookup (symbol, info, st)
      integer symbol (100)
      integer info (100)
      integer st
      integer mem( 60000)
      common/cdsmem/mem
      integer i, nodsiz, kluge
      integer stlu
      integer node, pred
      if (.not.(stlu (symbol, node, pred, st) .eq. 0))goto 23000
      lookup = 0
      return
23000 continue
      nodsiz = mem (st)
      i = 1
23002 if (.not.(i .le. nodsiz))goto 23004
      kluge = node + 1 - 1 + i
      info (i) = mem (kluge)
23003 i = i + 1
      goto 23002
23004 continue
      lookup = 1
      return
      end
