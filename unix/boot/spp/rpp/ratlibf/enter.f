      subroutine enter (symbol, info, st)
      integer symbol (100)
      integer info (100)
      integer st
      integer mem( 60000)
      common/cdsmem/mem
      integer i, nodsiz, j
      integer stlu, length
      integer node, pred
      integer dsget
      nodsiz = mem (st)
      if (.not.(stlu (symbol, node, pred, st) .eq. 0))goto 23000
      node = dsget (1 + nodsiz + length (symbol) + 1)
      mem (node + 0) = 0
      mem (pred + 0) = node
      i = 1
      j = node + 1 + nodsiz
23002 if (.not.(symbol (i) .ne. -2))goto 23003
      mem (j) = symbol (i)
      i = i + 1
      j = j + 1
      goto 23002
23003 continue
      mem (j) = -2
23000 continue
      i = 1
23004 if (.not.(i .le. nodsiz))goto 23006
      j = node + 1 + i - 1
      mem (j) = info (i)
23005 i = i + 1
      goto 23004
23006 continue
      return
      end
