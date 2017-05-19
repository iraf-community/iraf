      integer function stlu (symbol, node, pred, st)
      integer symbol (100)
      integer node, pred, st
      integer mem( 60000)
      common/cdsmem/mem
      integer hash, i, j, nodsiz
      nodsiz = mem (st)
      hash = 0
      i = 1
23000 if (.not.(symbol (i) .ne. -2))goto 23002
      hash = hash + symbol (i)
23001 i = i + 1
      goto 23000
23002 continue
      hash = mod (hash, 43) + 1
      pred = st + hash
      node = mem (pred)
23003 if (.not.(node .ne. 0))goto 23004
      i = 1
      j = node + 1 + nodsiz
23005 if (.not.(symbol (i) .eq. mem (j)))goto 23006
      if (.not.(symbol (i) .eq. -2))goto 23007
      stlu=(1)
      return
23007 continue
      i = i + 1
      j = j + 1
      goto 23005
23006 continue
      pred = node
      node = mem (pred + 0)
      goto 23003
23004 continue
      stlu=(0)
      return
      end
