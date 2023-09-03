      integer function sctabl (table, sym, info, posn)
      integer table, posn
      integer sym (100)
      integer info (100)
      integer mem( 60000)
      common/cdsmem/mem
      integer bucket, walker
      integer dsget
      integer nodsiz, i, j
      if (.not.(posn .eq. 0))goto 23000
      posn = dsget (2)
      mem (posn) = 1
      mem (posn + 1) = mem (table + 1)
23000 continue
      bucket = mem (posn)
      walker = mem (posn + 1)
      nodsiz = mem (table)
23002 continue
      if (.not.(walker .ne. 0))goto 23005
      i = walker + 1 + nodsiz
      j = 1
23007 if (.not.(mem (i) .ne. -2))goto 23008
      sym (j) = mem (i)
      i = i + 1
      j = j + 1
      goto 23007
23008 continue
      sym (j) = -2
      i = 1
23009 if (.not.(i .le. nodsiz))goto 23011
      j = walker + 1 + i - 1
      info (i) = mem (j)
23010 i = i + 1
      goto 23009
23011 continue
      mem (posn) = bucket
      mem (posn + 1) = mem (walker + 0)
      sctabl = 1
      return
23005 continue
      bucket = bucket + 1
      if (.not.(bucket .gt. 43))goto 23012
      goto 23004
23012 continue
      j = table + bucket
      walker = mem (j)
23006 continue
23003 goto 23002
23004 continue
      call dsfree (posn)
      posn = 0
      sctabl = -1
      return
      end
