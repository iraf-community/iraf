      subroutine addchr (c, buf, bp, maxsiz)
      integer bp, maxsiz
      integer c, buf (100)
      if (.not.(bp .gt. maxsiz))goto 23000
      call baderr (16Hbuffer overflow.)
23000 continue
      buf (bp) = c
      bp = bp + 1
      return
      end
