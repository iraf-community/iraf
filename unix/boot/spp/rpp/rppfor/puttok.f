      subroutine puttok (str)
      integer str (100)
      integer i
      i = 1
23000 if (.not.(str (i) .ne. -2))goto 23002
      call putchr (str (i))
23001 i = i + 1
      goto 23000
23002 continue
      return
      end
