      subroutine skipbl(lin, i)
      integer lin(100)
      integer i
23000 if (.not.(lin (i) .eq. 32 .or. lin (i) .eq. 9))goto 23001
      i = i + 1
      goto 23000
23001 continue
      return
      end
