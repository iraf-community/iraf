      subroutine stcopy (in, i, out, j)
      integer in (100), out (100)
      integer i, j
      integer k
      k = i
23000 if (.not.(in (k) .ne. -2))goto 23002
      out (j) = in (k)
      j = j + 1
23001 k = k + 1
      goto 23000
23002 continue
      out(j) = -2
      return
      end
