      subroutine scopy (from, i, to, j)
      integer from (100), to (100)
      integer i, j
      integer k1, k2
      k2 = j
      k1 = i
23000 if (.not.(from (k1) .ne. -2))goto 23002
      to (k2) = from (k1)
      k2 = k2 + 1
23001 k1 = k1 + 1
      goto 23000
23002 continue
      to (k2) = -2
      return
      end
