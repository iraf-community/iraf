      subroutine outnum (n)
      integer n
      integer chars (20)
      integer i, m
      m = iabs (n)
      i = 0
23000 continue
      i = i + 1
      chars (i) = mod (m, 10) + 48
      m = m / 10
23001 if (.not.(m .eq. 0 .or. i .ge. 20))goto 23000
23002 continue
      if (.not.(n .lt. 0))goto 23003
      call outch (45)
23003 continue
23005 if (.not.(i .gt. 0))goto 23007
      call outch (chars (i))
23006 i = i - 1
      goto 23005
23007 continue
      return
      end
