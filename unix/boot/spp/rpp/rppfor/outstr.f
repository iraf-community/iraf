      subroutine outstr (str)
      integer str (100)
      integer c
      integer i, j
      i = 1
23000 if (.not.(str (i) .ne. -2))goto 23002
      c = str (i)
      if (.not.(c .ne. 39 .and. c .ne. 34))goto 23003
      call outch (c)
      goto 23004
23003 continue
      i = i + 1
      j = i
23005 if (.not.(str (j) .ne. c))goto 23007
23006 j = j + 1
      goto 23005
23007 continue
      call outnum (j - i)
      call outch (72)
23008 if (.not.(i .lt. j))goto 23010
      call outch (str (i))
23009 i = i + 1
      goto 23008
23010 continue
23004 continue
23001 i = i + 1
      goto 23000
23002 continue
      return
      end
