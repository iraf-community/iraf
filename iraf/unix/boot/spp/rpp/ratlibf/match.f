      integer function match (lin, pat)
      integer lin (128), pat (128)
      integer i, junk (9)
      integer amatch
      i = 1
23000 if (.not.(lin (i) .ne. -2))goto 23002
      if (.not.(amatch (lin, i, pat, junk, junk) .gt. 0))goto 23003
      match = 1
      return
23003 continue
23001 i = i + 1
      goto 23000
23002 continue
      match = 0
      return
      end
