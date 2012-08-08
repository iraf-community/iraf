      integer function amatch (lin, from, pat, tagbeg, tagend)
      integer lin (128), pat (128)
      integer from, tagbeg (10), tagend (10)
      integer i, j, offset, stack
      integer omatch, patsiz
      i = 1
23000 if (.not.(i .le. 10))goto 23002
      tagbeg (i) = 0
      tagend (i) = 0
23001 i = i + 1
      goto 23000
23002 continue
      tagbeg (1) = from
      stack = 0
      offset = from
      j = 1
23003 if (.not.(pat (j) .ne. -2))goto 23005
      if (.not.(pat (j) .eq. 42))goto 23006
      stack = j
      j = j + 4
      i = offset
23008 if (.not.(lin (i) .ne. -2))goto 23010
      if (.not.(omatch (lin, i, pat, j) .eq. 0))goto 23011
      goto 23010
23011 continue
23009 goto 23008
23010 continue
      pat (stack + 1) = i - offset
      pat (stack + 3) = offset
      offset = i
      goto 23007
23006 continue
      if (.not.(pat (j) .eq. 123))goto 23013
      i = pat (j + 1)
      tagbeg (i + 1) = offset
      goto 23014
23013 continue
      if (.not.(pat (j) .eq. 125))goto 23015
      i = pat (j + 1)
      tagend (i + 1) = offset
      goto 23016
23015 continue
      if (.not.(omatch (lin, offset, pat, j) .eq. 0))goto 23017
23019 if (.not.(stack .gt. 0))goto 23021
      if (.not.(pat (stack + 1) .gt. 0))goto 23022
      goto 23021
23022 continue
23020 stack = pat (stack + 2)
      goto 23019
23021 continue
      if (.not.(stack .le. 0))goto 23024
      amatch = 0
      return
23024 continue
      pat (stack + 1) = pat (stack + 1) - 1
      j = stack + 4
      offset = pat (stack + 3) + pat (stack + 1)
23017 continue
23016 continue
23014 continue
23007 continue
23004 j = j + patsiz (pat, j)
      goto 23003
23005 continue
      amatch = offset
      tagend (1) = offset
      return
      end
