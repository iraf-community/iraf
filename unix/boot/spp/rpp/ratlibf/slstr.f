      integer function slstr (from, to, first, chars)
      integer from (100), to (100)
      integer first, chars
      integer len, i, j, k
      integer length
      len = length (from)
      i = first
      if (.not.(i .lt. 1))goto 23000
      i = i + len + 1
23000 continue
      if (.not.(chars .lt. 0))goto 23002
      i = i + chars + 1
      chars = - chars
23002 continue
      j = i + chars - 1
      if (.not.(i .lt. 1))goto 23004
      i = 1
23004 continue
      if (.not.(j .gt. len))goto 23006
      j = len
23006 continue
      k = 0
23008 if (.not.(i .le. j))goto 23010
      to (k + 1) = from (i)
      i = i + 1
23009 k = k + 1
      goto 23008
23010 continue
      to (k + 1) = -2
      slstr=(k)
      return
      end
