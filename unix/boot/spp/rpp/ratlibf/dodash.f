      subroutine dodash (valid, array, i, set, j, maxset)
      integer i, j, maxset
      integer valid (100), array (100), set (maxset)
      integer esc
      integer junk, k, limit
      external index
      integer addset, index
      i = i + 1
      j = j - 1
      limit = index (valid, esc (array, i))
      k = index (valid, set (j))
23000 if (.not.(k .le. limit))goto 23002
      junk = addset (valid (k), set, j, maxset)
23001 k = k + 1
      goto 23000
23002 continue
      return
      end
