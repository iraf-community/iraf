      integer function addstr (s, str, j, maxsiz)
      integer j, maxsiz
      integer s (100), str (maxsiz)
      integer i, addset
      i = 1
23000 if (.not.(s (i) .ne. -2))goto 23002
      if (.not.(addset (s (i), str, j, maxsiz) .eq. 0))goto 23003
      addstr = 0
      return
23003 continue
23001 i = i + 1
      goto 23000
23002 continue
      addstr = 1
      return
      end
