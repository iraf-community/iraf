      integer function addset (c, str, j, maxsiz)
      integer j, maxsiz
      integer c, str (maxsiz)
      if (.not.(j .gt. maxsiz))goto 23000
      addset = 0
      goto 23001
23000 continue
      str(j) = c
      j = j + 1
      addset = 1
23001 continue
      return
      end
