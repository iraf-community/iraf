      integer function itoc (int, str, size)
      integer int, size
      integer str (100)
      integer mod
      integer d, i, intval, j, k
      integer digits (11)
      data digits (1) /48/, digits (2) /49/, digits (3) /50/, digits (4)
     * /51/, digits (5) /52/, digits (6) /53/, digits (7) /54/, digits (
     *8) /55/, digits (9) /56/, digits (10) /57/, digits (11) /-2/
      intval = iabs (int)
      str (1) = -2
      i = 1
23000 continue
      i = i + 1
      d = mod (intval, 10)
      str (i) = digits (d+1)
      intval = intval / 10
23001 if (.not.(intval .eq. 0 .or. i .ge. size))goto 23000
23002 continue
      if (.not.(int .lt. 0 .and. i .lt. size))goto 23003
      i = i + 1
      str (i) = 45
23003 continue
      itoc = i - 1
      j = 1
23005 if (.not.(j .lt. i))goto 23007
      k = str (i)
      str (i) = str (j)
      str (j) = k
      i = i - 1
23006 j = j + 1
      goto 23005
23007 continue
      return
      end
