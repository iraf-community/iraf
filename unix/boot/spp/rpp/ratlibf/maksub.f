      integer function maksub (arg, from, delim, sub)
      integer arg (128), delim, sub (128)
      integer from
      integer esc, type
      integer i, j, junk
      integer addset
      j = 1
      i = from
23000 if (.not.(arg (i) .ne. delim .and. arg (i) .ne. -2))goto 23002
      if (.not.(arg (i) .eq. 38))goto 23003
      junk = addset (-3, sub, j, 128)
      junk = addset (0, sub, j, 128)
      goto 23004
23003 continue
      if (.not.(arg (i) .eq. 64 .and. type (arg (i + 1)) .eq. 48))goto 2
     *3005
      i = i + 1
      junk = addset (-3, sub, j, 128)
      junk = addset (arg (i) - 48, sub, j, 128)
      goto 23006
23005 continue
      junk = addset (esc (arg, i), sub, j, 128)
23006 continue
23004 continue
23001 i = i + 1
      goto 23000
23002 continue
      if (.not.(arg (i) .ne. delim))goto 23007
      maksub = -3
      goto 23008
23007 continue
      if (.not.(addset (-2, sub, j, 128) .eq. 0))goto 23009
      maksub = -3
      goto 23010
23009 continue
      maksub = i
23010 continue
23008 continue
      return
      end
