      integer function makpat (arg, from, delim, pat)
      integer arg (128), delim, pat (128)
      integer from
      integer esc
      integer i, j, junk, lastcl, lastj, lj, tagnst, tagnum, tagstk (9)
      integer addset, getccl, stclos
      j = 1
      lastj = 1
      lastcl = 0
      tagnum = 0
      tagnst = 0
      i = from
23000 if (.not.(arg (i) .ne. delim .and. arg (i) .ne. -2))goto 23002
      lj = j
      if (.not.(arg (i) .eq. 63))goto 23003
      junk = addset (63, pat, j, 128)
      goto 23004
23003 continue
      if (.not.(arg (i) .eq. 37 .and. i .eq. from))goto 23005
      junk = addset (37, pat, j, 128)
      goto 23006
23005 continue
      if (.not.(arg (i) .eq. 36 .and. arg (i + 1) .eq. delim))goto 23007
      junk = addset (36, pat, j, 128)
      goto 23008
23007 continue
      if (.not.(arg (i) .eq. 91))goto 23009
      if (.not.(getccl (arg, i, pat, j) .eq. -3))goto 23011
      makpat = -3
      return
23011 continue
      goto 23010
23009 continue
      if (.not.(arg (i) .eq. 42 .and. i .gt. from))goto 23013
      lj = lastj
      if (.not.(pat (lj) .eq. 37 .or. pat (lj) .eq. 36 .or. pat (lj) .eq
     *. 42 .or. pat (lj) .eq. 123 .or. pat (lj) .eq. 125))goto 23015
      goto 23002
23015 continue
      lastcl = stclos (pat, j, lastj, lastcl)
      goto 23014
23013 continue
      if (.not.(arg (i) .eq. 123))goto 23017
      if (.not.(tagnum .ge. 9))goto 23019
      goto 23002
23019 continue
      tagnum = tagnum + 1
      tagnst = tagnst + 1
      tagstk (tagnst) = tagnum
      junk = addset (123, pat, j, 128)
      junk = addset (tagnum, pat, j, 128)
      goto 23018
23017 continue
      if (.not.(arg (i) .eq. 125 .and. tagnst .gt. 0))goto 23021
      junk = addset (125, pat, j, 128)
      junk = addset (tagstk (tagnst), pat, j, 128)
      tagnst = tagnst - 1
      goto 23022
23021 continue
      junk = addset (97, pat, j, 128)
      junk = addset (esc (arg, i), pat, j, 128)
23022 continue
23018 continue
23014 continue
23010 continue
23008 continue
23006 continue
23004 continue
      lastj = lj
23001 i = i + 1
      goto 23000
23002 continue
      if (.not.(arg (i) .ne. delim))goto 23023
      makpat = -3
      goto 23024
23023 continue
      if (.not.(addset (-2, pat, j, 128) .eq. 0))goto 23025
      makpat = -3
      goto 23026
23025 continue
      if (.not.(tagnst .ne. 0))goto 23027
      makpat = -3
      goto 23028
23027 continue
      makpat = i
23028 continue
23026 continue
23024 continue
      return
      end
