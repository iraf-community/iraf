      integer function omatch (lin, i, pat, j)
      integer lin (128), pat (128)
      integer i, j
      integer bump
      integer locate
      omatch = 0
      if (.not.(lin (i) .eq. -2))goto 23000
      return
23000 continue
      bump = -1
      if (.not.(pat (j) .eq. 97))goto 23002
      if (.not.(lin (i) .eq. pat (j + 1)))goto 23004
      bump = 1
23004 continue
      goto 23003
23002 continue
      if (.not.(pat (j) .eq. 37))goto 23006
      if (.not.(i .eq. 1))goto 23008
      bump = 0
23008 continue
      goto 23007
23006 continue
      if (.not.(pat (j) .eq. 63))goto 23010
      if (.not.(lin (i) .ne. 10))goto 23012
      bump = 1
23012 continue
      goto 23011
23010 continue
      if (.not.(pat (j) .eq. 36))goto 23014
      if (.not.(lin (i) .eq. 10))goto 23016
      bump = 0
23016 continue
      goto 23015
23014 continue
      if (.not.(pat (j) .eq. 91))goto 23018
      if (.not.(locate (lin (i), pat, j + 1) .eq. 1))goto 23020
      bump = 1
23020 continue
      goto 23019
23018 continue
      if (.not.(pat (j) .eq. 110))goto 23022
      if (.not.(lin (i) .ne. 10 .and. locate (lin (i), pat, j + 1) .eq. 
     *0))goto 23024
      bump = 1
23024 continue
      goto 23023
23022 continue
      call error (24Hin omatch: can't happen.)
23023 continue
23019 continue
23015 continue
23011 continue
23007 continue
23003 continue
      if (.not.(bump .ge. 0))goto 23026
      i = i + bump
      omatch = 1
23026 continue
      return
      end
