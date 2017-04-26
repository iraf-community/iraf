      subroutine relate (token, last)
      integer token (100)
      integer last
      integer ngetch
      integer length
      if (.not.(ngetch (token (2)) .ne. 61))goto 23000
      call putbak (token (2))
      token (3) = 116
      goto 23001
23000 continue
      token (3) = 101
23001 continue
      token (4) = 46
      token (5) = -2
      token (6) = -2
      if (.not.(token (1) .eq. 62))goto 23002
      token (2) = 103
      goto 23003
23002 continue
      if (.not.(token (1) .eq. 60))goto 23004
      token (2) = 108
      goto 23005
23004 continue
      if (.not.(token (1) .eq. 126 .or. token (1) .eq. 33 .or. token (1)
     * .eq. 94 .or. token (1) .eq. 126))goto 23006
      if (.not.(token (2) .ne. 61))goto 23008
      token (3) = 111
      token (4) = 116
      token (5) = 46
23008 continue
      token (2) = 110
      goto 23007
23006 continue
      if (.not.(token (1) .eq. 61))goto 23010
      if (.not.(token (2) .ne. 61))goto 23012
      token (2) = -2
      last = 1
      return
23012 continue
      token (2) = 101
      token (3) = 113
      goto 23011
23010 continue
      if (.not.(token (1) .eq. 38))goto 23014
      token (2) = 97
      token (3) = 110
      token (4) = 100
      token (5) = 46
      goto 23015
23014 continue
      if (.not.(token (1) .eq. 124))goto 23016
      token (2) = 111
      token (3) = 114
      goto 23017
23016 continue
      token (2) = -2
23017 continue
23015 continue
23011 continue
23007 continue
23005 continue
23003 continue
      token (1) = 46
      last = length (token)
      return
      end
