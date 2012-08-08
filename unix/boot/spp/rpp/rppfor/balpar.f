      subroutine balpar
      integer t, token (100)
      integer gettok, gnbtok
      integer nlpar
      if (.not.(gnbtok (token, 100) .ne. 40))goto 23000
      call synerr (19Hmissing left paren.)
      return
23000 continue
      call outstr (token)
      nlpar = 1
23002 continue
      t = gettok (token, 100)
      if (.not.(t .eq. 59 .or. t .eq. 123 .or. t .eq. 125 .or. t .eq. -1
     *))goto 23005
      call pbstr (token)
      goto 23004
23005 continue
      if (.not.(t .eq. 10))goto 23007
      token (1) = -2
      goto 23008
23007 continue
      if (.not.(t .eq. 40))goto 23009
      nlpar = nlpar + 1
      goto 23010
23009 continue
      if (.not.(t .eq. 41))goto 23011
      nlpar = nlpar - 1
23011 continue
23010 continue
23008 continue
      if (.not.(t .eq. -9))goto 23013
      call squash (token)
23013 continue
      call outstr (token)
23003 if (.not.(nlpar .le. 0))goto 23002
23004 continue
      if (.not.(nlpar .ne. 0))goto 23015
      call synerr (33Hmissing parenthesis in condition.)
23015 continue
      return
      end
