      integer function caslab (n, t)
      integer n, t
      integer tok(100)
      integer i, s, lev
      integer gnbtok, ctoi
      caslab=0
      t = gnbtok (tok, 100)
23000 if (.not.(t .eq. 10))goto 23001
      t = gnbtok (tok, 100)
      goto 23000
23001 continue
      if (.not.(t .eq. -1))goto 23002
      caslab=(t)
      return
23002 continue
      lev=0
23004 if (.not.(t .eq. 40))goto 23006
      lev = lev + 1
23005 t = gnbtok (tok, 100)
      goto 23004
23006 continue
      if (.not.(t .eq. 45))goto 23007
      s = -1
      goto 23008
23007 continue
      s = +1
23008 continue
      if (.not.(t .eq. 45 .or. t .eq. 43))goto 23009
      t = gnbtok (tok, 100)
23009 continue
      if (.not.(t .ne. 48))goto 23011
      goto 99
c     goto 23012
23011 continue
      i = 1
      n = s * ctoi (tok, i)
23012 continue
      t=gnbtok(tok,100)
23013 if (.not.(t .eq. 41))goto 23015
      lev = lev - 1
23014 t=gnbtok(tok,100)
      goto 23013
23015 continue
      if (.not.(lev .ne. 0))goto 23016
      goto 99
23016 continue
23018 if (.not.(t .eq. 10))goto 23019
      t = gnbtok (tok, 100)
      goto 23018
23019 continue
      return
99    call synerr (19HInvalid case label.)
      n = 0
      end
