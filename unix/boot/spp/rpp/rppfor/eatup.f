      subroutine eatup
      integer ptoken (100), t, token (100)
      integer gettok
      integer nlpar, equal
      common /cdefio/ bp, buf (4096)
      integer bp
      integer buf
      common /cfname/ fcname (30)
      integer fcname
      common /cfor/ fordep, forstk (200)
      integer fordep
      integer forstk
      common /cgoto/ xfer
      integer xfer
      common /clabel/ label, retlab, memflg, col, logic0
      integer label
      integer retlab
      integer memflg
      integer col
      integer logic0
      common /cline/ dbgout, dbglev, level, linect (5), infile (5), fnam
     *p, fnames ( 150)
      integer dbgout
      integer dbglev
      integer level
      integer linect
      integer infile
      integer fnamp
      integer fnames
      common /cmacro/ cp, ep, evalst (500), deftbl
      integer cp
      integer ep
      integer evalst
      integer deftbl
      common /coutln/ outp, outbuf (74)
      integer outp
      integer outbuf
      common /csbuf/ sbp, sbuf(2048), smem(240)
      integer sbp
      integer sbuf
      integer smem
      common /cswtch/ swtop, swlast, swstak(1000), swvnum, swvlev, swvst
     *k(10), swinrg
      integer swtop
      integer swlast
      integer swstak
      integer swvnum
      integer swvlev
      integer swvstk
      integer swinrg
      common /ckword/ rkwtbl
      integer rkwtbl
      common /clname/ fkwtbl, namtbl, gentbl, errtbl, xpptbl
      integer fkwtbl
      integer namtbl
      integer gentbl
      integer errtbl
      integer xpptbl
      common /erchek/ ername, body, esp, errstk(30)
      integer ername
      integer body
      integer esp
      integer errstk
      integer mem( 60000)
      common/cdsmem/mem
      integer serror(6)
      data serror(1)/101/,serror(2)/114/,serror(3)/114/,serror(4)/111/,s
     *error(5)/114/,serror(6)/-2/
      nlpar = 0
      token(1) = -2
23000 continue
      call outstr (token)
      t = gettok (token, 100)
23001 if (.not.(t .ne. 32 .and. t .ne. 9))goto 23000
23002 continue
      if (.not.(t .eq. -9))goto 23003
      if (.not.(equal (token, serror) .eq. 1))goto 23005
      ername = 1
23005 continue
23003 continue
      goto 10
23007 continue
      t = gettok (token, 100)
10    if (.not.(t .eq. 59 .or. t .eq. 10))goto 23010
      goto 23009
23010 continue
      if (.not.(t .eq. 125 .or. t .eq. 123))goto 23012
      call pbstr (token)
      goto 23009
23012 continue
      if (.not.(t .eq. -1))goto 23014
      call synerr (15Hunexpected EOF.)
      call pbstr (token)
      goto 23009
23014 continue
      if (.not.(t .eq. 44 .or. t .eq. 43 .or. t .eq. 45 .or. t .eq. 42 .
     *or. (t .eq. 47 .and. body .eq. 1) .or. t .eq. 40 .or. t .eq. 38 .o
     *r. t .eq. 124 .or. t .eq. 33 .or. t .eq. 126 .or. t .eq. 126 .or. 
     *t .eq. 94 .or. t .eq. 61 .or. t .eq. 95))goto 23016
23018 if (.not.(gettok (ptoken, 100) .eq. 10))goto 23019
      goto 23018
23019 continue
      call pbstr (ptoken)
      if (.not.(t .eq. 95))goto 23020
      token (1) = -2
23020 continue
23016 continue
      if (.not.(t .eq. 40))goto 23022
      nlpar = nlpar + 1
      goto 23023
23022 continue
      if (.not.(t .eq. 41))goto 23024
      nlpar = nlpar - 1
23024 continue
23023 continue
      if (.not.(t .eq. -9))goto 23026
      call squash (token)
23026 continue
      call outstr (token)
23008 if (.not.(nlpar .lt. 0))goto 23007
23009 continue
      if (.not.(nlpar .ne. 0))goto 23028
      call synerr (23Hunbalanced parentheses.)
23028 continue
      return
      end
c     logic0  logical_column
