      subroutine cascod (lab, token)
      integer lab, token
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
      integer t, l, lb, ub, i, j, junk
      integer caslab, labgen, gnbtok
      integer tok (100)
      if (.not.(swtop .le. 0))goto 23000
      call synerr (24Hillegal case or default.)
      return
23000 continue
      call indent (-1)
      call outgo (lab + 1)
      xfer = 1
      l = labgen (1)
      if (.not.(token .eq. -91))goto 23002
23004 if (.not.(caslab (lb, t) .ne. -1))goto 23005
      ub = lb
      if (.not.(t .eq. 45))goto 23006
      junk = caslab (ub, t)
23006 continue
      if (.not.(lb .gt. ub))goto 23008
      call synerr (28Hillegal range in case label.)
      ub = lb
23008 continue
      if (.not.(swlast + 3 .gt. 1000))goto 23010
      call baderr (22Hswitch table overflow.)
23010 continue
      i = swtop + 3
23012 if (.not.(i .lt. swlast))goto 23014
      if (.not.(lb .le. swstak (i)))goto 23015
      goto 23014
23015 continue
      if (.not.(lb .le. swstak (i+1)))goto 23017
      call synerr (21Hduplicate case label.)
23017 continue
23016 continue
23013 i = i + 3
      goto 23012
23014 continue
      if (.not.(i .lt. swlast .and. ub .ge. swstak (i)))goto 23019
      call synerr (21Hduplicate case label.)
23019 continue
      j = swlast
23021 if (.not.(j .gt. i))goto 23023
      swstak (j+2) = swstak (j-1)
23022 j = j - 1
      goto 23021
23023 continue
      swstak (i) = lb
      swstak (i + 1) = ub
      swstak (i + 2) = l
      swstak (swtop + 1) = swstak (swtop + 1) + 1
      swlast = swlast + 3
      if (.not.(t .eq. 58))goto 23024
      goto 23005
23024 continue
      if (.not.(t .ne. 44))goto 23026
      call synerr (20Hillegal case syntax.)
23026 continue
23025 continue
      goto 23004
23005 continue
      goto 23003
23002 continue
      t = gnbtok (tok, 100)
      if (.not.(swstak (swtop + 2) .ne. 0))goto 23028
      call error (38Hmultiple defaults in switch statement.)
      goto 23029
23028 continue
      swstak (swtop + 2) = l
23029 continue
23003 continue
      if (.not.(t .eq. -1))goto 23030
      call synerr (15Hunexpected EOF.)
      goto 23031
23030 continue
      if (.not.(t .ne. 58))goto 23032
      call error (39Hmissing colon in case or default label.)
23032 continue
23031 continue
      xfer = 0
      call outcon (l)
      call indent (1)
      return
      end
c     logic0  logical_column
