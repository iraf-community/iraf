      subroutine getdef (token, toksiz, defn, defsiz)
      integer token (100), defn (2048)
      integer toksiz, defsiz
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
      integer c, t, ptoken (100)
      integer gtok, ngetch
      integer i, nlpar
      call skpblk
      c = gtok (ptoken, 100)
      if (.not.(c .eq. 40))goto 23000
      t = 40
      goto 23001
23000 continue
      t = 32
      call pbstr (ptoken)
23001 continue
      call skpblk
      if (.not.(gtok (token, toksiz) .ne. -9))goto 23002
      call baderr (22Hnon-alphanumeric name.)
23002 continue
      call skpblk
      c = gtok (ptoken, 100)
      if (.not.(t .eq. 32))goto 23004
      call pbstr (ptoken)
      i = 1
23006 continue
      c = ngetch (c)
      if (.not.(i .gt. defsiz))goto 23009
      call baderr (20Hdefinition too long.)
23009 continue
      defn (i) = c
      i = i + 1
23007 if (.not.(c .eq. 35 .or. c .eq. 10 .or. c .eq. -1))goto 23006
23008 continue
      if (.not.(c .eq. 35))goto 23011
      call putbak (c)
23011 continue
      goto 23005
23004 continue
      if (.not.(t .eq. 40))goto 23013
      if (.not.(c .ne. 44))goto 23015
      call baderr (24Hmissing comma in define.)
23015 continue
      nlpar = 0
      i = 1
23017 if (.not.(nlpar .ge. 0))goto 23019
      if (.not.(i .gt. defsiz))goto 23020
      call baderr (20Hdefinition too long.)
      goto 23021
23020 continue
      if (.not.(ngetch (defn (i)) .eq. -1))goto 23022
      call baderr (20Hmissing right paren.)
      goto 23023
23022 continue
      if (.not.(defn (i) .eq. 40))goto 23024
      nlpar = nlpar + 1
      goto 23025
23024 continue
      if (.not.(defn (i) .eq. 41))goto 23026
      nlpar = nlpar - 1
23026 continue
23025 continue
23023 continue
23021 continue
23018 i = i + 1
      goto 23017
23019 continue
      goto 23014
23013 continue
      call baderr (19Hgetdef is confused.)
23014 continue
23005 continue
      defn (i - 1) = -2
      return
      end
c     logic0  logical_column
