      integer function gettok (token, toksiz)
      integer token (100)
      integer toksiz
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
      integer equal
      integer t, deftok
      integer ssubr(7)
      integer sfunc(7)
      data ssubr(1)/120/,ssubr(2)/36/,ssubr(3)/115/,ssubr(4)/117/,ssubr(
     *5)/98/,ssubr(6)/114/,ssubr(7)/-2/
      data sfunc(1)/120/,sfunc(2)/36/,sfunc(3)/102/,sfunc(4)/117/,sfunc(
     *5)/110/,sfunc(6)/99/,sfunc(7)/-2/
      gettok = deftok (token, toksiz)
      if (.not.(gettok .ne. -1))goto 23000
      if (.not.(gettok .eq.  -166))goto 23002
      if (.not.(equal (token, sfunc) .eq. 1))goto 23004
      call skpblk
      t = deftok (fcname, 30)
      call pbstr (fcname)
      if (.not.(t .ne. -9))goto 23006
      call synerr (22HMissing function name.)
23006 continue
      call putbak (32)
      swvnum = 0
      swvlev = 0
      return
23004 continue
      if (.not.(equal (token, ssubr) .eq. 1))goto 23008
      swvnum = 0
      swvlev = 0
      return
23008 continue
      return
23009 continue
23005 continue
23002 continue
      return
23000 continue
      token (1) = -1
      token (2) = -2
      gettok = -1
      return
      end
c     logic0  logical_column
