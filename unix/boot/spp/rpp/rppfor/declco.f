      subroutine declco (id)
      integer id(100)
      integer newid(100), tok, tokbl
      integer junk, ludef, equal, gettok
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
      integer xptyp(9)
      integer xpntr(7)
      integer xfunc(7)
      integer xsubr(7)
      data xptyp(1)/105/,xptyp(2)/110/,xptyp(3)/116/,xptyp(4)/101/,xptyp
     *(5)/103/,xptyp(6)/101/,xptyp(7)/114/,xptyp(8)/32/,xptyp(9)/-2/
      data xpntr(1)/120/,xpntr(2)/36/,xpntr(3)/112/,xpntr(4)/110/,xpntr(
     *5)/116/,xpntr(6)/114/,xpntr(7)/-2/
      data xfunc(1)/120/,xfunc(2)/36/,xfunc(3)/102/,xfunc(4)/117/,xfunc(
     *5)/110/,xfunc(6)/99/,xfunc(7)/-2/
      data xsubr(1)/120/,xsubr(2)/36/,xsubr(3)/115/,xsubr(4)/117/,xsubr(
     *5)/98/,xsubr(6)/114/,xsubr(7)/-2/
      if (.not.(ludef (id, newid, xpptbl) .eq. 1))goto 23000
      if (.not.(equal (id, xpntr) .eq. 1))goto 23002
      tokbl = gettok (newid, 100)
      if (.not.(tokbl .eq. 32))goto 23004
      tok = gettok (newid, 100)
      goto 23005
23004 continue
      tok = tokbl
23005 continue
      if (.not.(tok .eq.  -166 .and. equal (newid, xfunc) .eq. 1))goto 2
     *3006
      call outtab
      call outstr (xptyp)
      junk = ludef (newid, newid, xpptbl)
      call outstr (newid)
      call eatup
      call outdon
      call poicod (0)
      goto 23007
23006 continue
      call pbstr (newid)
      call poicod (1)
23007 continue
      goto 23003
23002 continue
      if (.not.(equal (id, xsubr) .eq. 1))goto 23008
      call outtab
      call outstr (newid)
      call eatup
      call outdon
      goto 23009
23008 continue
      call outtab
      call outstr (newid)
      call outch (32)
23009 continue
23003 continue
      goto 23001
23000 continue
      call synerr (32HInvalid x$type type declaration.)
23001 continue
      end
c     logic0  logical_column
