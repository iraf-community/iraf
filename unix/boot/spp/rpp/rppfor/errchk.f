      subroutine errchk
      integer tok, lastt0, gnbtok, token(100)
      integer ntok
      integer mktabl
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
      integer serrc0(27)
      integer serrd0(31)
      data serrc0(1)/108/,serrc0(2)/111/,serrc0(3)/103/,serrc0(4)/105/,s
     *errc0(5)/99/,serrc0(6)/97/,serrc0(7)/108/,serrc0(8)/32/,serrc0(9)/
     *120/,serrc0(10)/101/,serrc0(11)/114/,serrc0(12)/102/,serrc0(13)/10
     *8/,serrc0(14)/103/,serrc0(15)/44/,serrc0(16)/32/,serrc0(17)/120/,s
     *errc0(18)/101/,serrc0(19)/114/,serrc0(20)/112/,serrc0(21)/97/,serr
     *c0(22)/100/,serrc0(23)/40/,serrc0(24)/56/,serrc0(25)/52/,serrc0(26
     *)/41/,serrc0(27)/-2/
      data serrd0(1)/99/,serrd0(2)/111/,serrd0(3)/109/,serrd0(4)/109/,se
     *rrd0(5)/111/,serrd0(6)/110/,serrd0(7)/32/,serrd0(8)/47/,serrd0(9)/
     *120/,serrd0(10)/101/,serrd0(11)/114/,serrd0(12)/99/,serrd0(13)/111
     */,serrd0(14)/109/,serrd0(15)/47/,serrd0(16)/32/,serrd0(17)/120/,se
     *rrd0(18)/101/,serrd0(19)/114/,serrd0(20)/102/,serrd0(21)/108/,serr
     *d0(22)/103/,serrd0(23)/44/,serrd0(24)/32/,serrd0(25)/120/,serrd0(2
     *6)/101/,serrd0(27)/114/,serrd0(28)/112/,serrd0(29)/97/,serrd0(30)/
     *100/,serrd0(31)/-2/
      ntok = 0
      tok = 0
23000 continue
      lastt0 = tok
      tok = gnbtok (token, 100)
      I23003=(tok)
      goto 23003
23005 continue
      if (.not.(errtbl .eq. 0))goto 23006
      errtbl = mktabl(0)
      call outtab
      call outstr (serrc0)
      call outdon
      call outtab
      call outstr (serrd0)
      call outdon
23006 continue
      call enter (token, 0, errtbl)
      goto 23004
23008 continue
      goto 23004
23009 continue
      if (.not.(lastt0 .ne. 44))goto 23010
      goto 23002
23010 continue
      goto 23004
23012 continue
      call synerr (35HSyntax error in ERRCHK declaration.)
      goto 23004
23003 continue
      if (I23003.eq.-9)goto 23005
      if (I23003.eq.10)goto 23009
      if (I23003.eq.44)goto 23008
      goto 23012
23004 continue
23001 goto 23000
23002 continue
      end
c     lastt0  last_tok
c     logic0  logical_column
c     serrc0  serrcom1
c     serrd0  serrcom2
