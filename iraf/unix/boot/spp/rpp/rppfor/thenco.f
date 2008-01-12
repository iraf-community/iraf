      subroutine thenco (tok, lab)
      integer lab, tok
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
      integer siferr(20)
      integer sifno0(15)
      data siferr(1)/105/,siferr(2)/102/,siferr(3)/32/,siferr(4)/40/,sif
     *err(5)/46/,siferr(6)/110/,siferr(7)/111/,siferr(8)/116/,siferr(9)/
     *46/,siferr(10)/120/,siferr(11)/101/,siferr(12)/114/,siferr(13)/112
     */,siferr(14)/111/,siferr(15)/112/,siferr(16)/40/,siferr(17)/41/,si
     *ferr(18)/41/,siferr(19)/32/,siferr(20)/-2/
      data sifno0(1)/105/,sifno0(2)/102/,sifno0(3)/32/,sifno0(4)/40/,sif
     *no0(5)/120/,sifno0(6)/101/,sifno0(7)/114/,sifno0(8)/112/,sifno0(9)
     */111/,sifno0(10)/112/,sifno0(11)/40/,sifno0(12)/41/,sifno0(13)/41/
     *,sifno0(14)/32/,sifno0(15)/-2/
      xfer = 0
      call outnum (lab+2)
      call outtab
      if (.not.(tok .eq. -98))goto 23000
      call outstr (siferr)
      goto 23001
23000 continue
      call outstr (sifno0)
23001 continue
      call outgo (lab)
      esp = esp - 1
      call indent (1)
      return
      end
c     sifno0  sifnoerr
c     logic0  logical_column
