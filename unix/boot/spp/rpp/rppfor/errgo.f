      subroutine errgo
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
      integer serrc0(13)
      data serrc0(1)/105/,serrc0(2)/102/,serrc0(3)/32/,serrc0(4)/40/,ser
     *rc0(5)/120/,serrc0(6)/101/,serrc0(7)/114/,serrc0(8)/102/,serrc0(9)
     */108/,serrc0(10)/103/,serrc0(11)/41/,serrc0(12)/32/,serrc0(13)/-2/
      if (.not.(ername .eq. 1))goto 23000
      call outtab
      if (.not.(esp .gt. 0))goto 23002
      if (.not.(errstk(esp) .gt. 0))goto 23004
      call outstr (serrc0)
      call ogotos (errstk(esp)+2, 0)
23004 continue
      goto 23003
23002 continue
      call outstr (serrc0)
      call ogotos (retlab, 0)
      call outdon
23003 continue
      ername = 0
23000 continue
      end
c     logic0  logical_column
c     serrc0  serrchk
