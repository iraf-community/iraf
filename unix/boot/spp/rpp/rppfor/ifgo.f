      subroutine ifgo (lab)
      integer lab
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
      integer ifnot(10)
      integer serrc0(21)
      data ifnot(1)/105/,ifnot(2)/102/,ifnot(3)/32/,ifnot(4)/40/,ifnot(5
     *)/46/,ifnot(6)/110/,ifnot(7)/111/,ifnot(8)/116/,ifnot(9)/46/,ifnot
     *(10)/-2/
      data serrc0(1)/46/,serrc0(2)/97/,serrc0(3)/110/,serrc0(4)/100/,ser
     *rc0(5)/46/,serrc0(6)/40/,serrc0(7)/46/,serrc0(8)/110/,serrc0(9)/11
     *1/,serrc0(10)/116/,serrc0(11)/46/,serrc0(12)/120/,serrc0(13)/101/,
     *serrc0(14)/114/,serrc0(15)/102/,serrc0(16)/108/,serrc0(17)/103/,se
     *rrc0(18)/41/,serrc0(19)/41/,serrc0(20)/32/,serrc0(21)/-2/
      call outtab
      call outstr (ifnot)
      call balpar
      if (.not.(ername .eq. 1))goto 23000
      call outstr (serrc0)
      goto 23001
23000 continue
      call outch (41)
      call outch (32)
23001 continue
      call outgo (lab)
      call errgo
      end
c     logic0  logical_column
c     serrc0  serrchk
