      subroutine ratfor
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
      common /cline/ level, linect (5), infile (5), fnamp, fnames ( 150)
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
      integer i, n
      integer getarg, rfopen
      integer arg (30)
      integer defns(1)
      data defns(1)/-2/
      call initkw
      if (.not.(defns (1) .ne. -2))goto 23000
      infile (1) = rfopen(defns, 1)
      if (.not.(infile (1) .eq. -3))goto 23002
      call remark (37Hcan't open standard definitions file.)
      goto 23003
23002 continue
      call finit
      call parse
      call rfclos(infile (1))
23003 continue
23000 continue
      n = 1
      i = 1
23004 if (.not.(getarg (i, arg, 30) .ne. -1))goto 23006
      n = n + 1
      call query (32Husage:  ratfor [files] >outfile.)
      if (.not.(arg (1) .eq. 45 .and. arg (2) .eq. -2))goto 23007
      infile (1) = 0
      call finit
      goto 23008
23007 continue
      infile (1) = rfopen(arg, 1)
      if (.not.(infile (1) .eq. -3))goto 23009
      call cant (arg)
      goto 23010
23009 continue
      call finit
      call scopy (arg, 1, fnames, 1)
      fnamp=1
23011 if (.not.(fnames(fnamp) .ne. -2))goto 23013
      if (.not.(fnames(fnamp) .eq. 46 .and. fnames(fnamp+1) .eq. 114))go
     *to 23014
      fnames(fnamp+1) = 120
23014 continue
23012 fnamp=fnamp+1
      goto 23011
23013 continue
23010 continue
23008 continue
      call parse
      if (.not.(infile (1) .ne. 0))goto 23016
      call rfclos(infile (1))
23016 continue
23005 i = i + 1
      goto 23004
23006 continue
      if (.not.(n .eq. 1))goto 23018
      infile (1) = 0
      call finit
      call parse
23018 continue
      call lndict
      end
c     logic0  logical_column
