      subroutine brknxt (sp, lextyp, labval, token)
      integer labval (100), lextyp (100), sp, token
      integer i, n
      integer alldig, ctoi
      integer t, ptoken (100)
      integer gnbtok
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
      n = 0
      t = gnbtok (ptoken, 100)
      if (.not.(alldig (ptoken) .eq. 1))goto 23000
      i = 1
      n = ctoi (ptoken, i) - 1
      goto 23001
23000 continue
      if (.not.(t .ne. 59))goto 23002
      call pbstr (ptoken)
23002 continue
23001 continue
      i = sp
23004 if (.not.(i .gt. 0))goto 23006
      if (.not.(lextyp (i) .eq. -95 .or. lextyp (i) .eq. -96 .or. lextyp
     * (i) .eq. -94 .or. lextyp (i) .eq. -93))goto 23007
      if (.not.(n .gt. 0))goto 23009
      n = n - 1
      goto 23005
23009 continue
      if (.not.(token .eq. -79))goto 23011
      call outgo (labval (i) + 1)
      goto 23012
23011 continue
      call outgo (labval (i))
23012 continue
23010 continue
      xfer = 1
      return
23007 continue
23005 i = i - 1
      goto 23004
23006 continue
      if (.not.(token .eq. -79))goto 23013
      call synerr (14Hillegal break.)
      goto 23014
23013 continue
      call synerr (13Hillegal next.)
23014 continue
      return
      end
c     logic0  logical_column
