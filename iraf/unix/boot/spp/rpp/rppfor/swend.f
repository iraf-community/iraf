      subroutine swend (lab)
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
      integer lb, ub, n, i, j, swn
      integer sif(5)
      integer slt(10)
      integer sgt(5)
      integer sgoto(7)
      integer seq(5)
      integer sge(5)
      integer sle(5)
      integer sand(6)
      data sif(1)/105/,sif(2)/102/,sif(3)/32/,sif(4)/40/,sif(5)/-2/
      data slt(1)/46/,slt(2)/108/,slt(3)/116/,slt(4)/46/,slt(5)/49/,slt(
     *6)/46/,slt(7)/111/,slt(8)/114/,slt(9)/46/,slt(10)/-2/
      data sgt(1)/46/,sgt(2)/103/,sgt(3)/116/,sgt(4)/46/,sgt(5)/-2/
      data sgoto(1)/103/,sgoto(2)/111/,sgoto(3)/116/,sgoto(4)/111/,sgoto
     *(5)/32/,sgoto(6)/40/,sgoto(7)/-2/
      data seq(1)/46/,seq(2)/101/,seq(3)/113/,seq(4)/46/,seq(5)/-2/
      data sge(1)/46/,sge(2)/103/,sge(3)/101/,sge(4)/46/,sge(5)/-2/
      data sle(1)/46/,sle(2)/108/,sle(3)/101/,sle(4)/46/,sle(5)/-2/
      data sand(1)/46/,sand(2)/97/,sand(3)/110/,sand(4)/100/,sand(5)/46/
     *,sand(6)/-2/
      swn = swvstk(swvlev)
      swvlev = max0(0, swvlev - 1)
      lb = swstak (swtop + 3)
      ub = swstak (swlast - 2)
      n = swstak (swtop + 1)
      call outgo (lab + 1)
      if (.not.(swstak (swtop + 2) .eq. 0))goto 23000
      swstak (swtop + 2) = lab + 1
23000 continue
      xfer = 0
      call indent (-1)
      call outcon (lab)
      call indent (1)
      if (.not.(n .ge. 3 .and. ub - lb + 1 .lt. 2 * n))goto 23002
      if (.not.(lb .ne. 1))goto 23004
      call outtab
      call swvar (swn)
      call outch (61)
      call swvar (swn)
      if (.not.(lb .lt. 1))goto 23006
      call outch (43)
23006 continue
      call outnum (-lb + 1)
      call outdon
23004 continue
      if (.not.(swinrg .eq. 0))goto 23008
      call outtab
      call outstr (sif)
      call swvar (swn)
      call outstr (slt)
      call swvar (swn)
      call outstr (sgt)
      call outnum (ub - lb + 1)
      call outch (41)
      call outch (32)
      call outgo (swstak (swtop + 2))
23008 continue
      call outtab
      call outstr (sgoto)
      j = lb
      i = swtop + 3
23010 if (.not.(i .lt. swlast))goto 23012
23013 if (.not.(j .lt. swstak (i)))goto 23015
      call outnum (swstak (swtop + 2))
      call outch (44)
23014 j = j + 1
      goto 23013
23015 continue
      j = swstak (i + 1) - swstak (i)
23016 if (.not.(j .ge. 0))goto 23018
      call outnum (swstak (i + 2))
23017 j = j - 1
      goto 23016
23018 continue
      j = swstak (i + 1) + 1
      if (.not.(i .lt. swlast - 3))goto 23019
      call outch (44)
23019 continue
23011 i = i + 3
      goto 23010
23012 continue
      call outch (41)
      call outch (44)
      call swvar (swn)
      call outdon
      goto 23003
23002 continue
      if (.not.(n .gt. 0))goto 23021
      i = swtop + 3
23023 if (.not.(i .lt. swlast))goto 23025
      call outtab
      call outstr (sif)
      call swvar (swn)
      if (.not.(swstak (i) .eq. swstak (i+1)))goto 23026
      call outstr (seq)
      call outnum (swstak (i))
      goto 23027
23026 continue
      call outstr (sge)
      call outnum (swstak (i))
      call outstr (sand)
      call swvar (swn)
      call outstr (sle)
      call outnum (swstak (i + 1))
23027 continue
      call outch (41)
      call outch (32)
      call outgo (swstak (i + 2))
23024 i = i + 3
      goto 23023
23025 continue
      if (.not.(lab + 1 .ne. swstak (swtop + 2)))goto 23028
      call outgo (swstak (swtop + 2))
23028 continue
23021 continue
23003 continue
      call indent (-1)
      call outcon (lab + 1)
      swlast = swtop
      swtop = swstak (swtop)
      swinrg = 0
      return
      end
c     logic0  logical_column
