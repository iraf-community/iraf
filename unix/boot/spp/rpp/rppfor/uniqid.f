      subroutine uniqid (id)
      integer id (100)
      integer i, j, junk, idchl
      external index
      integer lookup, index, length
      integer start (6)
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
      integer idch(37)
      data idch(1)/48/,idch(2)/49/,idch(3)/50/,idch(4)/51/,idch(5)/52/,i
     *dch(6)/53/,idch(7)/54/,idch(8)/55/,idch(9)/56/,idch(10)/57/,idch(1
     *1)/97/,idch(12)/98/,idch(13)/99/,idch(14)/100/,idch(15)/101/,idch(
     *16)/102/,idch(17)/103/,idch(18)/104/,idch(19)/105/,idch(20)/106/,i
     *dch(21)/107/,idch(22)/108/,idch(23)/109/,idch(24)/110/,idch(25)/11
     *1/,idch(26)/112/,idch(27)/113/,idch(28)/114/,idch(29)/115/,idch(30
     *)/116/,idch(31)/117/,idch(32)/118/,idch(33)/119/,idch(34)/120/,idc
     *h(35)/121/,idch(36)/122/,idch(37)/-2/
      i = 1
23000 if (.not.(id (i) .ne. -2))goto 23002
23001 i = i + 1
      goto 23000
23002 continue
23003 if (.not.(i .le. 6))goto 23005
      id (i) = 48
23004 i = i + 1
      goto 23003
23005 continue
      i = 6 + 1
      id (i) = -2
      id (i - 1) = 48
      if (.not.(lookup (id, junk, gentbl) .eq. 1))goto 23006
      idchl = length (idch)
      i = 2
23008 if (.not.(i .lt. 6))goto 23010
      start (i) = id (i)
23009 i = i + 1
      goto 23008
23010 continue
23011 continue
      i = 6 - 1
23014 if (.not.(i .gt. 1))goto 23016
      j = mod (index (idch, id (i)), idchl) + 1
      id (i) = idch (j)
      if (.not.(id (i) .ne. start (i)))goto 23017
      goto 23016
23017 continue
23015 i = i - 1
      goto 23014
23016 continue
      if (.not.(i .eq. 1))goto 23019
      call baderr (30Hcannot make identifier unique.)
23019 continue
23012 if (.not.(lookup (id, junk, gentbl) .eq. 0))goto 23011
23013 continue
23006 continue
      end
c     logic0  logical_column
