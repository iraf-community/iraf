      subroutine evalr (argstk, i, j)
      integer argstk (100), i, j
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
      integer argno, k, m, n, t, td, instr0, delim
      external index
      integer index, length
      integer digits(11)
      data digits(1)/48/,digits(2)/49/,digits(3)/50/,digits(4)/51/,digit
     *s(5)/52/,digits(6)/53/,digits(7)/54/,digits(8)/55/,digits(9)/56/,d
     *igits(10)/57/,digits(11)/-2/
      t = argstk (i)
      td = evalst (t)
      if (.not.(td .eq. -10))goto 23000
      call domac (argstk, i, j)
      goto 23001
23000 continue
      if (.not.(td .eq. -12))goto 23002
      call doincr (argstk, i, j)
      goto 23003
23002 continue
      if (.not.(td .eq. -13))goto 23004
      call dosub (argstk, i, j)
      goto 23005
23004 continue
      if (.not.(td .eq. -11))goto 23006
      call doif (argstk, i, j)
      goto 23007
23006 continue
      if (.not.(td .eq. -14))goto 23008
      call doarth (argstk, i, j)
      goto 23009
23008 continue
      instr0 = 0
      k = t + length (evalst (t)) - 1
23010 if (.not.(k .gt. t))goto 23012
      if (.not.(evalst(k) .eq. 39 .or. evalst(k) .eq. 34))goto 23013
      if (.not.(instr0 .eq. 0))goto 23015
      delim = evalst(k)
      instr0 = 1
      goto 23016
23015 continue
      instr0 = 0
23016 continue
      call putbak (evalst(k))
      goto 23014
23013 continue
      if (.not.(evalst(k-1) .ne. 36 .or. instr0 .eq. 1))goto 23017
      call putbak (evalst (k))
      goto 23018
23017 continue
      argno = index (digits, evalst (k)) - 1
      if (.not.(argno .ge. 0 .and. argno .lt. j - i))goto 23019
      n = i + argno + 1
      m = argstk (n)
      call pbstr (evalst (m))
23019 continue
      k = k - 1
23018 continue
23014 continue
23011 k = k - 1
      goto 23010
23012 continue
      if (.not.(k .eq. t))goto 23021
      call putbak (evalst (k))
23021 continue
23009 continue
23007 continue
23005 continue
23003 continue
23001 continue
      return
      end
c     logic0  logical_column
c     instr0  in_string
