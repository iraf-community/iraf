      integer function lex (lexstr)
      integer lexstr (100)
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
      integer gnbtok, t, c
      integer lookup, n
      integer sdefa0(8)
      data sdefa0(1)/100/,sdefa0(2)/101/,sdefa0(3)/102/,sdefa0(4)/97/,sd
     *efa0(5)/117/,sdefa0(6)/108/,sdefa0(7)/116/,sdefa0(8)/-2/
      lex = gnbtok (lexstr, 100)
23000 if (.not.(lex .eq. 10))goto 23002
23001  lex = gnbtok (lexstr, 100)
      goto 23000
23002 continue
      if (.not.(lex .eq. -1 .or. lex .eq. 59 .or. lex .eq. 123 .or. lex 
     *.eq. 125))goto 23003
      return
23003 continue
      if (.not.(lex .eq. 48))goto 23005
      lex = -89
      goto 23006
23005 continue
      if (.not.(lex .eq. 37))goto 23007
      lex = -85
      goto 23008
23007 continue
      if (.not.(lex .eq.  -166))goto 23009
      lex = -67
      goto 23010
23009 continue
      if (.not.(lookup (lexstr, lex, rkwtbl) .eq. 1))goto 23011
      if (.not.(lex .eq. -90))goto 23013
      n = -1
23015 continue
      c = ngetch (c)
      n = n + 1
23016 if (.not.(c .ne. 32 .and. c .ne. 9))goto 23015
23017 continue
      call putbak (c)
      t = gnbtok (lexstr, 100)
      call pbstr (lexstr)
      if (.not.(n .gt. 0))goto 23018
      call putbak (32)
23018 continue
      call scopy (sdefa0, 1, lexstr, 1)
      if (.not.(t .ne. 58))goto 23020
      lex = -80
23020 continue
23013 continue
      goto 23012
23011 continue
      lex = -80
23012 continue
23010 continue
23008 continue
23006 continue
      return
      end
c     logic0  logical_column
c     sdefa0  sdefault
