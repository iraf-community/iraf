      integer function ngetch (c)
      integer c
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
      integer getlin, n, i
      if (.not.(buf (bp) .eq. -2))goto 23000
      if (.not.(getlin (buf (3192), infile (level)) .eq. -1))goto 23002
      c = -1
      goto 23003
23002 continue
      c = buf (3192)
      bp = 3192 + 1
      if (.not.(c .eq. 35))goto 23004
      if (.not.(buf(bp) .eq. 33 .and. buf(bp+1) .eq. 35))goto 23006
      n = 0
      i=bp+3
23008 if (.not.(buf(i) .ge. 48 .and. buf(i) .le. 57))goto 23010
      n = n * 10 + buf(i) - 48
23009 i=i+1
      goto 23008
23010 continue
      linect (level) = n - 1
23006 continue
23004 continue
      if (.not.(linect (level) .gt. 0))goto23003
      linect (level) = linect (level) + 1
23003 continue
      goto 23001
23000 continue
      c = buf (bp)
      bp = bp + 1
23001 continue
      ngetch=(c)
      return
      end
c     logic0  logical_column
