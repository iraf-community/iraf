      subroutine outdon
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
      integer allblk
      integer itoc, ip, op, i
      integer obuf(80)
      integer sline0(7)
      data sline0(1)/35/,sline0(2)/108/,sline0(3)/105/,sline0(4)/110/,sl
     *ine0(5)/101/,sline0(6)/32/,sline0(7)/-2/
      if (.not.(dbgout .eq. 1))goto 23000
      if (.not.((body .eq. 1 .or. dbglev .ne. level)
     * .and. linect (level) .gt. 0))goto 23002
      op = 1
      ip=1
23004 if (.not.(sline0(ip) .ne. -2))goto 23006
      obuf(op) = sline0(ip)
      op = op + 1
23005 ip=ip+1
      goto 23004
23006 continue
      op = op + itoc (linect, obuf(op), 80-op+1)
      obuf(op) = 32
      op = op + 1
      obuf(op) = 34
      op = op + 1
      i=fnamp-1
23007 if (.not.(i .ge. 1))goto 23009
      if (.not.(fnames(i-1) .eq. -2 .or. i .eq. 1))goto 23010
      ip=i
23012 if (.not.(fnames(ip) .ne. -2))goto 23014
      obuf(op) = fnames(ip)
      op = op + 1
23013 ip=ip+1
      goto 23012
23014 continue
      goto 23009
23010 continue
23008 i=i-1
      goto 23007
23009 continue
      obuf(op) = 34
      op = op + 1
      obuf(op) = 10
      op = op + 1
      obuf(op) = -2
      op = op + 1
      call putlin (obuf, 1)
      dbglev = level
23002 continue
23000 continue
      outbuf (outp + 1) = 10
      outbuf (outp + 2) = -2
      if (.not.(allblk (outbuf) .eq. 0))goto 23015
      call putlin (outbuf, 1)
23015 continue
      outp = 0
      return
      end
c     logic0  logical_column
c     sline0  s_line
