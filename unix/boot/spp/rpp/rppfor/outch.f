      subroutine outch (c)
      integer c, splbuf(8+1)
      integer i, ip, op, index
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
      external index
      integer break0(10)
      data break0(1)/32/,break0(2)/41/,break0(3)/44/,break0(4)/46/,break
     *0(5)/43/,break0(6)/45/,break0(7)/42/,break0(8)/47/,break0(9)/40/,b
     *reak0(10)/-2/
      if (.not.(outp .ge. 72))goto 23000
      if (.not.(index (break0, c) .gt. 0))goto 23002
      ip = outp
      goto 23003
23002 continue
      ip=outp
23004 if (.not.(ip .ge. 1))goto 23006
      if (.not.(index (break0, outbuf(ip)) .gt. 0))goto 23007
      goto 23006
23007 continue
23005 ip=ip-1
      goto 23004
23006 continue
23003 continue
      if (.not.(ip .ne. outp .and. (outp-ip) .lt. 8))goto 23009
      op = 1
      i=ip+1
23011 if (.not.(i .le. outp))goto 23013
      splbuf(op) = outbuf(i)
      op = op + 1
23012 i=i+1
      goto 23011
23013 continue
      splbuf(op) = -2
      outp = ip
      goto 23010
23009 continue
      splbuf(1) = -2
23010 continue
      call outdon
      op=1
23014 if (.not.(op .lt. col))goto 23016
      outbuf(op) = 32
23015 op=op+1
      goto 23014
23016 continue
      outbuf(6) = 42
      outp = col
      ip=1
23017 if (.not.(splbuf(ip) .ne. -2))goto 23019
      outp = outp + 1
      outbuf(outp) = splbuf(ip)
23018 ip=ip+1
      goto 23017
23019 continue
23000 continue
      outp = outp + 1
      outbuf(outp) = c
      end
c     logic0  logical_column
c     break0  break_chars
