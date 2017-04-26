      subroutine synerr (msg)
      integer msg
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
      integer lc (20)
      integer i, junk
      integer itoc
      integer of(5)
      integer errmsg(100)
      data of(1)/32/,of(2)/111/,of(3)/102/,of(4)/32/,of(5)/-2/
      data errmsg(1)/69/,errmsg(2)/114/,errmsg(3)/114/,errmsg(4)/111/,er
     *rmsg(5)/114/,errmsg(6)/32/,errmsg(7)/111/,errmsg(8)/110/,errmsg(9)
     */32/,errmsg(10)/108/,errmsg(11)/105/,errmsg(12)/110/,errmsg(13)/10
     *1/,errmsg(14)/32/,errmsg(15)/-2/
      call putlin (errmsg, 2)
      if (.not.(level .ge. 1))goto 23000
      i = level
      goto 23001
23000 continue
      i = 1
23001 continue
      junk = itoc (linect (i), lc, 20)
      call putlin (lc, 2)
      i = fnamp - 1
23002 if (.not.(i .ge. 1))goto 23004
      if (.not.(fnames (i - 1) .eq. -2 .or. i .eq. 1))goto 23005
      call putlin (of, 2)
      call putlin (fnames (i), 2)
      goto 23004
23005 continue
23003 i = i - 1
      goto 23002
23004 continue
      call putch (58, 2)
      call putch (32, 2)
      call remark (msg)
      return
      end
c     logic0  logical_column
