      subroutine dosub (argstk, i, j)
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
      integer ap, fc, k, nc
      integer ctoi, length
      if (.not.(j - i .lt. 3))goto 23000
      return
23000 continue
      if (.not.(j - i .lt. 4))goto 23002
      nc = 100
      goto 23003
23002 continue
      k = argstk (i + 4)
      nc = ctoi (evalst, k)
23003 continue
      k = argstk (i + 3)
      ap = argstk (i + 2)
      fc = ap + ctoi (evalst, k) - 1
      if (.not.(fc .ge. ap .and. fc .lt. ap + length (evalst (ap))))goto
     * 23004
      k = fc + min0(nc, length (evalst (fc))) - 1
23006 if (.not.(k .ge. fc))goto 23008
      call putbak (evalst (k))
23007 k = k - 1
      goto 23006
23008 continue
23004 continue
      return
      end
c     logic0  logical_column
