      subroutine swcode (lab)
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
      integer tok (100)
      integer labgen, gnbtok
      lab = labgen (2)
      swvnum = swvnum + 1
      swvlev = swvlev + 1
      if (.not.(swvlev .gt. 10))goto 23000
      call baderr (27Hswitches nested too deeply.)
23000 continue
      swvstk(swvlev) = swvnum
      if (.not.(swlast + 3 .gt. 1000))goto 23002
      call baderr (22Hswitch table overflow.)
23002 continue
      swstak (swlast) = swtop
      swstak (swlast + 1) = 0
      swstak (swlast + 2) = 0
      swtop = swlast
      swlast = swlast + 3
      xfer = 0
      call outtab
      call swvar (swvnum)
      call outch (61)
      call balpar
      call outdwe
      call outgo (lab)
      call indent (1)
      xfer = 1
23004 if (.not.(gnbtok (tok, 100) .eq. 10))goto 23005
      goto 23004
23005 continue
      if (.not.(tok (1) .ne. 123))goto 23006
      call synerr (39Hmissing left brace in switch statement.)
      call pbstr (tok)
23006 continue
      return
      end
c     logic0  logical_column
