      subroutine endcod (endstr)
      integer endstr(1)
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
      integer sret(7)
      integer sepro(12)
      data sret(1)/114/,sret(2)/101/,sret(3)/116/,sret(4)/117/,sret(5)/1
     *14/,sret(6)/110/,sret(7)/-2/
      data sepro(1)/99/,sepro(2)/97/,sepro(3)/108/,sepro(4)/108/,sepro(5
     *)/32/,sepro(6)/122/,sepro(7)/122/,sepro(8)/101/,sepro(9)/112/,sepr
     *o(10)/114/,sepro(11)/111/,sepro(12)/-2/
      if (.not.(esp .ne. 0))goto 23000
      call synerr (36HUnmatched 'iferr' or 'then' keyword.)
23000 continue
      esp = 0
      body = 0
      ername = 0
      if (.not.(errtbl .ne. 0))goto 23002
      call rmtabl (errtbl)
23002 continue
      errtbl = 0
      memflg = 0
      if (.not.(retlab .ne. 0))goto 23004
      call outnum (retlab)
23004 continue
      call outtab
      call outstr (sepro)
      call outdon
      call outtab
      call outstr (sret)
      call outdon
      col = 6
      call outtab
      call outstr (endstr)
      call outdon
      end
c     logic0  logical_column
