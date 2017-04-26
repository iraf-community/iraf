      subroutine iferrc (lab, sense)
      integer lab, sense
      integer labgen, nlpar
      integer t, gettok, gnbtok, token(100)
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
      integer errpsh(12)
      integer siferr(20)
      integer sifno0(15)
      data errpsh(1)/99/,errpsh(2)/97/,errpsh(3)/108/,errpsh(4)/108/,err
     *psh(5)/32/,errpsh(6)/120/,errpsh(7)/101/,errpsh(8)/114/,errpsh(9)/
     *112/,errpsh(10)/115/,errpsh(11)/104/,errpsh(12)/-2/
      data siferr(1)/105/,siferr(2)/102/,siferr(3)/32/,siferr(4)/40/,sif
     *err(5)/46/,siferr(6)/110/,siferr(7)/111/,siferr(8)/116/,siferr(9)/
     *46/,siferr(10)/120/,siferr(11)/101/,siferr(12)/114/,siferr(13)/112
     */,siferr(14)/111/,siferr(15)/112/,siferr(16)/40/,siferr(17)/41/,si
     *ferr(18)/41/,siferr(19)/32/,siferr(20)/-2/
      data sifno0(1)/105/,sifno0(2)/102/,sifno0(3)/32/,sifno0(4)/40/,sif
     *no0(5)/120/,sifno0(6)/101/,sifno0(7)/114/,sifno0(8)/112/,sifno0(9)
     */111/,sifno0(10)/112/,sifno0(11)/40/,sifno0(12)/41/,sifno0(13)/41/
     *,sifno0(14)/32/,sifno0(15)/-2/
      xfer = 0
      lab = labgen (3)
      call outtab
      call outstr (errpsh)
      call outdon
      I23000=(gnbtok (token, 100))
      goto 23000
23002 continue
      call outtab
      goto 23001
23003 continue
      call pbstr (token)
      esp = esp + 1
      if (.not.(esp .ge. 30))goto 23004
      call baderr (35HIferr statements nested too deeply.)
23004 continue
      errstk(esp) = lab
      return
23006 continue
      call synerr (19HMissing left paren.)
      return
23000 continue
      if (I23000.eq.40)goto 23002
      if (I23000.eq.123)goto 23003
      goto 23006
23001 continue
      nlpar = 1
      token(1) = -2
      esp = esp + 1
      if (.not.(esp .ge. 30))goto 23007
      call baderr (35HIferr statements nested too deeply.)
23007 continue
      errstk(esp) = 0
23009 continue
      call outstr (token)
      t = gettok (token, 100)
      if (.not.(t .eq. 59 .or. t .eq. 123 .or. t .eq. 125 .or. t .eq. -1
     *))goto 23012
      call pbstr (token)
      goto 23011
23012 continue
      if (.not.(t .eq. 10))goto 23014
      token (1) = -2
      goto 23015
23014 continue
      if (.not.(t .eq. 40))goto 23016
      nlpar = nlpar + 1
      goto 23017
23016 continue
      if (.not.(t .eq. 41))goto 23018
      nlpar = nlpar - 1
      goto 23019
23018 continue
      if (.not.(t .eq. 59))goto 23020
      call outdon
      call outtab
      goto 23021
23020 continue
      if (.not.(t .eq. -9))goto 23022
      call squash (token)
23022 continue
23021 continue
23019 continue
23017 continue
23015 continue
23010 if (.not.(nlpar .le. 0))goto 23009
23011 continue
      esp = esp - 1
      ername = 0
      if (.not.(nlpar .ne. 0))goto 23024
      call synerr (33HMissing parenthesis in condition.)
      goto 23025
23024 continue
      call outdon
23025 continue
      call outtab
      if (.not.(sense .eq. 1))goto 23026
      call outstr (siferr)
      goto 23027
23026 continue
      call outstr (sifno0)
23027 continue
      call outgo (lab)
      call indent (1)
      return
      end
c     sifno0  sifnoerr
c     logic0  logical_column
