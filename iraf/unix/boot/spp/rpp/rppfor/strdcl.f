      subroutine strdcl
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
      integer t, token (100), dchar (100)
      integer gnbtok
      integer i, j, k, n, len
      integer length, ctoi, lex
      integer char(11)
      integer dat(6)
      integer eoss(3)
      data char(1)/105/,char(2)/110/,char(3)/116/,char(4)/101/,char(5)/1
     *03/,char(6)/101/,char(7)/114/,char(8)/42/,char(9)/50/,char(10)/47/
     *,char(11)/-2/
      data dat(1)/100/,dat(2)/97/,dat(3)/116/,dat(4)/97/,dat(5)/32/,dat(
     *6)/-2/
      data eoss(1)/48/,eoss(2)/47/,eoss(3)/-2/
      t = gnbtok (token, 100)
      if (.not.(t .ne. -9))goto 23000
      call synerr (21Hmissing string token.)
23000 continue
      call squash (token)
      call outtab
      call pbstr (char)
23002 continue
      t = gnbtok (dchar, 100)
      if (.not.(t .eq. 47))goto 23005
      goto 23004
23005 continue
      call outstr (dchar)
23003 goto 23002
23004 continue
      call outch (32)
      call outstr (token)
      call addstr (token, sbuf, sbp, 2048)
      call addchr (-2, sbuf, sbp, 2048)
      if (.not.(gnbtok (token, 100) .ne. 40))goto 23007
      len = length (token) + 1
      if (.not.(token (1) .eq. 39 .or. token (1) .eq. 34))goto 23009
      len = len - 2
23009 continue
      goto 23008
23007 continue
      t = gnbtok (token, 100)
      i = 1
      len = ctoi (token, i)
      if (.not.(token (i) .ne. -2))goto 23011
      call synerr (20Hinvalid string size.)
23011 continue
      if (.not.(gnbtok (token, 100) .ne. 41))goto 23013
      call synerr (20Hmissing right paren.)
      goto 23014
23013 continue
      t = gnbtok (token, 100)
23014 continue
23008 continue
      call outch (40)
      call outnum (len)
      call outch (41)
      call outdon
      if (.not.(token (1) .eq. 39 .or. token (1) .eq. 34))goto 23015
      len = length (token)
      token (len) = -2
      call addstr (token (2), sbuf, sbp, 2048)
      goto 23016
23015 continue
      call addstr (token, sbuf, sbp, 2048)
23016 continue
      call addchr (-2, sbuf, sbp, 2048)
      t = lex (token)
      call pbstr (token)
      if (.not.(t .ne. -75))goto 23017
      i = 1
23019 if (.not.(i .lt. sbp))goto 23021
      call outtab
      call outstr (dat)
      k = 1
      j = i + length (sbuf (i)) + 1
23022 continue
      if (.not.(k .gt. 1))goto 23025
      call outch (44)
23025 continue
      call outstr (sbuf (i))
      call outch (40)
      call outnum (k)
      call outch (41)
      call outch (47)
      if (.not.(sbuf (j) .eq. -2))goto 23027
      goto 23024
23027 continue
      n = sbuf (j)
      call outnum (n)
      call outch (47)
      k = k + 1
23023 j = j + 1
      goto 23022
23024 continue
      call pbstr (eoss)
23029 continue
      t = gnbtok (token, 100)
      call outstr (token)
23030 if (.not.(t .eq. 47))goto 23029
23031 continue
      call outdon
23020 i = j + 1
      goto 23019
23021 continue
      sbp = 1
23017 continue
      return
      end
c     logic0  logical_column
