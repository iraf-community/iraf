      subroutine forcod (lab)
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
      integer t, token (100)
      integer gettok, gnbtok
      integer i, j, nlpar
      integer length, labgen
      integer ifnot(10)
      integer serrc0(22)
      data ifnot(1)/105/,ifnot(2)/102/,ifnot(3)/32/,ifnot(4)/40/,ifnot(5
     *)/46/,ifnot(6)/110/,ifnot(7)/111/,ifnot(8)/116/,ifnot(9)/46/,ifnot
     *(10)/-2/
      data serrc0(1)/46/,serrc0(2)/97/,serrc0(3)/110/,serrc0(4)/100/,ser
     *rc0(5)/46/,serrc0(6)/40/,serrc0(7)/46/,serrc0(8)/110/,serrc0(9)/11
     *1/,serrc0(10)/116/,serrc0(11)/46/,serrc0(12)/120/,serrc0(13)/101/,
     *serrc0(14)/114/,serrc0(15)/102/,serrc0(16)/108/,serrc0(17)/103/,se
     *rrc0(18)/41/,serrc0(19)/41/,serrc0(20)/41/,serrc0(21)/32/,serrc0(2
     *2)/-2/
      lab = labgen (3)
      call outcon (0)
      if (.not.(gnbtok (token, 100) .ne. 40))goto 23000
      call synerr (19Hmissing left paren.)
      return
23000 continue
      if (.not.(gnbtok (token, 100) .ne. 59))goto 23002
      call pbstr (token)
      call outtab
      call eatup
      call outdwe
23002 continue
      if (.not.(gnbtok (token, 100) .eq. 59))goto 23004
      call outcon (lab)
      goto 23005
23004 continue
      call pbstr (token)
      call outnum (lab)
      call outtab
      call outstr (ifnot)
      call outch (40)
      nlpar = 0
23006 if (.not.(nlpar .ge. 0))goto 23007
      t = gettok (token, 100)
      if (.not.(t .eq. 59))goto 23008
      goto 23007
23008 continue
      if (.not.(t .eq. 40))goto 23010
      nlpar = nlpar + 1
      goto 23011
23010 continue
      if (.not.(t .eq. 41))goto 23012
      nlpar = nlpar - 1
23012 continue
23011 continue
      if (.not.(t .eq. -1))goto 23014
      call pbstr (token)
      return
23014 continue
      if (.not.(t .eq. -9))goto 23016
      call squash (token)
23016 continue
      if (.not.(t .ne. 10 .and. t .ne. 95))goto 23018
      call outstr (token)
23018 continue
      goto 23006
23007 continue
      if (.not.(ername .eq. 1))goto 23020
      call outstr (serrc0)
      goto 23021
23020 continue
      call outch (41)
      call outch (41)
      call outch (32)
23021 continue
      call outgo (lab+2)
      if (.not.(nlpar .lt. 0))goto 23022
      call synerr (19Hinvalid for clause.)
23022 continue
23005 continue
      fordep = fordep + 1
      j = 1
      i = 1
23024 if (.not.(i .lt. fordep))goto 23026
      j = j + length (forstk (j)) + 1
23025 i = i + 1
      goto 23024
23026 continue
      forstk (j) = -2
      nlpar = 0
      t = gnbtok (token, 100)
      call pbstr (token)
23027 if (.not.(nlpar .ge. 0))goto 23028
      t = gettok (token, 100)
      if (.not.(t .eq. 40))goto 23029
      nlpar = nlpar + 1
      goto 23030
23029 continue
      if (.not.(t .eq. 41))goto 23031
      nlpar = nlpar - 1
23031 continue
23030 continue
      if (.not.(t .eq. -1))goto 23033
      call pbstr (token)
      goto 23028
23033 continue
      if (.not.(nlpar .ge. 0 .and. t .ne. 10 .and. t .ne. 95))goto 23035
      if (.not.(t .eq. -9))goto 23037
      call squash (token)
23037 continue
      if (.not.(j + length (token) .ge. 200))goto 23039
      call baderr (20Hfor clause too long.)
23039 continue
      call scopy (token, 1, forstk, j)
      j = j + length (token)
23035 continue
      goto 23027
23028 continue
      lab = lab + 1
      call indent (1)
      call errgo
      return
      end
c     logic0  logical_column
c     serrc0  serrchk
