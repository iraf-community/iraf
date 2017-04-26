      subroutine parse
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
      integer lexstr(100)
      integer lab, labval(100), lextyp(100), sp, token, i, t
      integer lex
      logical pushs0
      sp = 1
      lextyp(1) = -1
      token = lex(lexstr)
23000 if (.not.(token .ne. -1))goto 23002
      pushs0 = .false.
      I23003=(token)
      goto 23003
23005 continue
      call ifcode (lab)
      pushs0 = .true.
      goto 23004
23006 continue
      call iferrc (lab, 1)
      pushs0 = .true.
      goto 23004
23007 continue
      call iferrc (lab, 0)
      pushs0 = .true.
      goto 23004
23008 continue
      call docode (lab)
      pushs0 = .true.
      goto 23004
23009 continue
      call whilec (lab)
      pushs0 = .true.
      goto 23004
23010 continue
      call forcod (lab)
      pushs0 = .true.
      goto 23004
23011 continue
      call repcod (lab)
      pushs0 = .true.
      goto 23004
23012 continue
      call swcode (lab)
      pushs0 = .true.
      goto 23004
23013 continue
      i=sp
23014 if (.not.(i .gt. 0))goto 23016
      if (.not.(lextyp(i) .eq. -92))goto 23017
      goto 23016
23017 continue
23015 i=i-1
      goto 23014
23016 continue
      if (.not.(i .eq. 0))goto 23019
      call synerr (24Hillegal case or default.)
      goto 23020
23019 continue
      call cascod (labval (i), token)
23020 continue
      goto 23004
23021 continue
      call labelc (lexstr)
      pushs0 = .true.
      goto 23004
23022 continue
      t = lextyp(sp)
      if (.not.(t .eq. -99 .or. t .eq. -98 .or. t .eq. -97))goto 23023
      call elseif (labval(sp))
      goto 23024
23023 continue
      call synerr (13HIllegal else.)
23024 continue
      t = lex (lexstr)
      call pbstr (lexstr)
      if (.not.(t .eq. -99 .or. t .eq. -98 .or. t .eq. -97))goto 23025
      call indent (-1)
      token = -72
23025 continue
      pushs0 = .true.
      goto 23004
23027 continue
      if (.not.(lextyp(sp) .eq. -98 .or. lextyp(sp) .eq. -97))goto 23028
      call thenco (lextyp(sp), labval(sp))
      lab = labval(sp)
      token = lextyp(sp)
      sp = sp - 1
      goto 23029
23028 continue
      call synerr (41HIllegal 'then' clause in iferr statement.)
23029 continue
      pushs0 = .true.
      goto 23004
23030 continue
      call litral
      goto 23004
23031 continue
      call errchk
      goto 23004
23032 continue
      call beginc
      goto 23004
23033 continue
      call endcod (lexstr)
      if (.not.(sp .ne. 1))goto 23034
      call synerr (31HMissing right brace or 'begin'.)
      sp = 1
23034 continue
      goto 23004
23036 continue
      if (.not.(token .eq. 123))goto 23037
      pushs0 = .true.
      goto 23038
23037 continue
      if (.not.(token .eq. -67))goto 23039
      call declco (lexstr)
23039 continue
23038 continue
      goto 23004
23003 continue
      I23003=I23003+100
      if (I23003.lt.1.or.I23003.gt.18)goto 23036
      goto (23005,23006,23007,23008,23009,23010,23011,23012,23013,23013,
     *23021,23036,23022,23027,23030,23031,23032,23033),I23003
23004 continue
      if (.not.(pushs0))goto 23041
      if (.not.(body .eq. 0))goto 23043
      call synerr (24HMissing 'begin' keyword.)
      call beginc
23043 continue
      sp = sp + 1
      if (.not.(sp .gt. 100))goto 23045
      call baderr (25HStack overflow in parser.)
23045 continue
      lextyp(sp) = token
      labval(sp) = lab
      goto 23042
23041 continue
      if (.not.(token .ne. -91 .and. token .ne. -90))goto 23047
      if (.not.(token .eq. 125))goto 23049
      token = -74
23049 continue
      I23051=(token)
      goto 23051
23053 continue
      call otherc (lexstr)
      goto 23052
23054 continue
      call brknxt (sp, lextyp, labval, token)
      goto 23052
23055 continue
      call retcod
      goto 23052
23056 continue
      call gocode
      goto 23052
23057 continue
      if (.not.(body .eq. 0))goto 23058
      call strdcl
      goto 23059
23058 continue
      call otherc (lexstr)
23059 continue
      goto 23052
23060 continue
      if (.not.(lextyp(sp) .eq. 123))goto 23061
      sp = sp - 1
      goto 23062
23061 continue
      if (.not.(lextyp(sp) .eq. -92))goto 23063
      call swend (labval(sp))
      sp = sp - 1
      goto 23064
23063 continue
      call synerr (20HIllegal right brace.)
23064 continue
23062 continue
      goto 23052
23051 continue
      I23051=I23051+81
      if (I23051.lt.1.or.I23051.gt.7)goto 23052
      goto (23053,23054,23054,23055,23056,23057,23060),I23051
23052 continue
      token = lex (lexstr)
      call pbstr (lexstr)
      call unstak (sp, lextyp, labval, token)
23047 continue
23042 continue
23001 token = lex(lexstr)
      goto 23000
23002 continue
      if (.not.(sp .ne. 1))goto 23065
      call synerr (15Hunexpected EOF.)
23065 continue
      end
c     pushs0  push_stack
c     logic0  logical_column
