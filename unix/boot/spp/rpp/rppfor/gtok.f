      integer function gtok (lexstr, toksiz)
      integer lexstr (100)
      integer toksiz
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
      integer c
      integer ngetch
      integer i
      c = ngetch (lexstr (1))
      if (.not.(c .eq. 32 .or. c .eq. 9))goto 23000
      lexstr (1) = 32
23002 if (.not.(c .eq. 32 .or. c .eq. 9))goto 23003
      c = ngetch (c)
      goto 23002
23003 continue
      if (.not.(c .eq. 35))goto 23004
23006 if (.not.(ngetch (c) .ne. 10))goto 23007
      goto 23006
23007 continue
23004 continue
      if (.not.(c .ne. 10))goto 23008
      call putbak (c)
      goto 23009
23008 continue
      lexstr (1) = 10
23009 continue
      lexstr (2) = -2
      gtok = lexstr (1)
      return
23000 continue
      i = 1
      if (.not.(((65.le.c.and.c.le.90).or.(97.le.c.and.c.le.122))))goto 
     *23010
      gtok = -9
      if (.not.(c .eq. 120))goto 23012
      c = ngetch (lexstr(2))
      if (.not.(c .eq. 36))goto 23014
      gtok =  -166
      i = 2
      goto 23015
23014 continue
      call putbak (c)
23015 continue
23012 continue
23016 if (.not.(i .lt. toksiz - 2))goto 23018
      c = ngetch (lexstr(i+1))
      if (.not.(.not.((65.le.c.and.c.le.90).or.(97.le.c.and.c.le.122)) .
     *and. .not.(48.le.c.and.c.le.57) .and. c .ne. 95))goto 23019
      goto 23018
23019 continue
23017 i=i+1
      goto 23016
23018 continue
      call putbak (c)
      goto 23011
23010 continue
      if (.not.((48.le.c.and.c.le.57)))goto 23021
      i=1
23023 if (.not.(i .lt. toksiz - 2))goto 23025
      c = ngetch (lexstr (i + 1))
      if (.not.(.not.(48.le.c.and.c.le.57)))goto 23026
      goto 23025
23026 continue
23024 i=i+1
      goto 23023
23025 continue
      call putbak (c)
      gtok = 48
      goto 23022
23021 continue
      if (.not.(c .eq. 91))goto 23028
      lexstr (1) = 123
      gtok = 123
      goto 23029
23028 continue
      if (.not.(c .eq. 93))goto 23030
      lexstr (1) = 125
      gtok = 125
      goto 23031
23030 continue
      if (.not.(c .eq. 36))goto 23032
      if (.not.(ngetch (lexstr (2)) .eq. 40))goto 23034
      i = 2
      gtok = -69
      goto 23035
23034 continue
      if (.not.(lexstr (2) .eq. 41))goto 23036
      i = 2
      gtok = -68
      goto 23037
23036 continue
      call putbak (lexstr (2))
      gtok = 36
23037 continue
23035 continue
      goto 23033
23032 continue
      if (.not.(c .eq. 39 .or. c .eq. 34))goto 23038
      gtok = c
      i = 2
23040 if (.not.(ngetch (lexstr (i)) .ne. lexstr (1)))goto 23042
      if (.not.(lexstr (i) .eq. 95))goto 23043
      if (.not.(ngetch (c) .eq. 10))goto 23045
23047 if (.not.(c .eq. 10 .or. c .eq. 32 .or. c .eq. 9))goto 23048
      c = ngetch (c)
      goto 23047
23048 continue
      lexstr (i) = c
      goto 23046
23045 continue
      call putbak (c)
23046 continue
23043 continue
      if (.not.(lexstr (i) .eq. 10 .or. i .ge. toksiz - 1))goto 23049
      call synerr (14Hmissing quote.)
      lexstr (i) = lexstr (1)
      call putbak (10)
      goto 23042
23049 continue
23041 i = i + 1
      goto 23040
23042 continue
      goto 23039
23038 continue
      if (.not.(c .eq. 35))goto 23051
23053 if (.not.(ngetch (lexstr (1)) .ne. 10))goto 23054
      goto 23053
23054 continue
      gtok = 10
      goto 23052
23051 continue
      if (.not.(c .eq. 62 .or. c .eq. 60 .or. c .eq. 126 .or. c .eq. 33 
     *.or. c .eq. 126 .or. c .eq. 94 .or. c .eq. 61 .or. c .eq. 38 .or. 
     *c .eq. 124))goto 23055
      call relate (lexstr, i)
      gtok = c
      goto 23056
23055 continue
      gtok = c
23056 continue
23052 continue
23039 continue
23033 continue
23031 continue
23029 continue
23022 continue
23011 continue
      if (.not.(i .ge. toksiz - 1))goto 23057
      call synerr (15Htoken too long.)
23057 continue
      lexstr (i + 1) = -2
      return
      end
c     logic0  logical_column
