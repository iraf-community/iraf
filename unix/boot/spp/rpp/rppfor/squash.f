      subroutine squash (id)
      integer id(100)
      integer junk, i, j
      integer lookup, ludef
      integer newid(100), recdid(100)
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
      if (.not.(body .eq. 1 .and. errtbl .ne. 0 .and. ername .eq. 0))got
     *o 23000
      if (.not.(lookup (id, junk, errtbl) .eq. 1))goto 23002
      ername = 1
23002 continue
23000 continue
      j = 1
      i=1
23004 if (.not.(id(i) .ne. -2))goto 23006
      if (.not.(((65.le.id(i).and.id(i).le.90).or.(97.le.id(i).and.id(i)
     *.le.122)) .or. (48.le.id(i).and.id(i).le.57)))goto 23007
      newid(j) = id(i)
      j = j + 1
23007 continue
23005 i=i+1
      goto 23004
23006 continue
      newid(j) = -2
      if (.not.(i-1 .lt. 6 .and. i .eq. j))goto 23009
      return
23009 continue
      if (.not.(lookup (id, junk, fkwtbl) .eq. 1))goto 23011
      return
23011 continue
      if (.not.(ludef (id, recdid, namtbl) .eq. 1))goto 23013
      call scopy (recdid, 1, id, 1)
      return
23013 continue
      call mapid (newid)
      if (.not.(lookup (newid, junk, gentbl) .eq. 1))goto 23015
      call synerr (39HWarning: identifier mapping not unique.)
      call uniqid (newid)
23015 continue
      call entdef (newid, id, gentbl)
      call entdef (id, newid, namtbl)
      call scopy (newid, 1, id, 1)
      end
c     logic0  logical_column
