      integer function deftok (token, toksiz)
      integer token (100)
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
      integer t, c, defn (2048), mdefn (2048)
      integer gtok
      integer equal
      integer ap, argstk (100), callst (50), nlb, plev (50), ifl
      integer ludef, push, ifparm
      integer balp(3)
      integer pswrg(22)
      data balp(1)/40/,balp(2)/41/,balp(3)/-2/
      data pswrg(1)/115/,pswrg(2)/119/,pswrg(3)/105/,pswrg(4)/116/,pswrg
     *(5)/99/,pswrg(6)/104/,pswrg(7)/95/,pswrg(8)/110/,pswrg(9)/111/,psw
     *rg(10)/95/,pswrg(11)/114/,pswrg(12)/97/,pswrg(13)/110/,pswrg(14)/1
     *03/,pswrg(15)/101/,pswrg(16)/95/,pswrg(17)/99/,pswrg(18)/104/,pswr
     *g(19)/101/,pswrg(20)/99/,pswrg(21)/107/,pswrg(22)/-2/
      cp = 0
      ap = 1
      ep = 1
      t = gtok (token, toksiz)
23000 if (.not.(t .ne. -1))goto 23002
      if (.not.(t .eq. -9))goto 23003
      if (.not.(ludef (token, defn, deftbl) .eq. 0))goto 23005
      if (.not.(cp .eq. 0))goto 23007
      goto 23002
23007 continue
      call puttok (token)
23008 continue
      goto 23006
23005 continue
      if (.not.(defn (1) .eq. -4))goto 23009
      call getdef (token, toksiz, defn, 2048)
      call entdef (token, defn, deftbl)
      goto 23010
23009 continue
      if (.not.(defn (1) .eq. -15 .or. defn (1) .eq. -16))goto 23011
      c = defn (1)
      call getdef (token, toksiz, defn, 2048)
      ifl = ludef (token, mdefn, deftbl)
      if (.not.((ifl .eq. 1 .and. c .eq. -15) .or. (ifl .eq. 0 .and. c .
     *eq. -16)))goto 23013
      call pbstr (defn)
23013 continue
      goto 23012
23011 continue
      if (.not.(defn(1) .eq. -17 .and. cp .eq. 0))goto 23015
      if (.not.(gtok (defn, 2048) .eq. 32))goto 23017
      if (.not.(gtok (defn, 2048) .eq. -9))goto 23019
      if (.not.(equal (defn, pswrg) .eq. 1))goto 23021
      swinrg = 1
      goto 23022
23021 continue
      goto 10
23022 continue
      goto 23020
23019 continue
10    call pbstr (defn)
      call putbak (32)
      goto 23002
23020 continue
      goto 23018
23017 continue
      call pbstr (defn)
      goto 23002
23018 continue
      goto 23016
23015 continue
      cp = cp + 1
      if (.not.(cp .gt. 50))goto 23023
      call baderr (20Hcall stack overflow.)
23023 continue
      callst (cp) = ap
      ap = push (ep, argstk, ap)
      call puttok (defn)
      call putchr (-2)
      ap = push (ep, argstk, ap)
      call puttok (token)
      call putchr (-2)
      ap = push (ep, argstk, ap)
      t = gtok (token, toksiz)
      if (.not.(t .eq. 32))goto 23025
      t = gtok (token, toksiz)
      call pbstr (token)
      if (.not.(t .ne. 40))goto 23027
      call putbak (32)
23027 continue
      goto 23026
23025 continue
      call pbstr (token)
23026 continue
      if (.not.(t .ne. 40))goto 23029
      call pbstr (balp)
      goto 23030
23029 continue
      if (.not.(ifparm (defn) .eq. 0))goto 23031
      call pbstr (balp)
23031 continue
23030 continue
      plev (cp) = 0
23016 continue
23012 continue
23010 continue
23006 continue
      goto 23004
23003 continue
      if (.not.(t .eq. -69))goto 23033
      nlb = 1
23035 continue
      t = gtok (token, toksiz)
      if (.not.(t .eq. -69))goto 23038
      nlb = nlb + 1
      goto 23039
23038 continue
      if (.not.(t .eq. -68))goto 23040
      nlb = nlb - 1
      if (.not.(nlb .eq. 0))goto 23042
      goto 23037
23042 continue
      goto 23041
23040 continue
      if (.not.(t .eq. -1))goto 23044
      call baderr (14HEOF in string.)
23044 continue
23041 continue
23039 continue
      call puttok (token)
23036 goto 23035
23037 continue
      goto 23034
23033 continue
      if (.not.(cp .eq. 0))goto 23046
      goto 23002
23046 continue
      if (.not.(t .eq. 40))goto 23048
      if (.not.(plev (cp) .gt. 0))goto 23050
      call puttok (token)
23050 continue
      plev (cp) = plev (cp) + 1
      goto 23049
23048 continue
      if (.not.(t .eq. 41))goto 23052
      plev (cp) = plev (cp) - 1
      if (.not.(plev (cp) .gt. 0))goto 23054
      call puttok (token)
      goto 23055
23054 continue
      call putchr (-2)
      call evalr (argstk, callst (cp), ap - 1)
      ap = callst (cp)
      ep = argstk (ap)
      cp = cp - 1
23055 continue
      goto 23053
23052 continue
      if (.not.(t .eq. 44 .and. plev (cp) .eq. 1))goto 23056
      call putchr (-2)
      ap = push (ep, argstk, ap)
      goto 23057
23056 continue
      call puttok (token)
23057 continue
23053 continue
23049 continue
23047 continue
23034 continue
23004 continue
23001 t = gtok (token, toksiz)
      goto 23000
23002 continue
      deftok = t
      if (.not.(t .eq. -9))goto 23058
      call fold (token)
23058 continue
      return
      end
c     logic0  logical_column
