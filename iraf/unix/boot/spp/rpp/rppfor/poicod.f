      subroutine poicod (decla0)
      integer decla0
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
      common/spptyp/ dbool(64), dchar(64), dshort(64), dint(64), 
     *               dlong(64), dreal(64), ddble(64),  dcplx(64),
     *               dpntr(64), dsizet(64)
      integer dbool
      integer dchar
      integer dshort
      integer dint
      integer dlong
      integer dreal
      integer ddble
      integer dcplx
      integer dpntr
      integer dsizet
      integer space(2)
      integer spoin0(128)
c     " Memp(1)"
      integer pp1(9)
      integer pb1(9)
      integer pc1(9)
      integer ps1(9)
      integer pi1(9)
      integer pl1(9)
      integer pr1(9)
      integer pd1(9)
      integer px1(9)
      integer pz1(9)
c     "logical Memb(1)"
      integer p0(128)
      integer p1(128)
      integer p2(128)
      integer p3(128)
      integer p4(128)
      integer p5(128)
      integer p6(128)
      integer p7(128)
      integer p8(128)
      integer pz(128)
      integer p9(64)
      integer pa(18)
      data space(1)/32/,space(2)/-2/
      data pp1(1)/32/,pp1(2)/77/,pp1(3)/101/,pp1(4)/109/,pp1(5)/112/,
     *     pp1(6)/40/,pp1(7)/49/,pp1(8)/41/,pp1(9)/-2/
      data pb1(1)/32/,pb1(2)/77/,pb1(3)/101/,pb1(4)/109/,pb1(5)/98/,
     *     pb1(6)/40/,pb1(7)/49/,pb1(8)/41/,pb1(9)/-2/
      data pc1(1)/32/,pc1(2)/77/,pc1(3)/101/,pc1(4)/109/,pc1(5)/99/,
     *     pc1(6)/40/,pc1(7)/49/,pc1(8)/41/,pc1(9)/-2/
      data ps1(1)/32/,ps1(2)/77/,ps1(3)/101/,ps1(4)/109/,ps1(5)/115/,
     *     ps1(6)/40/,ps1(7)/49/,ps1(8)/41/,ps1(9)/-2/
      data pi1(1)/32/,pi1(2)/77/,pi1(3)/101/,pi1(4)/109/,pi1(5)/105/,
     *     pi1(6)/40/,pi1(7)/49/,pi1(8)/41/,pi1(9)/-2/
      data pl1(1)/32/,pl1(2)/77/,pl1(3)/101/,pl1(4)/109/,pl1(5)/108/,
     *     pl1(6)/40/,pl1(7)/49/,pl1(8)/41/,pl1(9)/-2/
      data pr1(1)/32/,pr1(2)/77/,pr1(3)/101/,pr1(4)/109/,pr1(5)/114/,
     *     pr1(6)/40/,pr1(7)/49/,pr1(8)/41/,pr1(9)/-2/
      data pd1(1)/32/,pd1(2)/77/,pd1(3)/101/,pd1(4)/109/,pd1(5)/100/,
     *     pd1(6)/40/,pd1(7)/49/,pd1(8)/41/,pd1(9)/-2/
      data px1(1)/32/,px1(2)/77/,px1(3)/101/,px1(4)/109/,px1(5)/120/,
     *     px1(6)/40/,px1(7)/49/,px1(8)/41/,px1(9)/-2/
      data pz1(1)/32/,pz1(2)/77/,pz1(3)/101/,pz1(4)/109/,pz1(5)/122/,
     *     pz1(6)/40/,pz1(7)/49/,pz1(8)/41/,pz1(9)/-2/
      data p9(1)/101/,p9(2)/113/,p9(3)/117/,p9(4)/105/,p9(5)/118/,p9(6)/
     *97/,p9(7)/108/,p9(8)/101/,p9(9)/110/,p9(10)/99/,p9(11)/101/,p9(12)
     */32/,p9(13)/40/,p9(14)/77/,p9(15)/101/,p9(16)/109/,p9(17)/112/,p9(
     *18)/44/,p9(19)/77/,p9(20)/101/,p9(21)/109/,p9(22)/98/,p9(23)/44/,p
     *9(24)/77/,p9(25)/101/,p9(26)/109/,p9(27)/99/,p9(28)/44/,p9(29)/77/
     *,p9(30)/101/,p9(31)/109/,p9(32)/115/,p9(33)/44/,p9(34)/77/,p9(35)/
     *101/,p9(36)/109/,p9(37)/105/,p9(38)/44/,p9(39)/77/,p9(40)/101/,p9(
     *41)/109/,p9(42)/108/,p9(43)/44/,p9(44)/77/,p9(45)/101/,p9(46)/109/
     *,p9(47)/114/,p9(48)/44/,p9(49)/77/,p9(50)/101/,p9(51)/109/,p9(52)/
     *100/,p9(53)/44/,p9(54)/77/,p9(55)/101/,p9(56)/109/p9(57)/120/,p9(5
     *8)/44/,p9(59)/77/,p9(60)/101/,p9(61)/109/p9(62)/122/,p9(63)/41/,p9
     *(64)/-2/
      data pa(1)/99/,pa(2)/111/,pa(3)/109/,pa(4)/109/,pa(5)/111/,pa(6)/1
     *10/,pa(7)/32/,pa(8)/47/,pa(9)/77/,pa(10)/101/,pa(11)/109/,pa(12)/4
     *7/,pa(13)/32/,pa(14)/77/,pa(15)/101/,pa(16)/109/,pa(17)/100/,pa(18
     *)/-2/
      call concat (dpntr, space, spoin0)
      call concat (dpntr,  pp1, p0)
      call concat (dbool,  pb1, p1)
      call concat (dchar,  pc1, p2)
      call concat (dshort, ps1, p3)
      call concat (dint,   pi1, p4)
      call concat (dlong,  pl1, p5)
      call concat (dreal,  pr1, p6)
      call concat (ddble,  pd1, p7)
      call concat (dcplx,  px1, p8)
      call concat (dsizet, pz1, pz)
      if (.not.(memflg .eq. 0))goto 23000
      call poidec (p0)
      call poidec (p1)
      call poidec (p2)
      call poidec (p3)
      call poidec (p4)
      call poidec (p5)
      call poidec (p6)
      call poidec (p7)
      call poidec (p8)
      call poidec (pz)
      call poidec (p9)
      call poidec (pa)
      memflg = 1
23000 continue
      if (.not.(decla0 .eq. 1))goto 23002
      call outtab
      call outstr (spoin0)
23002 continue
      end
      subroutine poidec (str)
      integer str
      call outtab
      call outstr (str)
      call outdon
      end
c     logic0  logical_column
c     decla0  declare_variable
c     spoin0  spointer
