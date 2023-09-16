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
      integer spoin0(9)
      integer p1(16)
      integer p2(18)
      integer p3(18)
C      integer p4(18)
C      integer p5(18)
C      integer p6(25)
      integer p4(16)
      integer p5(16)
      integer p6(13)
      integer p7(25)
      integer p8(16)
      integer p9(61)
      integer pa(18)

C      data spoin0(1)/105/,spoin0(2)/110/,spoin0(3)/116/,spoin0(4)/101/,s
C     *poin0(5)/103/,spoin0(6)/101/,spoin0(7)/114/,spoin0(8)/42/,spoin0(9
C     *)/56/,spoin0(10)/32/,spoin0(11)/-2/
      data spoin0(1)/105/,spoin0(2)/110/,spoin0(3)/116/,spoin0(4)/101/,s
     *poin0(5)/103/,spoin0(6)/101/,spoin0(7)/114/,spoin0(8)/32/,spoin0(9
     *)/-2/

      data p1(1)/108/,p1(2)/111/,p1(3)/103/,p1(4)/105/,p1(5)/99/,p1(6)/9
     *7/,p1(7)/108/,p1(8)/32/,p1(9)/77/,p1(10)/101/,p1(11)/109/,p1(12)/9
     *8/,p1(13)/40/,p1(14)/49/,p1(15)/41/,p1(16)/-2/
      data p2(1)/105/,p2(2)/110/,p2(3)/116/,p2(4)/101/,p2(5)/103/,p2(6)/
     *101/,p2(7)/114/,p2(8)/42/,p2(9)/50/,p2(10)/32/,p2(11)/77/,p2(12)/1
     *01/,p2(13)/109/,p2(14)/99/,p2(15)/40/,p2(16)/49/,p2(17)/41/,p2(18)
     */-2/
      data p3(1)/105/,p3(2)/110/,p3(3)/116/,p3(4)/101/,p3(5)/103/,p3(6)/
     *101/,p3(7)/114/,p3(8)/42/,p3(9)/50/,p3(10)/32/,p3(11)/77/,p3(12)/1
     *01/,p3(13)/109/,p3(14)/115/,p3(15)/40/,p3(16)/49/,p3(17)/41/,p3(18
     *)/-2/

      data p4(1)/105/,p4(2)/110/,p4(3)/116/,p4(4)/101/,p4(5)/103/,p4(6)/
     *101/,p4(7)/114/,p4(8)/32/,p4(9)/77/,p4(10)/101/,p4(11)/109/,p4(12)
     */105/,p4(13)/40/,p4(14)/49/,p4(15)/41/,p4(16)/-2/
      data p5(1)/105/,p5(2)/110/,p5(3)/116/,p5(4)/101/,p5(5)/103/,p5(6)/
     *101/,p5(7)/114/,p5(8)/32/,p5(9)/77/,p5(10)/101/,p5(11)/109/,p5(12)
     */108/,p5(13)/40/,p5(14)/49/,p5(15)/41/,p5(16)/-2/

C      data p4(1)/105/,p4(2)/110/,p4(3)/116/,p4(4)/101/,p4(5)/103/,p4(6)/
C     *101/,p4(7)/114/,p4(8)/42/,p4(9)/56/,p4(10)/32/,p4(11)/77/,p4(12)/1
C     *01/,p4(13)/109/,p4(14)/105/,p4(15)/40/,p4(16)/49/,p4(17)/41/,p4(18
C     *)/-2/
C      data p5(1)/105/,p5(2)/110/,p5(3)/116/,p5(4)/101/,p5(5)/103/,p5(6)/
C     *101/,p5(7)/114/,p5(8)/42/,p5(9)/56/,p5(10)/32/,p5(11)/77/,p5(12)/1
C     *01/,p5(13)/109/,p5(14)/108/,p5(15)/40/,p5(16)/49/,p5(17)/41/,p5(18
C     *)/-2/
C      data p6(1)/100/,p6(2)/111/,p6(3)/117/,p6(4)/98/,p6(5)/108/,p6(6)/1
C     *01/,p6(7)/32/,p6(8)/112/,p6(9)/114/,p6(10)/101/,p6(11)/99/,p6(12)/
C     *105/,p6(13)/115/,p6(14)/105/,p6(15)/111/,p6(16)/110/,p6(17)/32/,p6
C     *(18)/77/,p6(19)/101/,p6(20)/109/,p6(21)/114/,p6(22)/40/,p6(23)/49/
C     *,p6(24)/41/,p6(25)/-2/

      data p6(1)/114/,p6(2)/101/,p6(3)/97/,p6(4)/108/,p6(5)/32/,p6(6)/77
     */,p6(7)/101/,p6(8)/109/,p6(9)/114/,p6(10)/40/,p6(11)/49/,p6(12)/41
     */,p6(13)/-2/

      data p7(1)/100/,p7(2)/111/,p7(3)/117/,p7(4)/98/,p7(5)/108/,p7(6)/1
     *01/,p7(7)/32/,p7(8)/112/,p7(9)/114/,p7(10)/101/,p7(11)/99/,p7(12)/
     *105/,p7(13)/115/,p7(14)/105/,p7(15)/111/,p7(16)/110/,p7(17)/32/,p7
     *(18)/77/,p7(19)/101/,p7(20)/109/,p7(21)/100/,p7(22)/40/,p7(23)/49/
     *,p7(24)/41/,p7(25)/-2/
      data p8(1)/99/,p8(2)/111/,p8(3)/109/,p8(4)/112/,p8(5)/108/,p8(6)/1
     *01/,p8(7)/120/,p8(8)/32/,p8(9)/77/,p8(10)/101/,p8(11)/109/,p8(12)/
     *120/,p8(13)/40/,p8(14)/49/,p8(15)/41/,p8(16)/-2/
      data p9(1)/101/,p9(2)/113/,p9(3)/117/,p9(4)/105/,p9(5)/118/,p9(6)/
     *97/,p9(7)/108/,p9(8)/101/,p9(9)/110/,p9(10)/99/,p9(11)/101/,p9(12)
     */32/,p9(13)/40/,p9(14)/77/,p9(15)/101/,p9(16)/109/,p9(17)/98/,p9(1
     *8)/44/,p9(19)/32/,p9(20)/77/,p9(21)/101/,p9(22)/109/,p9(23)/99/,p9
     *(24)/44/,p9(25)/32/,p9(26)/77/,p9(27)/101/,p9(28)/109/,p9(29)/115/
     *,p9(30)/44/,p9(31)/32/,p9(32)/77/,p9(33)/101/,p9(34)/109/,p9(35)/1
     *05/,p9(36)/44/,p9(37)/32/,p9(38)/77/,p9(39)/101/,p9(40)/109/,p9(41
     *)/108/,p9(42)/44/,p9(43)/32/,p9(44)/77/,p9(45)/101/,p9(46)/109/,p9
     *(47)/114/,p9(48)/44/,p9(49)/32/,p9(50)/77/,p9(51)/101/,p9(52)/109/
     *,p9(53)/100/,p9(54)/44/,p9(55)/32/,p9(56)/77/,p9(57)/101/,p9(58)/1
     *09/,p9(59)/120/,p9(60)/41/,p9(61)/-2/
      data pa(1)/99/,pa(2)/111/,pa(3)/109/,pa(4)/109/,pa(5)/111/,pa(6)/1
     *10/,pa(7)/32/,pa(8)/47/,pa(9)/77/,pa(10)/101/,pa(11)/109/,pa(12)/4
     *7/,pa(13)/32/,pa(14)/77/,pa(15)/101/,pa(16)/109/,pa(17)/100/,pa(18
     *)/-2/
      if (.not.(memflg .eq. 0))goto 23000
      call poidec (p1)
      call poidec (p2)
      call poidec (p3)
      call poidec (p4)
      call poidec (p5)
      call poidec (p6)
      call poidec (p7)
      call poidec (p8)
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
