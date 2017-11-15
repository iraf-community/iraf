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
      integer p1(19)
      integer p2(21)
      integer p3(21)
      integer p4(19)
      integer p5(19)
      integer p6(16)
      integer p7(28)
      integer p8(19)
      integer p9(61)
      integer pa(18)

      data spoin0(1),spoin0(2),spoin0(3),spoin0(4),spoin0(5),spoin0(6),s
     *poin0(7),spoin0(8),spoin0(9)/105,110,116,101,103,101,114,32,-2/

      data p1(1),p1(2),p1(3),p1(4),p1(5),p1(6),p1(7),p1(8),p1(9),p1(10),
     *p1(11),p1(12),p1(13),p1(14),p1(15),p1(16),p1(17),p1(18),p1(19)/108
     *,111,103,105,99,97,108,32,77,101,109,98,40,49,48,50,52,41,-2/
      data p2(1),p2(2),p2(3),p2(4),p2(5),p2(6),p2(7),p2(8),p2(9),p2(10),
     *p2(11),p2(12),p2(13),p2(14),p2(15),p2(16),p2(17),p2(18),p2(19),p2(
     *20),p2(21)/105,110,116,101,103,101,114,42,50,32,77,101,109,99,40,4
     *9,48,50,52,41,-2/
      data p3(1),p3(2),p3(3),p3(4),p3(5),p3(6),p3(7),p3(8),p3(9),p3(10),
     *p3(11),p3(12),p3(13),p3(14),p3(15),p3(16),p3(17),p3(18),p3(19),p3(
     *20),p3(21)/105,110,116,101,103,101,114,42,50,32,77,101,109,115,40,
     *49,48,50,52,41,-2/
      data p4(1),p4(2),p4(3),p4(4),p4(5),p4(6),p4(7),p4(8),p4(9),p4(10),
     *p4(11),p4(12),p4(13),p4(14),p4(15),p4(16),p4(17),p4(18),p4(19)/105
     *,110,116,101,103,101,114,32,77,101,109,105,40,49,48,50,52,41,-2/
      data p5(1),p5(2),p5(3),p5(4),p5(5),p5(6),p5(7),p5(8),p5(9),p5(10),
     *p5(11),p5(12),p5(13),p5(14),p5(15),p5(16),p5(17),p5(18),p5(19)/105
     *,110,116,101,103,101,114,32,77,101,109,108,40,49,48,50,52,41,-2/
      data p6(1),p6(2),p6(3),p6(4),p6(5),p6(6),p6(7),p6(8),p6(9),p6(10),
     *p6(11),p6(12),p6(13),p6(14),p6(15),p6(16)/114,101,97,108,32,77,101
     *,109,114,40,49,48,50,52,41,-2/
      data p7(1),p7(2),p7(3),p7(4),p7(5),p7(6),p7(7),p7(8),p7(9),p7(10),
     *p7(11),p7(12),p7(13),p7(14),p7(15),p7(16),p7(17),p7(18),p7(19),p7(
     *20),p7(21),p7(22),p7(23),p7(24),p7(25),p7(26),p7(27),p7(28)/100,11
     *1,117,98,108,101,32,112,114,101,99,105,115,105,111,110,32,77,101,1
     *09,100,40,49,48,50,52,41,-2/
      data p8(1),p8(2),p8(3),p8(4),p8(5),p8(6),p8(7),p8(8),p8(9),p8(10),
     *p8(11),p8(12),p8(13),p8(14),p8(15),p8(16),p8(17),p8(18),p8(19)/99,
     *111,109,112,108,101,120,32,77,101,109,120,40,49,48,50,52,41,-2/
      data p9(1),p9(2),p9(3),p9(4),p9(5),p9(6),p9(7),p9(8),p9(9),p9(10),
     *p9(11),p9(12),p9(13),p9(14),p9(15),p9(16),p9(17),p9(18),p9(19),p9(
     *20),p9(21),p9(22),p9(23),p9(24),p9(25),p9(26),p9(27),p9(28),p9(29)
     *,p9(30),p9(31),p9(32),p9(33),p9(34),p9(35),p9(36),p9(37),p9(38),p9
     *(39),p9(40),p9(41),p9(42),p9(43),p9(44),p9(45),p9(46),p9(47),p9(48
     *),p9(49),p9(50),p9(51),p9(52),p9(53),p9(54),p9(55),p9(56),p9(57),p
     *9(58),p9(59),p9(60),p9(61)/101,113,117,105,118,97,108,101,110,99,1
     *01,32,40,77,101,109,98,44,32,77,101,109,99,44,32,77,101,109,115,44
     *,32,77,101,109,105,44,32,77,101,109,108,44,32,77,101,109,114,44,32
     *,77,101,109,100,44,32,77,101,109,120,41,-2/
      data pa(1),pa(2),pa(3),pa(4),pa(5),pa(6),pa(7),pa(8),pa(9),pa(10),
     *pa(11),pa(12),pa(13),pa(14),pa(15),pa(16),pa(17),pa(18)/99,111,109
     *,109,111,110,32,47,77,101,109,47,32,77,101,109,100,-2/

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
