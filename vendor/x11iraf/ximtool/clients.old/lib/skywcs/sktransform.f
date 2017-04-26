      subroutine skultn (cooin, cooout, ilng, ilat, olng, olat, npts)
      logical Memb(1)
      integer*2 Memc(1)
      integer*2 Mems(1)
      integer Memi(1)
      integer*4 Meml(1)
      real Memr(1)
      double precision Memd(1)
      complex Memx(1)
      equivalence (Memb, Memc, Mems, Memi, Meml, Memr, Memd, Memx)
      common /Mem/ Memd
      integer cooin
      integer cooout
      integer npts
      double precision ilng(*)
      double precision ilat(*)
      double precision olng(*)
      double precision olat(*)
      double precision tilng
      double precision tilat
      double precision tolng
      double precision tolat
      integer i
      integer sw0001,sw0002,sw0003,sw0004
      save
         do 110 i = 1, npts 
            sw0001=(memi(cooin+22) )
            goto 120
130         continue
               tilng = ((15.0d0 * ilng(i))/57.295779513082320877)
            goto 121
140         continue
               tilng = ((ilng(i))/57.295779513082320877)
            goto 121
150         continue
               tilng = ilng(i)
            goto 121
160         continue
               tilng = ilng(i)
               goto 121
120         continue
               if (sw0001.lt.1.or.sw0001.gt.3) goto 160
               goto (140,150,130),sw0001
121         continue
            sw0002=(memi(cooin+23) )
            goto 170
180         continue
               tilat = ((15.0d0 * ilat(i))/57.295779513082320877)
            goto 171
190         continue
               tilat = ((ilat(i))/57.295779513082320877)
            goto 171
200         continue
               tilat = ilat(i)
            goto 171
210         continue
               tilat = ilat(i)
               goto 171
170         continue
               if (sw0002.lt.1.or.sw0002.gt.3) goto 210
               goto (190,200,180),sw0002
171         continue
            call sklltn (cooin, cooout, tilng, tilat, 1.6d308, 1.6d308, 
     *      0.0d0, 0.0d0, tolng, tolat)
            sw0003=(memi(cooout+22) )
            goto 220
230         continue
               olng(i) = ((tolng)*57.295779513082320877) / 15.0d0
            goto 221
240         continue
               olng(i) = ((tolng)*57.295779513082320877)
            goto 221
250         continue
               olng(i) = tolng
            goto 221
260         continue
               olng(i) = tolng
               goto 221
220         continue
               if (sw0003.lt.1.or.sw0003.gt.3) goto 260
               goto (240,250,230),sw0003
221         continue
            sw0004=(memi(cooout+23) )
            goto 270
280         continue
               olat(i) = ((tolat)*57.295779513082320877) / 15.0d0
            goto 271
290         continue
               olat(i) = ((tolat)*57.295779513082320877)
            goto 271
300         continue
               olat(i) = tolat
            goto 271
310         continue
               olat(i) = tolat
               goto 271
270         continue
               if (sw0004.lt.1.or.sw0004.gt.3) goto 310
               goto (290,300,280),sw0004
271         continue
110      continue
111      continue
100      return
      end
      subroutine sklltn (cooin, cooout, ilng, ilat, ipmlng, ipmlat, px, 
     *rv, olng, olat)
      logical Memb(1)
      integer*2 Memc(1)
      integer*2 Mems(1)
      integer Memi(1)
      integer*4 Meml(1)
      real Memr(1)
      double precision Memd(1)
      complex Memx(1)
      equivalence (Memb, Memc, Mems, Memi, Meml, Memr, Memd, Memx)
      common /Mem/ Memd
      integer cooin
      integer cooout
      double precision ilng
      double precision ilat
      double precision ipmlng
      double precision ipmlat
      double precision px
      double precision rv
      double precision olng
      double precision olat
      integer pmflag
      double precision pmr
      double precision pmd
      double precision slepj
      double precision slepb
      integer sw0001,sw0002,sw0003,sw0004,sw0005,sw0006,sw0007,sw0008,
     *sw0009,sw0010
      save
         if (.not.(memi(cooin+12) .eq. memi(cooout+12) )) goto 110
            sw0001=(memi(cooin+12) )
            goto 120
130         continue
               call skequl (cooin, cooout, ilng, ilat, ipmlng, ipmlat, 
     *         px, rv, olng, olat)
            goto 121
140         continue
               if (.not.(memd((((cooin+10)-1)/2+1)) .eq. memd((((cooout+
     *         10)-1)/2+1)) )) goto 150
                  olng = ilng
                  olat = ilat
                  goto 151
150            continue
                  call sleceq (ilng, ilat, memd((((cooin+10)-1)/2+1)) , 
     *            olng, olat)
                  call sleqec (olng, olat, memd((((cooout+10)-1)/2+1)) ,
     *             olng, olat)
151            continue
            goto 121
160         continue
               olng = ilng
               olat = ilat
               goto 121
120         continue
               if (sw0001.eq.1) goto 130
               if (sw0001.eq.2) goto 140
               goto 160
121         continue
            goto 100
110      continue
         if (.not.(.not. ((ipmlng).eq.1.6d308) .and. .not. ((ipmlat).eq.
     *   1.6d308))) goto 170
            pmflag = 1
            goto 171
170      continue
            pmflag = 0
171      continue
         sw0002=(memi(cooin+12) )
         goto 180
190      continue
            sw0003=(memi(cooin+13) )
            goto 200
210         continue
               if (.not.(pmflag .eq. 1)) goto 220
                  call slpm (ilng, ilat, ipmlng, ipmlat, px, rv, slepb (
     *            memd((((cooin+10)-1)/2+1)) ), slepb (memd((((cooout+10
     *            )-1)/2+1)) ), olng, olat)
                  goto 221
220            continue
                  olng = ilng
                  olat = ilat
221            continue
               if (.not.(memi(cooin+13) .eq. 1)) goto 230
                  call slsuet (olng, olat, memd((((cooin+8)-1)/2+1)) , 
     *            olng, olat)
230            continue
               if (.not.(memd((((cooin+8)-1)/2+1)) .ne. 1950.0d0)) goto 
     *         240
                  call slprcs (1, memd((((cooin+8)-1)/2+1)) , 1950.0d0, 
     *            olng, olat)
240            continue
               call sladet (olng, olat, 1950.0d0, olng, olat)
               if (.not.(pmflag .eq. 1)) goto 250
                  call slf45z (olng, olat, slepb(memd((((cooout+10)-1)/2
     *            +1)) ), olng, olat)
                  goto 251
250            continue
                  call slf45z (olng, olat, slepb (memd((((cooin+10)-1)/2
     *            +1)) ), olng, olat)
251            continue
            goto 201
260         continue
               if (.not.(pmflag .eq. 1)) goto 270
                  call slpm (ilng, ilat, ipmlng, ipmlat, px, rv, slepj (
     *            memd((((cooin+10)-1)/2+1)) ), slepj(memd((((cooout+10)
     *            -1)/2+1)) ), olng, olat)
                  goto 271
270            continue
                  olng = ilng
                  olat = ilat
271            continue
               if (.not.(memd((((cooin+8)-1)/2+1)) .ne. 2000.0d0)) goto 
     *         280
                  call slprcs (2, memd((((cooin+8)-1)/2+1)) , 2000.0d0, 
     *            olng, olat)
280            continue
            goto 201
290         continue
               if (.not.(pmflag .eq. 1)) goto 300
                  call slpm (ilng, ilat, ipmlng, ipmlat, px, rv, slepj (
     *            memd((((cooin+10)-1)/2+1)) ), slepj(memd((((cooout+10)
     *            -1)/2+1)) ), olng, olat)
                  goto 301
300            continue
                  olng = ilng
                  olat = ilat
301            continue
               if (.not.(memd((((cooin+8)-1)/2+1)) .ne. 2000.0d0)) goto 
     *         310
                  call slprcs (2, memd((((cooin+8)-1)/2+1)) , 2000.0d0, 
     *            olng, olat)
310            continue
               call slhf5z (olng, olat, 2000.0d0, olng, olat, pmr, pmd)
            goto 201
320         continue
               call slamp (ilng, ilat, memd((((cooin+10)-1)/2+1)) , 2000
     *         .0d0, olng, olat)
               goto 201
200         continue
               if (sw0003.lt.1.or.sw0003.gt.5) goto 201
               goto (210,210,260,290,320),sw0003
201         continue
            sw0004=(memi(cooout+12) )
            goto 330
340         continue
               call sleqec (olng, olat, memd((((cooout+10)-1)/2+1)) , 
     *         olng, olat)
            goto 331
350         continue
               call sleqga (olng, olat, olng, olat)
            goto 331
360         continue
               call sleqga (olng, olat, olng, olat)
               call slgasu (olng, olat, olng, olat)
            goto 331
370         continue
               olng = ilng
               olat = ilat
               goto 331
330         continue
               sw0004=sw0004-1
               if (sw0004.lt.1.or.sw0004.gt.3) goto 370
               goto (340,350,360),sw0004
331         continue
         goto 181
380      continue
            call sleceq (ilng, ilat, memd((((cooin+10)-1)/2+1)) , olng, 
     *      olat)
            sw0005=(memi(cooout+12) )
            goto 390
400         continue
               sw0006=(memi(cooout+13) )
               goto 410
420            continue
                  call slf54z (olng, olat, slepb(memd((((cooout+10)-1)/2
     *            +1)) ), olng, olat, pmr, pmd)
                  call slsuet (olng, olat, 1950.0d0, olng, olat)
                  if (.not.(memd((((cooout+8)-1)/2+1)) .ne. 1950.0d0)) 
     *            goto 430
                     call slprcs (1, 1950.0d0, memd((((cooout+8)-1)/2+1)
     *               ) , olng, olat)
430               continue
                  if (.not.(memi(cooout+13) .eq. 1)) goto 440
                     call sladet (olng, olat, memd((((cooout+8)-1)/2+1))
     *                , olng, olat)
440               continue
               goto 411
450            continue
                  if (.not.(memd((((cooout+8)-1)/2+1)) .ne. 2000.0d0)) 
     *            goto 460
                     call slprcs (2, 2000.0d0, memd((((cooout+8)-1)/2+1)
     *               ) , olng, olat)
460               continue
               goto 411
470            continue
                  call slf5hz (olng, olat, 2000.0d0, olng, olat)
                  if (.not.(memd((((cooout+8)-1)/2+1)) .ne. 2000.0d0)) 
     *            goto 480
                     call slprcs (2, 2000.0d0, memd((((cooout+8)-1)/2+1)
     *               ) , olng, olat)
480               continue
               goto 411
490            continue
                  call slmap (olng, olat, 0.0d0, 0.0d0, px, 0.0d0, 2000.
     *            0d0, memd((((cooout+10)-1)/2+1)) , olng, olat)
                  goto 411
410            continue
                  if (sw0006.lt.1.or.sw0006.gt.5) goto 411
                  goto (420,420,450,470,490),sw0006
411            continue
            goto 391
500         continue
               call sleqga (olng, olat, olng, olat)
            goto 391
510         continue
               call sleqga (olng, olat, olng, olat)
               call slgasu (olng, olat, olng, olat)
            goto 391
520         continue
               olng = ilng
               olat = ilat
               goto 391
390         continue
               if (sw0005.lt.1.or.sw0005.gt.4) goto 520
               goto (400,520,500,510),sw0005
391         continue
         goto 181
530      continue
            sw0007=(memi(cooout+12) )
            goto 540
550         continue
               call slgaeq (ilng, ilat, olng, olat)
               sw0008=(memi(cooout+13) )
               goto 560
570            continue
                  call slf54z (olng, olat, slepb(memd((((cooout+10)-1)/2
     *            +1)) ), olng, olat, pmr, pmd)
                  call slsuet (olng, olat, 1950.0d0, olng, olat)
                  if (.not.(memd((((cooout+8)-1)/2+1)) .ne. 1950.0d0)) 
     *            goto 580
                     call slprcs (1, 1950.0d0, memd((((cooout+8)-1)/2+1)
     *               ) , olng, olat)
580               continue
                  if (.not.(memi(cooout+13) .eq. 1)) goto 590
                     call sladet (olng, olat, memd((((cooout+8)-1)/2+1))
     *                , olng, olat)
590               continue
               goto 561
600            continue
                  if (.not.(memd((((cooout+8)-1)/2+1)) .ne. 2000.0d0)) 
     *            goto 610
                     call slprcs (2, 2000.0d0, memd((((cooout+8)-1)/2+1)
     *               ) , olng, olat)
610               continue
               goto 561
620            continue
                  call slf5hz (olng, olat, 2000.0d0, olng, olat)
                  if (.not.(memd((((cooout+8)-1)/2+1)) .ne. 2000.0d0)) 
     *            goto 630
                     call slprcs (2, 2000.0d0, memd((((cooout+8)-1)/2+1)
     *               ) , olng, olat)
630               continue
               goto 561
640            continue
                  call slmap (olng, olat, 0.0d0, 0.0d0, px, 0.0d0, 2000.
     *            0d0, memd((((cooout+10)-1)/2+1)) , olng, olat)
                  goto 561
560            continue
                  if (sw0008.lt.1.or.sw0008.gt.5) goto 561
                  goto (570,570,600,620,640),sw0008
561            continue
            goto 541
650         continue
               call slgaeq (ilng, ilat, olng, olat)
               call sleqec (olng, olat, memd((((cooout+10)-1)/2+1)) , 
     *         olng, olat)
            goto 541
660         continue
               call slgasu (ilng, ilat, olng, olat)
            goto 541
670         continue
               olng = ilng
               olat = ilat
               goto 541
540         continue
               if (sw0007.lt.1.or.sw0007.gt.4) goto 670
               goto (550,650,670,660),sw0007
541         continue
         goto 181
680      continue
            sw0009=(memi(cooout+12) )
            goto 690
700         continue
               call slsuga (ilng, ilat, olng, olat)
               sw0010=(memi(cooout+13) )
               goto 710
720            continue
                  call slgaeq (olng, olat, olng, olat)
                  call slf54z (olng, olat, slepb (memd((((cooout+10)-1)/
     *            2+1)) ), olng, olat, pmr, pmd)
                  call slsuet (olng, olat, 1950.0d0, olng, olat)
                  if (.not.(memd((((cooout+8)-1)/2+1)) .ne. 1950.0d0)) 
     *            goto 730
                     call slprcs (1, 1950.0d0, memd((((cooout+8)-1)/2+1)
     *               ) , olng, olat)
730               continue
                  call sladet (olng, olat, memd((((cooout+8)-1)/2+1)) , 
     *            olng, olat)
               goto 711
740            continue
                  call slgaeq (olng, olat, olng, olat)
                  call slf54z (olng, olat, slepb (memd((((cooout+10)-1)/
     *            2+1)) ), olng, olat, pmr, pmd)
                  call slsuet (olng, olat, 1950.0d0, olng, olat)
                  if (.not.(memd((((cooout+8)-1)/2+1)) .ne. 1950.0d0)) 
     *            goto 750
                     call slprcs (1, 1950.0d0, memd((((cooout+8)-1)/2+1)
     *               ) , olng, olat)
750               continue
               goto 711
760            continue
                  call slgaeq (olng, olat, olng, olat)
                  if (.not.(memd((((cooout+8)-1)/2+1)) .ne. 2000.0d0)) 
     *            goto 770
                     call slprcs (2, 2000.0d0, memd((((cooout+8)-1)/2+1)
     *               ) , olng, olat)
770               continue
               goto 711
780            continue
                  call slgaeq (olng, olat, olng, olat)
                  call slf5hz (olng, olat, 2000.0d0, olng, olat)
                  if (.not.(memd((((cooout+8)-1)/2+1)) .ne. 2000.0d0)) 
     *            goto 790
                     call slprcs (2, 2000.0d0, memd((((cooout+8)-1)/2+1)
     *               ) , olng, olat)
790               continue
               goto 711
800            continue
                  call slgaeq (olng, olat, olng, olat)
                  call slmap (olng, olat, 0.0d0, 0.0d0, px, 0.0d0, 2000.
     *            0d0, memd((((cooout+10)-1)/2+1)) , olng, olat)
                  goto 711
710            continue
                  if (sw0010.lt.1.or.sw0010.gt.5) goto 711
                  goto (720,740,760,780,800),sw0010
711            continue
            goto 691
810         continue
               call slsuga (ilng, ilat, olng, olat)
               call slgaeq (olng, olat, olng, olat)
               call sleqec (olng, olat, memd((((cooout+10)-1)/2+1)) , 
     *         olng, olat)
            goto 691
820         continue
               call slsuga (ilng, ilat, olng, olat)
            goto 691
830         continue
               olng = ilng
               olat = ilat
               goto 691
690         continue
               if (sw0009.lt.1.or.sw0009.gt.3) goto 830
               goto (700,810,820),sw0009
691         continue
         goto 181
840      continue
            olng = ilng
            olat = ilat
            goto 181
180      continue
            if (sw0002.lt.1.or.sw0002.gt.4) goto 840
            goto (190,380,530,680),sw0002
181      continue
100      return
      end
      subroutine skequl (cooin, cooout, ilng, ilat, ipmlng, ipmlat, px, 
     *rv, olng, olat)
      logical Memb(1)
      integer*2 Memc(1)
      integer*2 Mems(1)
      integer Memi(1)
      integer*4 Meml(1)
      real Memr(1)
      double precision Memd(1)
      complex Memx(1)
      equivalence (Memb, Memc, Mems, Memi, Meml, Memr, Memd, Memx)
      common /Mem/ Memd
      integer cooin
      integer cooout
      double precision ilng
      double precision ilat
      double precision ipmlng
      double precision ipmlat
      double precision px
      double precision rv
      double precision olng
      double precision olat
      integer pmflag
      double precision pmr
      double precision pmd
      double precision slepb
      double precision slepj
      integer sw0001,sw0002,sw0003,sw0004
      save
         if (.not.((memi(cooin+13) .eq. memi(cooout+13) ) .and. (memd(((
     *   (cooin+8)-1)/2+1)) .eq. memd((((cooout+8)-1)/2+1)) ) .and. (
     *   memd((((cooin+10)-1)/2+1)) .eq. memd((((cooout+10)-1)/2+1)) )))
     *    goto 110
            olng = ilng
            olat = ilat
            goto 100
110      continue
         if (.not.(.not. ((ipmlng).eq.1.6d308) .and. .not. ((ipmlat).eq.
     *   1.6d308))) goto 120
            pmflag = 1
            goto 121
120      continue
            pmflag = 0
121      continue
         sw0001=(memi(cooin+13) )
         goto 130
140      continue
            if (.not.(pmflag .eq. 1)) goto 150
               call slpm (ilng, ilat, ipmlng, ipmlat, px, rv, slepb (
     *         memd((((cooin+10)-1)/2+1)) ), slepb (memd((((cooout+10)-1
     *         )/2+1)) ), olng, olat)
               goto 151
150         continue
               olng = ilng
               olat = ilat
151         continue
            if (.not.(memi(cooin+13) .eq. 1)) goto 160
               call slsuet (olng, olat, memd((((cooin+8)-1)/2+1)) , olng
     *         , olat)
160         continue
            if (.not.(memd((((cooin+8)-1)/2+1)) .ne. 1950.0d0)) goto 170
               call slprcs (1, memd((((cooin+8)-1)/2+1)) , 1950.0d0, 
     *         olng, olat)
170         continue
            call sladet (olng, olat, 1950.0d0, olng, olat)
            if (.not.(pmflag .eq. 1)) goto 180
               call slf45z (olng, olat, slepb (memd((((cooout+10)-1)/2+1
     *         )) ), olng, olat)
               goto 181
180         continue
               call slf45z (olng, olat, slepb (memd((((cooin+10)-1)/2+1)
     *         ) ), olng, olat)
181         continue
            sw0002=(memi(cooout+13) )
            goto 190
200         continue
               call slf54z (olng, olat, slepb (memd((((cooout+10)-1)/2+1
     *         )) ), olng, olat, pmr, pmd)
               call slsuet (olng, olat, 1950.0d0, olng, olat)
               if (.not.(memd((((cooout+8)-1)/2+1)) .ne. 1950.0d0)) goto
     *          210
                  call slprcs (1, 1950.0d0, memd((((cooout+8)-1)/2+1)) ,
     *             olng, olat)
210            continue
               if (.not.(memi(cooout+13) .eq. 1)) goto 220
                  call sladet (olng, olat, memd((((cooout+8)-1)/2+1)) , 
     *            olng, olat)
220            continue
            goto 191
230         continue
               if (.not.(memd((((cooout+8)-1)/2+1)) .ne. 2000.0d0)) goto
     *          240
                  call slprcs (2, 2000.0d0, memd((((cooout+8)-1)/2+1)) ,
     *             olng, olat)
240            continue
            goto 191
250         continue
               call slf5hz (olng, olat, 2000.0d0, olng, olat)
               if (.not.(memd((((cooout+8)-1)/2+1)) .ne. 2000.0d0)) goto
     *          260
                  call slprcs (2, 2000.0d0, memd((((cooout+8)-1)/2+1)) ,
     *             olng, olat)
260            continue
            goto 191
270         continue
               call slmap (olng, olat, 0.0d0, 0.0d0, px, 0.0d0, 2000.0d0
     *         , memd((((cooout+10)-1)/2+1)) , olng, olat)
               goto 191
190         continue
               if (sw0002.lt.1.or.sw0002.gt.5) goto 191
               goto (200,200,230,250,270),sw0002
191         continue
         goto 131
280      continue
            if (.not.(memi(cooin+13) .eq. 3)) goto 290
               if (.not.(pmflag .eq. 1)) goto 300
                  call slpm (ilng, ilat, ipmlng, ipmlat, px, rv, slepj (
     *            memd((((cooin+10)-1)/2+1)) ), slepj (memd((((cooout+10
     *            )-1)/2+1)) ), olng, olat)
                  goto 301
300            continue
                  olng = ilng
                  olat = ilat
301            continue
               goto 291
290         continue
               call slamp (ilng, ilat, memd((((cooin+10)-1)/2+1)) , 2000
     *         .0d0, olng, olat)
291         continue
            sw0003=(memi(cooout+13) )
            goto 310
320         continue
               if (.not.(memd((((cooin+8)-1)/2+1)) .ne. 2000.0d0)) goto 
     *         330
                  call slprcs (2, memd((((cooin+8)-1)/2+1)) , 2000.0d0, 
     *            olng, olat)
330            continue
               call slf54z (olng, olat, slepb(memd((((cooout+10)-1)/2+1)
     *         ) ), olng, olat, pmr, pmd)
               call slsuet (olng, olat, 1950.0d0, olng, olat)
               if (.not.(memd((((cooout+8)-1)/2+1)) .ne. 1950.0d0)) goto
     *          340
                  call slprcs (1, 1950.0d0, memd((((cooout+8)-1)/2+1)) ,
     *             olng, olat)
340            continue
               if (.not.(memi(cooout+13) .eq. 1)) goto 350
                  call sladet (olng, olat, memd((((cooout+8)-1)/2+1)) , 
     *            olng, olat)
350            continue
            goto 311
360         continue
               if (.not.(memd((((cooin+8)-1)/2+1)) .ne. memd((((cooout+8
     *         )-1)/2+1)) )) goto 370
                  call slprcs (2, memd((((cooin+8)-1)/2+1)) , memd((((
     *            cooout+8)-1)/2+1)) , olng, olat)
370            continue
            goto 311
380         continue
               if (.not.(memd((((cooin+8)-1)/2+1)) .ne. 2000.0d0)) goto 
     *         390
                  call slprcs (2, memd((((cooin+8)-1)/2+1)) , 2000.0d0, 
     *            olng, olat)
390            continue
               call slf5hz (olng, olat, slepj(memd((((cooin+10)-1)/2+1))
     *          ), olng, olat)
               if (.not.(memd((((cooout+8)-1)/2+1)) .ne. 2000.0d0)) goto
     *          400
                  call slprcs (2, 2000.0d0, memd((((cooout+8)-1)/2+1)) ,
     *             olng, olat)
400            continue
            goto 311
410         continue
               if (.not.(memd((((cooin+8)-1)/2+1)) .ne. 2000.0d0)) goto 
     *         420
                  call slprcs (2, memd((((cooin+8)-1)/2+1)) , 2000.0d0, 
     *            olng, olat)
420            continue
               call slmap (olng, olat, 0.0d0, 0.0d0, px, 0.0d0, 2000.0d0
     *         , memd((((cooout+10)-1)/2+1)) , olng, olat)
               goto 311
310         continue
               if (sw0003.lt.1.or.sw0003.gt.5) goto 311
               goto (320,320,360,380,410),sw0003
311         continue
         goto 131
430      continue
            if (.not.(pmflag .eq. 1)) goto 440
               call slpm (ilng, ilat, ipmlng, ipmlat, px, rv, slepj (
     *         memd((((cooin+10)-1)/2+1)) ), slepj (memd((((cooout+10)-1
     *         )/2+1)) ), olng, olat)
               goto 441
440         continue
               olng = ilng
               olat = ilat
441         continue
            sw0004=(memi(cooout+13) )
            goto 450
460         continue
               if (.not.(memd((((cooin+8)-1)/2+1)) .ne. 2000.0d0)) goto 
     *         470
                  call slprcs (2, memd((((cooin+8)-1)/2+1)) , 2000.0d0, 
     *            olng, olat)
470            continue
               call slhf5z (olng, olat, 2000.0d0, olng, olat, pmr, pmd)
               call slf54z (olng, olat, slepb(memd((((cooout+10)-1)/2+1)
     *         ) ), olng, olat, pmr, pmd)
               call slsuet (olng, olat, 1950.0d0, olng, olat)
               if (.not.(memd((((cooout+8)-1)/2+1)) .ne. 1950.0d0)) goto
     *          480
                  call slprcs (1, 1950.0d0, memd((((cooout+8)-1)/2+1)) ,
     *             olng, olat)
480            continue
               if (.not.(memi(cooout+13) .eq. 1)) goto 490
                  call sladet (olng, olat, memd((((cooout+8)-1)/2+1)) , 
     *            olng, olat)
490            continue
            goto 451
500         continue
               if (.not.(memd((((cooin+8)-1)/2+1)) .ne. 2000.0d0)) goto 
     *         510
                  call slprcs (2, memd((((cooin+8)-1)/2+1)) , 2000.0d0, 
     *            olng, olat)
510            continue
               call slhf5z (olng, olat, slepj(memd((((cooout+10)-1)/2+1)
     *         ) ), olng, olat, pmr, pmd)
               if (.not.(memd((((cooout+8)-1)/2+1)) .ne. 2000.0d0)) goto
     *          520
                  call slprcs (2, 2000.0d0, memd((((cooout+8)-1)/2+1)) ,
     *             olng, olat)
520            continue
            goto 451
530         continue
               if (.not.(memd((((cooin+8)-1)/2+1)) .ne. memd((((cooout+8
     *         )-1)/2+1)) )) goto 540
                  call slprcs (2, memd((((cooin+8)-1)/2+1)) , memd((((
     *            cooout+8)-1)/2+1)) , olng, olat)
540            continue
            goto 451
550         continue
               if (.not.(memd((((cooin+8)-1)/2+1)) .ne. 2000.0d0)) goto 
     *         560
                  call slprcs (2, memd((((cooin+8)-1)/2+1)) , 2000.0d0, 
     *            olng, olat)
560            continue
               call slhf5z (olng, olat, slepj(memd((((cooout+10)-1)/2+1)
     *         ) ), olng, olat, pmr, pmd)
               call slmap (olng, olat, 0.0d0, 0.0d0, px, 0.0d0, 2000.0d0
     *         , memd((((cooout+10)-1)/2+1)) , olng, olat)
               goto 451
450         continue
               if (sw0004.lt.1.or.sw0004.gt.5) goto 451
               goto (460,460,500,530,550),sw0004
451         continue
            goto 131
130      continue
            if (sw0001.lt.1.or.sw0001.gt.5) goto 131
            goto (140,140,280,430,280),sw0001
131      continue
100      return
      end
c     sleceq  sl_eceq
c     sleqec  sl_eqec
c     sladet  sl_adet
c     sleqga  sl_eqga
c     slgaeq  sl_gaeq
c     slf45z  sl_f45z
c     slf54z  sl_f54z
c     slhf5z  sl_hf5z
c     slf5hz  sl_f5hz
c     slgasu  sl_gasu
c     slsuga  sl_suga
c     skequl  sk_equatorial
c     sklltn  sk_lltran
c     slprcs  sl_prcs
c     skultn  sk_ultran
c     slsuet  sl_suet
