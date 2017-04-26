      integer function skdecs (instr, mw, coo, imcoo)
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
      integer mw
      integer coo
      integer imcoo
      integer*2 instr(*)
      integer stat
      integer sp
      integer str1
      integer str2
      integer laxno
      integer paxval
      integer im
      integer skstrs
      integer skdecm
      integer immap
      logical xerpop
      logical xerflg
      common /xercom/ xerflg
      integer sw0001
      save
         call xcallc(coo, (30 + 255 + 1), 10 )
         call xstrcy(instr, memc((((coo+25)-1)*2+1)) , 255 )
         call smark (sp)
         call salloc (str1, 1023 , 2)
         call salloc (str2, 1023 , 2)
         call salloc (laxno, 7 , 4)
         call salloc (paxval, 7 , 4)
         call sscan (instr)
         call gargwd (memc(str1), 1023 )
         call gargwd (memc(str2), 1023 )
         call xerpsh
         im = immap (memc(str1), 1 , 0)
         if (xerflg) goto 112
112      if (.not.xerpop()) goto 110
            mw = 0
            if (.not.(imcoo .eq. 0)) goto 120
               memi(coo+20) = 2048
               memi(coo+21) = 2048
               memi(coo+15) = 1
               memi(coo+16) = 2
               memi(coo+17) = 1
               memi(coo+18) = 2
               memd((((coo)-1)/2+1)) = 0.0d0
               memd((((coo+2)-1)/2+1)) = 0.0d0
               memd((((coo+4)-1)/2+1)) = 1.0d0
               memd((((coo+6)-1)/2+1)) = 1.0d0
               memi(coo+14) = 0
               goto 121
120         continue
               memi(coo+20) = memi(imcoo+20)
               memi(coo+21) = memi(imcoo+21)
               memi(coo+15) = memi(imcoo+15)
               memi(coo+16) = memi(imcoo+16)
               memi(coo+17) = memi(imcoo+17)
               memi(coo+18) = memi(imcoo+18)
               memd((((coo)-1)/2+1)) = memd((((imcoo)-1)/2+1))
               memd((((coo+2)-1)/2+1)) = memd((((imcoo+2)-1)/2+1))
               memd((((coo+4)-1)/2+1)) = memd((((imcoo+4)-1)/2+1))
               memd((((coo+6)-1)/2+1)) = memd((((imcoo+6)-1)/2+1))
               memi(coo+14) = memi(imcoo+14)
121         continue
            memi(coo+19) = 4
            stat = skstrs (instr, memi(coo+12) , memi(coo+13) , memd((((
     *      coo+8)-1)/2+1)) , memd((((coo+10)-1)/2+1)) )
            sw0001=(memi(coo+12) )
            goto 130
140         continue
               memi(coo+22) = 3
               memi(coo+23) = 1
            goto 131
150         continue
               memi(coo+22) = 1
               memi(coo+23) = 1
               goto 131
130         continue
               if (sw0001.eq.1) goto 140
               goto 150
131         continue
            goto 111
110      continue
            stat = skdecm (im, memc(str2), mw, coo)
            call imunmp (im)
111      continue
         call sfree (sp)
         memi(coo+24) = stat
         skdecs = (stat)
         goto 100
100      return
      end
      integer function skdecr (instr, coo, imcoo)
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
      integer coo
      integer imcoo
      integer*2 instr(*)
      integer stat
      integer skstrs
      integer sw0001
      save
         call xcallc(coo, (30 + 255 + 1), 10 )
         call xstrcy(instr, memc((((coo+25)-1)*2+1)) , 255 )
         if (.not.(imcoo .eq. 0)) goto 110
            memi(coo+20) = 2048
            memi(coo+21) = 2048
            memi(coo+15) = 1
            memi(coo+16) = 2
            memi(coo+17) = 1
            memi(coo+18) = 2
            memd((((coo)-1)/2+1)) = 0.0d0
            memd((((coo+2)-1)/2+1)) = 0.0d0
            memd((((coo+4)-1)/2+1)) = 1.0d0
            memd((((coo+6)-1)/2+1)) = 1.0d0
            memi(coo+14) = 0
            goto 111
110      continue
            memi(coo+20) = memi(imcoo+20)
            memi(coo+21) = memi(imcoo+21)
            memi(coo+15) = memi(imcoo+15)
            memi(coo+16) = memi(imcoo+16)
            memi(coo+17) = memi(imcoo+17)
            memi(coo+18) = memi(imcoo+18)
            memd((((coo)-1)/2+1)) = memd((((imcoo)-1)/2+1))
            memd((((coo+2)-1)/2+1)) = memd((((imcoo+2)-1)/2+1))
            memd((((coo+4)-1)/2+1)) = memd((((imcoo+4)-1)/2+1))
            memd((((coo+6)-1)/2+1)) = memd((((imcoo+6)-1)/2+1))
            memi(coo+14) = memi(imcoo+14)
111      continue
         memi(coo+19) = 4
         stat = skstrs (instr, memi(coo+12) , memi(coo+13) , memd((((coo
     *   +8)-1)/2+1)) , memd((((coo+10)-1)/2+1)) )
         sw0001=(memi(coo+12) )
         goto 120
130      continue
            memi(coo+22) = 3
            memi(coo+23) = 1
         goto 121
140      continue
            memi(coo+22) = 1
            memi(coo+23) = 1
            goto 121
120      continue
            if (sw0001.eq.1) goto 130
            goto 140
121      continue
         memi(coo+24) = stat
         skdecr = (stat)
         goto 100
100      return
      end
      integer function skdecm (im, wcs, mw, coo)
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
      integer im
      integer mw
      integer coo
      integer*2 wcs(*)
      integer stat
      integer sp
      integer str1
      integer laxno
      integer paxval
      integer skimws
      integer strdic
      integer mwstai
      integer mwopem
      logical xerpop
      logical xerflg
      common /xercom/ xerflg
      integer sw0001
      integer*2 st0001(6)
      integer*2 st0002(28)
      save
      integer iyy
      data st0001 / 37,115, 32, 37,115, 0/
      data (st0002(iyy),iyy= 1, 8) /124,108,111,103,105, 99, 97,108/
      data (st0002(iyy),iyy= 9,16) /124,116,118,124,112,104,121,115/
      data (st0002(iyy),iyy=17,24) /105, 99, 97,108,124,119,111,114/
      data (st0002(iyy),iyy=25,28) /108,100,124, 0/
         call xmallc(coo, (30 + 255 + 1), 10 )
         call sprinf (memc((((coo+25)-1)*2+1)) , 255 , st0001)
         call pargsr (memc((((im+200 +165)-1)*2+1)) )
         call pargsr (wcs)
         call smark (sp)
         call salloc (str1, 1023 , 2)
         call salloc (laxno, 7 , 4)
         call salloc (paxval, 7 , 4)
         call xerpsh
         mw = mwopem (im)
         if (xerflg) goto 112
112      if (.not.xerpop()) goto 110
            memi(coo+12) = 0
            memi(coo+13) = 0
            memd((((coo+8)-1)/2+1)) = 1.6d308
            memd((((coo+10)-1)/2+1)) = 1.6d308
            mw = 0
            memi(coo+15) = 1
            memi(coo+16) = 2
            memi(coo+17) = 1
            memi(coo+18) = 2
            memi(coo+20) = 2048
            memi(coo+21) = 2048
            memd((((coo)-1)/2+1)) = 0.0d0
            memd((((coo+2)-1)/2+1)) = 0.0d0
            memd((((coo+4)-1)/2+1)) = 1.0d0
            memd((((coo+6)-1)/2+1)) = 1.0d0
            memi(coo+14) = 0
            memi(coo+19) = 1
            memi(coo+22) = 1
            memi(coo+23) = 1
            stat = -1
            goto 111
110      continue
            memi(coo+19) = strdic (wcs, memc(str1), 1023 , st0002)
            if (.not.(memi(coo+19) .le. 0)) goto 120
               memi(coo+19) = 1
120         continue
            if (.not.(skimws (im, mw, memi(coo+12) , memi(coo+15) , memi
     *      (coo+16) , memi(coo+14) , memi(coo+13) , memd((((coo+8)-1)/2
     *      +1)) , memd((((coo+10)-1)/2+1)) ) .eq. 0)) goto 130
               sw0001=(memi(coo+12) )
               goto 140
150            continue
                  memi(coo+22) = 3
                  memi(coo+23) = 1
               goto 141
160            continue
                  memi(coo+22) = 1
                  memi(coo+23) = 1
                  goto 141
140            continue
                  if (sw0001.eq.1) goto 150
                  goto 160
141            continue
               call mwgaxp (mw, memi(laxno), memi(paxval), mwstai(mw, 5 
     *         ))
               if (.not.(memi(laxno+memi(coo+15) -1) .lt. memi(laxno+
     *         memi(coo+16) -1))) goto 170
                  memi(coo+17) = memi(laxno+memi(coo+15) -1)
                  memi(coo+18) = memi(laxno+memi(coo+16) -1)
                  goto 171
170            continue
                  memi(coo+17) = memi(laxno+memi(coo+16) -1)
                  memi(coo+18) = memi(laxno+memi(coo+15) -1)
171            continue
               if (.not.(memi(coo+17) .le. 0 .or. memi(coo+18) .le. 0)) 
     *         goto 180
                  memd((((coo)-1)/2+1)) = 0.0d0
                  memd((((coo+2)-1)/2+1)) = 0.0d0
                  memd((((coo+4)-1)/2+1)) = 1.0d0
                  memd((((coo+6)-1)/2+1)) = 1.0d0
                  memi(coo+20) = 2048
                  memi(coo+21) = 2048
                  stat = -1
                  goto 181
180            continue
                  memd((((coo)-1)/2+1)) = meml(im+memi(im+memi(coo+17) +
     *            47-1) +54-1)
                  memd((((coo+2)-1)/2+1)) = meml(im+memi(im+memi(coo+18)
     *             +47-1) +54-1)
                  memd((((coo+4)-1)/2+1)) = memi(im+memi(coo+17) +59-1)
                  memd((((coo+6)-1)/2+1)) = memi(im+memi(coo+18) +59-1)
                  memi(coo+20) = meml(im+200 +memi(coo+17) +8-1)
                  memi(coo+21) = meml(im+200 +memi(coo+18) +8-1)
                  stat = 0
181            continue
               goto 131
130         continue
               call mwcloe (mw)
               mw = 0
               memi(coo+17) = 1
               memi(coo+18) = 2
               memi(coo+20) = 2048
               memi(coo+21) = 2048
               memd((((coo)-1)/2+1)) = 0.0d0
               memd((((coo+2)-1)/2+1)) = 0.0d0
               memd((((coo+4)-1)/2+1)) = 1.0d0
               memd((((coo+6)-1)/2+1)) = 1.0d0
               memi(coo+22) = 1
               memi(coo+23) = 1
               stat = -1
131         continue
111      continue
         call sfree (sp)
         memi(coo+24) = stat
         skdecm = (stat)
         goto 100
100      return
      end
      integer function skstrs (instr, ctype, radecs, equinx, epoch)
      integer ctype
      integer radecs
      double precision equinx
      double precision epoch
      integer*2 instr(*)
      integer ip
      integer nitems
      integer sctype
      integer srades
      integer stat
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
      integer sp
      integer str1
      integer str2
      integer strdic
      integer nscan
      integer ctod
      double precision slej2d
      double precision slepb
      double precision sleb2d
      double precision slepj
      integer sw0001,sw0002,sw0003
      integer*2 st0001(63)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /124,102,107, 52,124,110,111,101/
      data (st0001(iyy),iyy= 9,16) /102,107, 52,124,102,107, 53,124/
      data (st0001(iyy),iyy=17,24) /105, 99,114,115,124, 97,112,112/
      data (st0001(iyy),iyy=25,32) / 97,114,101,110,116,124,101, 99/
      data (st0001(iyy),iyy=33,40) /108,105,112,116,105, 99,124,103/
      data (st0001(iyy),iyy=41,48) / 97,108, 97, 99,116,105, 99,124/
      data (st0001(iyy),iyy=49,56) /115,117,112,101,114,103, 97,108/
      data (st0001(iyy),iyy=57,63) / 97, 99,116,105, 99,124, 0/
         ctype = 0
         radecs = 0
         equinx = 1.6d308
         epoch = 1.6d308
         call smark (sp)
         call salloc (str1, 1023 , 2)
         call salloc (str2, 1023 , 2)
         call sscan (instr)
         call gargwd (memc(str1), 1023 )
         if (.not.(memc(str1) .eq. 0 .or. nscan() .lt. 1)) goto 110
            call sfree (sp)
            skstrs = (-1)
            goto 100
110      continue
            nitems = 1
111      continue
         sctype = strdic (memc(str1), memc(str2), 1023 , st0001)
         if (.not.(sctype .le. 0)) goto 120
            ctype = 1
            goto 121
120      continue
            sw0001=(sctype)
            goto 130
140         continue
               ctype = 1
               radecs = 1
            goto 131
150         continue
               ctype = 1
               radecs = 2
            goto 131
160         continue
               ctype = 1
               radecs = 3
            goto 131
170         continue
               ctype = 1
               radecs = 4
            goto 131
180         continue
               ctype = 1
               radecs = 5
            goto 131
190         continue
               ctype = 2
            goto 131
200         continue
               ctype = 3
            goto 131
210         continue
               ctype = 4
               goto 131
130         continue
               if (sw0001.lt.1.or.sw0001.gt.8) goto 131
               goto (140,150,160,170,180,190,200,210),sw0001
131         continue
            call gargwd (memc(str1), 1023 )
            if (.not.(nscan() .gt. nitems)) goto 220
               nitems = nitems + 1
220         continue
121      continue
         sctype = ctype
         srades = radecs
         sw0002=(sctype)
         goto 230
240      continue
            sw0003=(srades)
            goto 250
260         continue
               if (.not.(memc(str1) .eq. 74 .or. memc(str1) .eq. 106 .or
     *         . memc(str1) .eq. 66 .or. memc(str1) .eq. 98)) goto 270
                  ip = 2
                  goto 271
270            continue
                  ip = 1
271            continue
               if (.not.(ctod (memc(str1), ip, equinx) .le. 0)) goto 280
                  equinx = 1950.0d0
280            continue
               if (.not.(memc(str1) .eq. 74 .or. memc(str1) .eq. 106)) 
     *         goto 290
                  equinx = slepb (slej2d (equinx))
290            continue
               call gargwd (memc(str2), 1023 )
               if (.not.(nscan() .le. nitems)) goto 300
                  epoch = sleb2d (equinx)
                  goto 301
300            continue
                  if (.not.(memc(str2) .eq. 74 .or. memc(str2) .eq. 106 
     *            .or. memc(str2) .eq. 66 .or. memc(str2) .eq. 98)) goto
     *             310
                     ip = 2
                     goto 311
310               continue
                     ip = 1
311               continue
                  if (.not.(ctod (memc(str2), ip, epoch) .le. 0)) goto 
     *            320
                     epoch = sleb2d (equinx)
                     goto 321
320               continue
                  if (.not.(epoch .le. 3000.0d0 .and. (memc(str2) .eq. 
     *            74 .or. memc(str2) .eq. 106))) goto 330
                     epoch = slej2d (epoch)
                     goto 331
330               continue
                  if (.not.(epoch .gt. 3000.0d0)) goto 340
                     epoch = epoch - 2400000.5d0
                     goto 341
340               continue
                     epoch = sleb2d (epoch)
341               continue
331               continue
321               continue
301            continue
            goto 251
350         continue
               if (.not.(memc(str1) .eq. 74 .or. memc(str1) .eq. 106 .or
     *         . memc(str1) .eq. 66 .or. memc(str1) .eq. 98)) goto 360
                  ip = 2
                  goto 361
360            continue
                  ip = 1
361            continue
               if (.not.(ctod (memc(str1), ip, equinx) .le. 0)) goto 370
                  equinx = 2000.0d0
370            continue
               if (.not.(memc(str1) .eq. 66 .or. memc(str1) .eq. 98)) 
     *         goto 380
                  equinx = slepj(sleb2d (equinx))
380            continue
               call gargwd (memc(str2), 1023 )
               if (.not.(nscan() .le. nitems)) goto 390
                  epoch = slej2d (equinx)
                  goto 391
390            continue
                  if (.not.(memc(str2) .eq. 74 .or. memc(str2) .eq. 106 
     *            .or. memc(str2) .eq. 66 .or. memc(str2) .eq. 98)) goto
     *             400
                     ip = 2
                     goto 401
400               continue
                     ip = 1
401               continue
                  if (.not.(ctod (memc(str2), ip, epoch) .le. 0)) goto 
     *            410
                     epoch = slej2d (equinx)
                     goto 411
410               continue
                  if (.not.(epoch .le. 3000.0d0 .and. (memc(str2) .eq. 
     *            66 .or. memc(str2) .eq. 98))) goto 420
                     epoch = sleb2d (epoch)
                     goto 421
420               continue
                  if (.not.(epoch .gt. 3000.0d0)) goto 430
                     epoch = epoch - 2400000.5d0
                     goto 431
430               continue
                     epoch = slej2d (epoch)
431               continue
421               continue
411               continue
391            continue
            goto 251
440         continue
               equinx = 2000.0d0
               if (.not.(memc(str1) .eq. 74 .or. memc(str1) .eq. 106 .or
     *         . memc(str1) .eq. 66 .or. memc(str1) .eq. 98)) goto 450
                  ip = 2
                  goto 451
450            continue
                  ip = 1
451            continue
               if (.not.(ctod (memc(str1), ip, epoch) .le. 0)) goto 460
                  epoch = 1.6d308
                  goto 461
460            continue
               if (.not.(epoch .le. 3000.0d0)) goto 470
                  if (.not.(memc(str1) .eq. 66 .or. memc(str1) .eq. 98))
     *             goto 480
                     epoch = sleb2d (epoch)
                     goto 481
480               continue
                  if (.not.(memc(str1) .eq. 74 .or. memc(str1) .eq. 106)
     *            ) goto 490
                     epoch = slej2d (epoch)
                     goto 491
490               continue
                  if (.not.(epoch .lt. 1984.0d0)) goto 500
                     epoch = sleb2d (epoch)
                     goto 501
500               continue
                     epoch = slej2d (epoch)
501               continue
491               continue
481               continue
                  goto 471
470            continue
                  epoch = epoch - 2400000.5d0
471            continue
461            continue
            goto 251
510         continue
               ip = 1
               if (.not.(memc(str1) .eq. 66 .or. memc(str1) .eq. 98)) 
     *         goto 520
                  radecs = 1
                  ip = ip + 1
                  if (.not.(ctod (memc(str1), ip, equinx) .le. 0)) goto 
     *            530
                     equinx = 1950.0d0
530               continue
                  call gargwd (memc(str2), 1023 )
                  if (.not.(nscan() .le. nitems)) goto 540
                     epoch = sleb2d (equinx)
                     goto 541
540               continue
                     if (.not.(memc(str2) .eq. 74 .or. memc(str2) .eq. 
     *               106)) goto 550
                        ip = 2
                        goto 551
550                  continue
                     if (.not.(memc(str2) .eq. 66 .or. memc(str2) .eq. 
     *               98)) goto 560
                        ip = 2
                        goto 561
560                  continue
                        ip = 1
561                  continue
551                  continue
                     if (.not.(ctod (memc(str2), ip, epoch) .le. 0)) 
     *               goto 570
                        epoch = sleb2d (equinx)
                        goto 571
570                  continue
                     if (.not.(epoch .le. 3000.0d0 .and. (memc(str2) .eq
     *               . 74 .or. memc(str2) .eq. 106))) goto 580
                        epoch = slej2d (epoch)
                        goto 581
580                  continue
                     if (.not.(epoch .gt. 3000.0d0)) goto 590
                        epoch = epoch - 2400000.5d0
                        goto 591
590                  continue
                        epoch = sleb2d (epoch)
591                  continue
581                  continue
571                  continue
541               continue
                  goto 521
520            continue
               if (.not.(memc(str1) .eq. 74 .or. memc(str1) .eq. 106)) 
     *         goto 600
                  radecs = 3
                  ip = ip + 1
                  if (.not.(ctod (memc(str1), ip, equinx) .le. 0)) goto 
     *            610
                     equinx = 2000.0d0
610               continue
                  call gargwd (memc(str2), 1023 )
                  if (.not.(nscan() .le. nitems)) goto 620
                     epoch = slej2d (equinx)
                     goto 621
620               continue
                     if (.not.(memc(str2) .eq. 74 .or. memc(str2) .eq. 
     *               106 .or. memc(str2) .eq. 66 .or. memc(str2) .eq. 98
     *               )) goto 630
                        ip = 2
                        goto 631
630                  continue
                        ip = 1
631                  continue
                     if (.not.(ctod (memc(str2), ip, epoch) .le. 0)) 
     *               goto 640
                        epoch = slej2d (equinx)
                        goto 641
640                  continue
                     if (.not.(epoch .le. 3000.0d0 .and. (memc(str2) .eq
     *               . 66 .or. memc(str2) .eq. 98))) goto 650
                        epoch = sleb2d (epoch)
                        goto 651
650                  continue
                     if (.not.(epoch .gt. 3000.0d0)) goto 660
                        epoch = epoch - 2400000.5d0
                        goto 661
660                  continue
                        epoch = slej2d (epoch)
661                  continue
651                  continue
641                  continue
621               continue
                  goto 601
600            continue
               if (.not.(ctod (memc(str1), ip, equinx) .le. 0)) goto 670
                  ctype = 0
                  radecs = 0
                  equinx = 1.6d308
                  epoch = 1.6d308
                  goto 671
670            continue
               if (.not.(equinx .lt. 1984.0d0)) goto 680
                  radecs = 1
                  call gargwd (memc(str2), 1023 )
                  if (.not.(nscan() .le. nitems)) goto 690
                     epoch = sleb2d (equinx)
                     goto 691
690               continue
                     if (.not.(memc(str2) .eq. 74 .or. memc(str2) .eq. 
     *               106 .or. memc(str2) .eq. 66 .or. memc(str2) .eq. 98
     *               )) goto 700
                        ip = 2
                        goto 701
700                  continue
                        ip = 1
701                  continue
                     if (.not.(ctod (memc(str2), ip, epoch) .le. 0)) 
     *               goto 710
                        epoch = sleb2d (equinx)
                        goto 711
710                  continue
                     if (.not.(epoch .le. 3000.0d0 .and. (memc(str2) .eq
     *               . 74 .or. memc(str2) .eq. 106))) goto 720
                        epoch = slej2d (epoch)
                        goto 721
720                  continue
                     if (.not.(epoch .gt. 3000.0d0)) goto 730
                        epoch = epoch - 2400000.5d0
                        goto 731
730                  continue
                        epoch = sleb2d (epoch)
731                  continue
721                  continue
711                  continue
691               continue
                  goto 681
680            continue
                  radecs = 3
                  call gargwd (memc(str2), 1023 )
                  if (.not.(nscan() .le. nitems)) goto 740
                     epoch = slej2d (equinx)
                     goto 741
740               continue
                     if (.not.(memc(str2) .eq. 74 .or. memc(str2) .eq. 
     *               106 .or. memc(str2) .eq. 66 .or. memc(str2) .eq. 98
     *               )) goto 750
                        ip = 2
                        goto 751
750                  continue
                        ip = 1
751                  continue
                     if (.not.(ctod (memc(str2), ip, epoch) .le. 0)) 
     *               goto 760
                        epoch = slej2d (equinx)
                        goto 761
760                  continue
                     if (.not.(epoch .le. 3000.0d0 .and. (memc(str2) .eq
     *               . 66 .or. memc(str2) .eq. 98))) goto 770
                        epoch = sleb2d (epoch)
                        goto 771
770                  continue
                     if (.not.(epoch .gt. 3000.0d0)) goto 780
                        epoch = epoch - 2400000.5d0
                        goto 781
780                  continue
                        epoch = slej2d (epoch)
781                  continue
771                  continue
761                  continue
741               continue
681            continue
671            continue
601            continue
521            continue
               goto 251
250         continue
               if (sw0003.lt.1.or.sw0003.gt.5) goto 510
               goto (260,260,350,350,440),sw0003
251         continue
         goto 231
790      continue
            if (.not.(memc(str1) .eq. 74 .or. memc(str1) .eq. 106 .or. 
     *      memc(str1) .eq. 66 .or. memc(str1) .eq. 98)) goto 800
               ip = 2
               goto 801
800         continue
               ip = 1
801         continue
            if (.not.(ctod (memc(str1), ip, epoch) .le. 0)) goto 810
               epoch = 1.6d308
               goto 811
810         continue
            if (.not.(epoch .le. 3000.0d0)) goto 820
               if (.not.(memc(str1) .eq. 66 .or. memc(str1) .eq. 98)) 
     *         goto 830
                  epoch = sleb2d (epoch)
                  goto 831
830            continue
               if (.not.(memc(str1) .eq. 74 .or. memc(str1) .eq. 106)) 
     *         goto 840
                  epoch = slej2d (epoch)
                  goto 841
840            continue
               if (.not.(epoch .lt. 1984.0d0)) goto 850
                  epoch = sleb2d (epoch)
                  goto 851
850            continue
                  epoch = slej2d (epoch)
851            continue
841            continue
831            continue
               goto 821
820         continue
               epoch = epoch - 2400000.5d0
821         continue
811         continue
         goto 231
860      continue
            if (.not.(memc(str1) .eq. 74 .or. memc(str1) .eq. 106 .or. 
     *      memc(str1) .eq. 66 .or. memc(str1) .eq. 98)) goto 870
               ip = 2
               goto 871
870         continue
               ip = 1
871         continue
            if (.not.(ctod (memc(str1), ip, epoch) .le. 0)) goto 880
               epoch = sleb2d (1950.0d0)
               goto 881
880         continue
            if (.not.(epoch .le. 3000.0d0)) goto 890
               if (.not.(memc(str1) .eq. 74 .or. memc(str1) .eq. 106)) 
     *         goto 900
                  epoch = slej2d (epoch)
                  goto 901
900            continue
               if (.not.(memc(str1) .eq. 66 .or. memc(str1) .eq. 98)) 
     *         goto 910
                  epoch = sleb2d (epoch)
                  goto 911
910            continue
               if (.not.(epoch .lt. 1984.0d0)) goto 920
                  epoch = sleb2d (epoch)
                  goto 921
920            continue
                  epoch = slej2d (epoch)
921            continue
911            continue
901            continue
               goto 891
890         continue
               epoch = epoch - 2400000.5d0
891         continue
881         continue
            goto 231
230      continue
            if (sw0002.lt.1.or.sw0002.gt.4) goto 231
            goto (240,790,860,860),sw0002
231      continue
         if (.not.(ctype .eq. 0)) goto 930
            stat = -1
            goto 931
930      continue
         if (.not.(ctype .eq. 1 .and. (radecs .eq. 0 .or. ((equinx).eq.1
     *   .6d308) .or. ((epoch).eq.1.6d308)))) goto 940
            stat = -1
            goto 941
940      continue
         if (.not.(ctype .eq. 2 .and. ((epoch).eq.1.6d308))) goto 950
            stat = -1
            goto 951
950      continue
            stat = 0
951      continue
941      continue
931      continue
         call sfree (sp)
         skstrs = (stat)
         goto 100
100      return
      end
      integer function skimws (im, mw, ctype, lngax, latax, wtype, 
     *radecs, equinx, epoch)
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
      integer im
      integer mw
      integer ctype
      integer lngax
      integer latax
      integer wtype
      integer radecs
      double precision equinx
      double precision epoch
      integer i
      integer ndim
      integer axtype
      integer day
      integer month
      integer year
      integer ier
      integer oldfis
      integer sp
      integer atval
      double precision hours
      double precision imgetd
      double precision sleb2d
      double precision slej2d
      integer mwstai
      integer strdic
      integer dtmdee
      logical xerpop
      logical xerflg
      common /xercom/ xerflg
      integer sw0001,sw0002,sw0003
      integer*2 st0001(7)
      integer*2 st0002(6)
      integer*2 st0003(39)
      integer*2 st0004(6)
      integer*2 st0005(6)
      integer*2 st0006(7)
      integer*2 st0007(114)
      integer*2 st0008(8)
      integer*2 st0009(6)
      integer*2 st0010(9)
      integer*2 st0011(30)
      integer*2 st0012(8)
      integer*2 st0013(8)
      integer*2 st0014(9)
      integer*2 st0015(8)
      integer*2 st0016(8)
      integer*2 st0017(9)
      integer*2 st0018(8)
      integer*2 st0019(8)
      integer*2 st0020(9)
      save
      integer iyy
      data st0001 / 97,120,116,121,112,101, 0/
      data st0002 / 73, 78, 68, 69, 70, 0/
      data (st0003(iyy),iyy= 1, 8) /124,114, 97,124,100,101, 99,124/
      data (st0003(iyy),iyy= 9,16) /103,108,111,110,124,103,108, 97/
      data (st0003(iyy),iyy=17,24) /116,124,101,108,111,110,124,101/
      data (st0003(iyy),iyy=25,32) /108, 97,116,124,115,108,111,110/
      data (st0003(iyy),iyy=33,39) /124,115,108, 97,116,124, 0/
      data st0004 /119,116,121,112,101, 0/
      data st0005 /119,116,121,112,101, 0/
      data st0006 /108,105,110,101, 97,114, 0/
      data (st0007(iyy),iyy= 1, 8) /124,108,105,110,124, 97,122,112/
      data (st0007(iyy),iyy= 9,16) /124,116, 97,110,124,115,105,110/
      data (st0007(iyy),iyy=17,24) /124,115,116,103,124, 97,114, 99/
      data (st0007(iyy),iyy=25,32) /124,122,112,110,124,122,101, 97/
      data (st0007(iyy),iyy=33,40) /124, 97,105,114,124, 99,121,112/
      data (st0007(iyy),iyy=41,48) /124, 99, 97,114,124,109,101,114/
      data (st0007(iyy),iyy=49,56) /124, 99,101, 97,124, 99,111,112/
      data (st0007(iyy),iyy=57,64) /124, 99,111,100,124, 99,111,101/
      data (st0007(iyy),iyy=65,72) /124, 99,111,111,124, 98,111,110/
      data (st0007(iyy),iyy=73,80) /124,112, 99,111,124,103,108,115/
      data (st0007(iyy),iyy=81,88) /124,112, 97,114,124, 97,105,116/
      data (st0007(iyy),iyy=89,96) /124,109,111,108,124, 99,115, 99/
      data (st0007(iyy),iyy=97,104) /124,113,115, 99,124,116,115, 99/
      data (st0007(iyy),iyy=105,112) /124,116,110,120,124,122,112,120/
      data (st0007(iyy),iyy=113,114) /124, 0/
      data st0008 / 69, 81, 85, 73, 78, 79, 88, 0/
      data st0009 / 69, 80, 79, 67, 72, 0/
      data (st0010(iyy),iyy= 1, 8) / 82, 65, 68, 69, 67, 83, 89, 83/
      data (st0010(iyy),iyy= 9, 9) / 0/
      data (st0011(iyy),iyy= 1, 8) /124,102,107, 52,124,102,107, 52/
      data (st0011(iyy),iyy= 9,16) / 45,110,111, 45,101,124,102,107/
      data (st0011(iyy),iyy=17,24) / 53,124,105, 99,114,115,124,103/
      data (st0011(iyy),iyy=25,30) / 97,112,112,116,124, 0/
      data st0012 / 77, 74, 68, 45, 87, 67, 83, 0/
      data st0013 / 77, 74, 68, 45, 79, 66, 83, 0/
      data (st0014(iyy),iyy= 1, 8) / 68, 65, 84, 69, 45, 79, 66, 83/
      data (st0014(iyy),iyy= 9, 9) / 0/
      data st0015 / 77, 74, 68, 45, 87, 67, 83, 0/
      data st0016 / 77, 74, 68, 45, 79, 66, 83, 0/
      data (st0017(iyy),iyy= 1, 8) / 68, 65, 84, 69, 45, 79, 66, 83/
      data (st0017(iyy),iyy= 9, 9) / 0/
      data st0018 / 77, 74, 68, 45, 87, 67, 83, 0/
      data st0019 / 77, 74, 68, 45, 79, 66, 83, 0/
      data (st0020(iyy),iyy= 1, 8) / 68, 65, 84, 69, 45, 79, 66, 83/
      data (st0020(iyy),iyy= 9, 9) / 0/
         call smark (sp)
         call salloc (atval, 1023 , 2)
         ctype = 0
         lngax = 0
         latax = 0
         wtype = 0
         radecs = 0
         equinx = 1.6d308
         epoch = 1.6d308
         ndim = mwstai (mw, 5 )
         do 110 i = 1, ndim 
            call xerpsh
            call mwgwas (mw, i, st0001, memc(atval), 1023 )
            if (.not.xerpop()) goto 120
               call xstrcy(st0002, memc(atval), 1023 )
120         continue
            axtype = strdic (memc(atval), memc(atval), 1023 , st0003)
            sw0001=(axtype)
            goto 130
140         continue
               ctype = 1
            goto 131
150         continue
               ctype = 2
            goto 131
160         continue
               ctype = 3
            goto 131
170         continue
               ctype = 4
            goto 131
180         continue
               goto 131
130         continue
               if (sw0001.lt.1.or.sw0001.gt.8) goto 180
               goto (140,140,160,160,150,150,170,170),sw0001
131         continue
            sw0002=(axtype)
            goto 190
200         continue
               lngax = i
            goto 191
210         continue
               latax = i
            goto 191
220         continue
               goto 191
190         continue
               if (sw0002.lt.1.or.sw0002.gt.8) goto 220
               goto (200,210,200,210,200,210,200,210),sw0002
191         continue
110      continue
111      continue
         if (.not.(ctype .eq. 0 .or. lngax .eq. 0 .or. latax .eq. 0)) 
     *   goto 230
            call sfree (sp)
            skimws = (-1)
            goto 100
230      continue
         call xerpsh
         call mwgwas (mw, lngax, st0004, memc(atval), 1023 )
         if (xerflg) goto 242
242      if (.not.xerpop()) goto 240
            call xerpsh
            call mwgwas(mw, latax, st0005, memc(atval), 1023 )
            if (.not.xerpop()) goto 250
               call xstrcy(st0006, memc(atval), 1023 )
250         continue
240      continue
         wtype = strdic (memc(atval), memc(atval), 1023 , st0007)
         if (.not.(wtype .eq. 0)) goto 260
            call sfree (sp)
            skimws = (-1)
            goto 100
260      continue
         if (.not.(ctype .eq. 1)) goto 270
            call xerpsh
            equinx = imgetd (im, st0008)
            if (xerflg) goto 282
282         if (.not.xerpop()) goto 280
               call xerpsh
               equinx = imgetd (im, st0009)
               if (xerflg) goto 292
292            if (.not.xerpop()) goto 290
                  equinx = 1.6d308
290            continue
280         continue
            call xerpsh
            call imgstr (im, st0010, memc(atval), 1023 )
            if (xerflg) goto 302
302         if (.not.xerpop()) goto 300
               radecs = 0
               goto 301
300         continue
               call strlwr (memc(atval))
               radecs = strdic (memc(atval), memc(atval), 1023 , st0011)
301         continue
            if (.not.(radecs .eq. 0)) goto 310
               if (.not.(((equinx).eq.1.6d308))) goto 320
                  radecs = 3
                  goto 321
320            continue
               if (.not.(equinx .lt. 1984.0d0)) goto 330
                  radecs = 1
                  goto 331
330            continue
                  radecs = 3
331            continue
321            continue
310         continue
            call xerpsh
            epoch = imgetd (im, st0012)
            if (xerflg) goto 342
342         if (.not.xerpop()) goto 340
               call xerpsh
               epoch = imgetd (im, st0013)
               if (xerflg) goto 352
352            if (.not.xerpop()) goto 350
                  call xerpsh
                  call imgstr (im, st0014, memc(atval), 1023 )
                  if (xerflg) goto 362
362               if (.not.xerpop()) goto 360
                     epoch = 1.6d308
                     goto 361
360               continue
                  if (.not.(dtmdee (memc(atval), year, month, day, hours
     *            , oldfis) .eq. 0)) goto 370
                     call slcadj (year, month, day, epoch, ier)
                     if (.not.(ier .ne. 0)) goto 380
                        epoch = 1.6d308
                        goto 381
380                  continue
                     if (.not.(.not. ((hours).eq.1.6d308) .and. hours .
     *               ge. 0.0d0 .and. hours .le. 24.0d0)) goto 390
                        epoch = epoch + hours / 24.0d0
390                  continue
381                  continue
                     goto 371
370               continue
                     epoch = 1.6d308
371               continue
361               continue
350            continue
340         continue
            sw0003=(radecs)
            goto 400
410         continue
               if (.not.(((equinx).eq.1.6d308))) goto 420
                  equinx = 1950.0d0
420            continue
               if (.not.(((epoch).eq.1.6d308))) goto 430
                  epoch = sleb2d (1950.0d0)
430            continue
            goto 401
440         continue
               if (.not.(((equinx).eq.1.6d308))) goto 450
                  equinx = 2000.0d0
450            continue
               if (.not.(((epoch).eq.1.6d308))) goto 460
                  epoch = slej2d (2000.0d0)
460            continue
            goto 401
470         continue
               equinx = 2000.0d0
               goto 401
400         continue
               if (sw0003.lt.1.or.sw0003.gt.5) goto 401
               goto (410,410,440,440,470),sw0003
401         continue
            if (.not.(((epoch).eq.1.6d308))) goto 480
               call sfree (sp)
               skimws = (-1)
               goto 100
480         continue
270      continue
         if (.not.(ctype .eq. 2)) goto 490
            call xerpsh
            epoch = imgetd (im, st0015)
            if (xerflg) goto 502
502         if (.not.xerpop()) goto 500
               call xerpsh
               epoch = imgetd (im, st0016)
               if (xerflg) goto 512
512            if (.not.xerpop()) goto 510
                  call xerpsh
                  call imgstr (im, st0017, memc(atval), 1023 )
                  if (xerflg) goto 522
522               if (.not.xerpop()) goto 520
                     epoch = 1.6d308
                     goto 521
520               continue
                  if (.not.(dtmdee (memc(atval), year, month, day, hours
     *            , oldfis) .eq. 0)) goto 530
                     call slcadj (year, month, day, epoch, ier)
                     if (.not.(ier .ne. 0)) goto 540
                        epoch = 1.6d308
                        goto 541
540                  continue
                     if (.not.(.not. ((hours).eq.1.6d308) .and. hours .
     *               ge. 0.0d0 .and. hours .le. 24.0d0)) goto 550
                        epoch = epoch + hours / 24.0d0
550                  continue
541                  continue
                     goto 531
530               continue
                     epoch = 1.6d308
531               continue
521               continue
510            continue
500         continue
            if (.not.(((epoch).eq.1.6d308))) goto 560
               call sfree (sp)
               skimws = (-1)
               goto 100
560         continue
490      continue
         if (.not.(ctype .eq. 3 .or. ctype .eq. 4)) goto 570
            call xerpsh
            epoch = imgetd (im, st0018)
            if (xerflg) goto 582
582         if (.not.xerpop()) goto 580
               call xerpsh
               epoch = imgetd (im, st0019)
               if (xerflg) goto 592
592            if (.not.xerpop()) goto 590
                  call xerpsh
                  call imgstr (im, st0020, memc(atval), 1023 )
                  if (xerflg) goto 602
602               if (.not.xerpop()) goto 600
                     epoch = sleb2d (1950.0d0)
                     goto 601
600               continue
                  if (.not.(dtmdee (memc(atval), year, month, day, hours
     *            , oldfis) .eq. 0)) goto 610
                     call slcadj (year, month, day, epoch, ier)
                     if (.not.(ier .ne. 0)) goto 620
                        epoch = sleb2d (1950.0d0)
                        goto 621
620                  continue
                        if (.not.(.not. ((hours).eq.1.6d308) .and. hours
     *                   .ge. 0.0d0 .and. hours .le. 24.0d0)) goto 630
                           epoch = epoch + hours / 24.0d0
630                     continue
621                  continue
                     goto 611
610               continue
                     epoch = sleb2d (1950.0d0)
611               continue
601               continue
590            continue
580         continue
570      continue
         call sfree (sp)
         skimws = (0)
         goto 100
100      return
      end
      subroutine skenws (coo, wcsstr, maxch)
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
      integer coo
      integer maxch
      integer*2 wcsstr(*)
      double precision skstad
      double precision slepj
      double precision slepb
      integer skstai
      integer sw0001,sw0002
      integer*2 st0001(9)
      integer*2 st0002(16)
      integer*2 st0003(18)
      integer*2 st0004(19)
      integer*2 st0005(18)
      integer*2 st0006(21)
      integer*2 st0007(9)
      integer*2 st0008(16)
      integer*2 st0009(16)
      integer*2 st0010(21)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) / 97,112,112, 97,114,101,110,116/
      data (st0001(iyy),iyy= 9, 9) / 0/
      data (st0002(iyy),iyy= 1, 8) / 97,112,112, 97,114,101,110,116/
      data (st0002(iyy),iyy= 9,16) / 32, 74, 37, 48, 46, 56,102, 0/
      data (st0003(iyy),iyy= 1, 8) /102,107, 53, 32, 74, 37, 48, 46/
      data (st0003(iyy),iyy= 9,16) / 51,102, 32, 74, 37, 48, 46, 56/
      data (st0003(iyy),iyy=17,18) /102, 0/
      data (st0004(iyy),iyy= 1, 8) /105, 99,114,115, 32, 74, 37, 48/
      data (st0004(iyy),iyy= 9,16) / 46, 51,102, 32, 74, 37, 48, 46/
      data (st0004(iyy),iyy=17,19) / 56,102, 0/
      data (st0005(iyy),iyy= 1, 8) /102,107, 52, 32, 66, 37, 48, 46/
      data (st0005(iyy),iyy= 9,16) / 51,102, 32, 66, 37, 48, 46, 56/
      data (st0005(iyy),iyy=17,18) /102, 0/
      data (st0006(iyy),iyy= 1, 8) /102,107, 52,110,111,101, 32, 66/
      data (st0006(iyy),iyy= 9,16) / 37, 48, 46, 51,102, 32, 66, 37/
      data (st0006(iyy),iyy=17,21) / 48, 46, 56,102, 0/
      data (st0007(iyy),iyy= 1, 8) /101, 99,108,105,112,116,105, 99/
      data (st0007(iyy),iyy= 9, 9) / 0/
      data (st0008(iyy),iyy= 1, 8) /101, 99,108,105,112,116,105, 99/
      data (st0008(iyy),iyy= 9,16) / 32, 74, 37, 48, 46, 56,102, 0/
      data (st0009(iyy),iyy= 1, 8) /103, 97,108, 97, 99,116,105, 99/
      data (st0009(iyy),iyy= 9,16) / 32, 74, 37, 48, 46, 56,102, 0/
      data (st0010(iyy),iyy= 1, 8) /115,117,112,101,114,103, 97,108/
      data (st0010(iyy),iyy= 9,16) / 97, 99,116,105, 99, 32,106, 37/
      data (st0010(iyy),iyy=17,21) / 48, 46, 56,102, 0/
         sw0001=(skstai (coo, 7))
         goto 110
120      continue
            sw0002=(skstai(coo, 8))
            goto 130
140         continue
               if (.not.(((skstad(coo, 6)).eq.1.6d308))) goto 150
                  call sprinf (wcsstr, maxch, st0001)
                  goto 151
150            continue
                  call sprinf (wcsstr, maxch, st0002)
                  call pargd (slepj(skstad(coo, 6)))
151            continue
            goto 131
160         continue
               call sprinf (wcsstr, maxch, st0003)
               call pargd (skstad(coo, 5))
               call pargd (slepj(skstad(coo, 6)))
            goto 131
170         continue
               call sprinf (wcsstr, maxch, st0004)
               call pargd (skstad(coo, 5))
               call pargd (slepj(skstad(coo, 6)))
            goto 131
180         continue
               call sprinf (wcsstr, maxch, st0005)
               call pargd (skstad(coo, 5))
               call pargd (slepb(skstad(coo, 6)))
            goto 131
190         continue
               call sprinf (wcsstr, maxch, st0006)
               call pargd (skstad(coo, 5))
               call pargd (slepb(skstad(coo, 6)))
            goto 131
200         continue
               wcsstr(1) = 0
               goto 131
130         continue
               if (sw0002.lt.1.or.sw0002.gt.5) goto 200
               goto (180,190,160,170,140),sw0002
131         continue
         goto 111
210      continue
            if (.not.(((skstad(coo, 6)).eq.1.6d308))) goto 220
               call sprinf (wcsstr, maxch, st0007)
               goto 221
220         continue
               call sprinf (wcsstr, maxch, st0008)
               call pargd (slepj(skstad(coo, 6)))
221         continue
         goto 111
230      continue
            call sprinf (wcsstr, maxch, st0009)
            call pargd (slepj(skstad(coo, 6)))
         goto 111
240      continue
            call sprinf (wcsstr, maxch, st0010)
            call pargd (slepj(skstad(coo, 6)))
            goto 111
110      continue
            if (sw0001.lt.1.or.sw0001.gt.4) goto 111
            goto (120,210,230,240),sw0001
111      continue
100      return
      end
      integer function skcopy (cooin)
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
      save
         if (.not.(cooin .eq. 0)) goto 110
            cooout = 0
            goto 111
110      continue
            call xcallc(cooout, (30 + 255 + 1), 10 )
            memd((((cooout)-1)/2+1)) = memd((((cooin)-1)/2+1))
            memd((((cooout+2)-1)/2+1)) = memd((((cooin+2)-1)/2+1))
            memd((((cooout+4)-1)/2+1)) = memd((((cooin+4)-1)/2+1))
            memd((((cooout+6)-1)/2+1)) = memd((((cooin+6)-1)/2+1))
            memd((((cooout+8)-1)/2+1)) = memd((((cooin+8)-1)/2+1))
            memd((((cooout+10)-1)/2+1)) = memd((((cooin+10)-1)/2+1))
            memi(cooout+12) = memi(cooin+12)
            memi(cooout+13) = memi(cooin+13)
            memi(cooout+14) = memi(cooin+14)
            memi(cooout+15) = memi(cooin+15)
            memi(cooout+16) = memi(cooin+16)
            memi(cooout+17) = memi(cooin+17)
            memi(cooout+18) = memi(cooin+18)
            memi(cooout+19) = memi(cooin+19)
            memi(cooout+20) = memi(cooin+20)
            memi(cooout+21) = memi(cooin+21)
            memi(cooout+22) = memi(cooin+22)
            memi(cooout+23) = memi(cooin+23)
            call xstrcy(memc((((cooin+25)-1)*2+1)) , memc((((cooout+25)-
     *      1)*2+1)) , 255 )
111      continue
         skcopy = (cooout)
         goto 100
100      return
      end
      subroutine skcloe (coo)
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
      integer coo
      save
         if (.not.(coo .ne. 0)) goto 110
            call xmfree(coo, 10 )
110      continue
100      return
      end
c     sprinf  sprintf
c     dtmdee  dtm_decode
c     skenws  sk_enwcs
c     skstad  sk_statd
c     radecs  radecsys
c     equinx  equinox
c     skdecs  sk_decwcs
c     skimws  sk_imwcs
c     skstrs  sk_strwcs
c     skdecr  sk_decwstr
c     skstai  sk_stati
c     mwstai  mw_stati
c     skdecm  sk_decim
c     mwgaxp  mw_gaxmap
c     gargwd  gargwrd
c     sleb2d  sl_eb2d
c     mwopem  mw_openim
c     oldfis  oldfits
c     imunmp  imunmap
c     mwgwas  mw_gwattrs
c     skcopy  sk_copy
c     slej2d  sl_ej2d
c     srades  sradecsys
c     slcadj  sl_cadj
c     skcloe  sk_close
c     pargsr  pargstr
c     mwcloe  mw_close
