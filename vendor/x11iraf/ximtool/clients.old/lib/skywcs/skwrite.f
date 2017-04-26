      subroutine skiipt (label, images, mw, coo)
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
      integer*2 label(*)
      integer*2 images(*)
      save
         if (.not.(mw .eq. 0)) goto 110
            call skinpt (label, images, memi(coo+12) , memi(coo+13) , 
     *      memd((((coo+8)-1)/2+1)) , memd((((coo+10)-1)/2+1)) )
            goto 111
110      continue
            call skimpt (label, images, memi(coo+12) , memi(coo+15) , 
     *      memi(coo+16) , memi(coo+14) , memi(coo+19) , memi(coo+13) , 
     *      memd((((coo+8)-1)/2+1)) , memd((((coo+10)-1)/2+1)) )
111      continue
100      return
      end
      subroutine skiiwe (fd, label, images, mw, coo)
      integer fd
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
      integer*2 label(*)
      integer*2 images(*)
      save
         if (.not.(mw .eq. 0)) goto 110
            call skinwe (fd, label, images, memi(coo+12) , memi(coo+13) 
     *      , memd((((coo+8)-1)/2+1)) , memd((((coo+10)-1)/2+1)) )
            goto 111
110      continue
            call skimwe (fd, label, images, memi(coo+12) , memi(coo+15) 
     *      , memi(coo+16) , memi(coo+14) , memi(coo+19) , memi(coo+13) 
     *      , memd((((coo+8)-1)/2+1)) , memd((((coo+10)-1)/2+1)) )
111      continue
100      return
      end
      subroutine skinpt (label, system, ctype, radecs, equinx, epoch)
      integer ctype
      integer radecs
      double precision equinx
      double precision epoch
      integer*2 label(*)
      integer*2 system(*)
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
      integer radecr
      double precision slepj
      double precision slepb
      integer skwrdr
      integer sw0001,sw0002
      integer*2 st0001(30)
      integer*2 st0002(4)
      integer*2 st0003(36)
      integer*2 st0004(37)
      integer*2 st0005(46)
      integer*2 st0006(46)
      integer*2 st0007(31)
      integer*2 st0008(37)
      integer*2 st0009(31)
      integer*2 st0010(37)
      integer*2 st0011(36)
      integer*2 st0012(37)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /124,102,107, 52,124,102,107, 52/
      data (st0001(iyy),iyy= 9,16) / 45,110,111, 45,101,124,102,107/
      data (st0001(iyy),iyy=17,24) / 53,124,105, 99,114,115,124,103/
      data (st0001(iyy),iyy=25,30) / 97,112,112,116,124, 0/
      data st0002 / 70, 75, 53, 0/
      data (st0003(iyy),iyy= 1, 8) / 37,115, 58, 32, 37,115, 32, 32/
      data (st0003(iyy),iyy= 9,16) / 67,111,111,114,100,105,110, 97/
      data (st0003(iyy),iyy=17,24) /116,101,115, 58, 32,101,113,117/
      data (st0003(iyy),iyy=25,32) / 97,116,111,114,105, 97,108, 32/
      data (st0003(iyy),iyy=33,36) / 37,115, 10, 0/
      data (st0004(iyy),iyy= 1, 8) / 32, 32, 32, 32, 77, 74, 68, 58/
      data (st0004(iyy),iyy= 9,16) / 32, 37, 48, 46, 53,102, 32, 69/
      data (st0004(iyy),iyy=17,24) /112,111, 99,104, 58, 32, 74, 37/
      data (st0004(iyy),iyy=25,32) / 48, 46, 56,102, 32, 66, 37, 48/
      data (st0004(iyy),iyy=33,37) / 46, 56,102, 10, 0/
      data (st0005(iyy),iyy= 1, 8) / 32, 32, 32, 32, 69,113,117,105/
      data (st0005(iyy),iyy= 9,16) /110,111,120, 58, 32, 74, 37, 48/
      data (st0005(iyy),iyy=17,24) / 46, 51,102, 32, 69,112,111, 99/
      data (st0005(iyy),iyy=25,32) /104, 58, 32, 74, 37, 48, 46, 56/
      data (st0005(iyy),iyy=33,40) /102, 32, 77, 74, 68, 58, 32, 37/
      data (st0005(iyy),iyy=41,46) / 48, 46, 53,102, 10, 0/
      data (st0006(iyy),iyy= 1, 8) / 32, 32, 32, 32, 69,113,117,105/
      data (st0006(iyy),iyy= 9,16) /110,111,120, 58, 32, 66, 37, 48/
      data (st0006(iyy),iyy=17,24) / 46, 51,102, 32, 69,112,111, 99/
      data (st0006(iyy),iyy=25,32) /104, 58, 32, 66, 37, 48, 46, 56/
      data (st0006(iyy),iyy=33,40) /102, 32, 77, 74, 68, 58, 32, 37/
      data (st0006(iyy),iyy=41,46) / 48, 46, 53,102, 10, 0/
      data (st0007(iyy),iyy= 1, 8) / 37,115, 58, 32, 37,115, 32, 32/
      data (st0007(iyy),iyy= 9,16) / 67,111,111,114,100,105,110, 97/
      data (st0007(iyy),iyy=17,24) /116,101,115, 58, 32,101, 99,108/
      data (st0007(iyy),iyy=25,31) /105,112,116,105, 99, 10, 0/
      data (st0008(iyy),iyy= 1, 8) / 32, 32, 32, 32, 77, 74, 68, 58/
      data (st0008(iyy),iyy= 9,16) / 32, 37, 48, 46, 53,102, 32, 69/
      data (st0008(iyy),iyy=17,24) /112,111, 99,104, 58, 32, 74, 37/
      data (st0008(iyy),iyy=25,32) / 48, 46, 56,102, 32, 66, 37, 48/
      data (st0008(iyy),iyy=33,37) / 46, 56,102, 10, 0/
      data (st0009(iyy),iyy= 1, 8) / 37,115, 58, 32, 37,115, 32, 32/
      data (st0009(iyy),iyy= 9,16) / 67,111,111,114,100,105,110, 97/
      data (st0009(iyy),iyy=17,24) /116,101,115, 58, 32,103, 97,108/
      data (st0009(iyy),iyy=25,31) / 97, 99,116,105, 99, 10, 0/
      data (st0010(iyy),iyy= 1, 8) / 32, 32, 32, 32, 77, 74, 68, 58/
      data (st0010(iyy),iyy= 9,16) / 32, 37, 48, 46, 53,102, 32, 69/
      data (st0010(iyy),iyy=17,24) /112,111, 99,104, 58, 32, 74, 37/
      data (st0010(iyy),iyy=25,32) / 48, 46, 56,102, 32, 66, 37, 48/
      data (st0010(iyy),iyy=33,37) / 46, 56,102, 10, 0/
      data (st0011(iyy),iyy= 1, 8) / 37,115, 58, 32, 37,115, 32, 32/
      data (st0011(iyy),iyy= 9,16) / 67,111,111,114,100,105,110, 97/
      data (st0011(iyy),iyy=17,24) /116,101,115, 58, 32,115,117,112/
      data (st0011(iyy),iyy=25,32) /101,114,103, 97,108, 97, 99,116/
      data (st0011(iyy),iyy=33,36) /105, 99, 10, 0/
      data (st0012(iyy),iyy= 1, 8) / 32, 32, 32, 32, 77, 74, 68, 58/
      data (st0012(iyy),iyy= 9,16) / 32, 37, 48, 46, 53,102, 32, 69/
      data (st0012(iyy),iyy=17,24) /112,111, 99,104, 58, 32, 74, 37/
      data (st0012(iyy),iyy=25,32) / 48, 46, 56,102, 32, 66, 37, 48/
      data (st0012(iyy),iyy=33,37) / 46, 56,102, 10, 0/
         call smark (sp)
         call salloc (radecr, 255 , 2)
         sw0001=(ctype)
         goto 110
120      continue
            if (.not.(skwrdr (radecs, memc(radecr), 255 , st0001) .le. 0
     *      )) goto 130
               call xstrcy(st0002, memc(radecr), 255 )
130         continue
            call strupr (memc(radecr))
            call xprinf(st0003)
            call pargsr (label)
            call pargsr (system)
            call pargsr (memc(radecr))
            sw0002=(radecs)
            goto 140
150         continue
               call xprinf(st0004)
               call pargd (epoch)
               if (.not.(((epoch).eq.1.6d308))) goto 160
                  call pargd (1.6d308)
                  call pargd (1.6d308)
                  goto 161
160            continue
                  call pargd (slepj (epoch))
                  call pargd (slepb (epoch))
161            continue
            goto 141
170         continue
               call xprinf(st0005)
               call pargd (equinx)
               call pargd (slepj(epoch))
               call pargd (epoch)
            goto 141
180         continue
               call xprinf(st0006)
               call pargd (equinx)
               call pargd (slepb(epoch))
               call pargd (epoch)
               goto 141
140         continue
               sw0002=sw0002-2
               if (sw0002.lt.1.or.sw0002.gt.3) goto 180
               goto (170,170,150),sw0002
141         continue
         goto 111
190      continue
            call xprinf(st0007)
            call pargsr (label)
            call pargsr (system)
            call xprinf(st0008)
            call pargd (epoch)
            if (.not.(((epoch).eq.1.6d308))) goto 200
               call pargd (1.6d308)
               call pargd (1.6d308)
               goto 201
200         continue
               call pargd (slepj(epoch))
               call pargd (slepb(epoch))
201         continue
         goto 111
210      continue
            call xprinf(st0009)
            call pargsr (label)
            call pargsr (system)
            call xprinf(st0010)
            call pargd (epoch)
            call pargd (slepj (epoch))
            call pargd (slepb (epoch))
         goto 111
220      continue
            call xprinf(st0011)
            call pargsr (label)
            call pargsr (system)
            call xprinf(st0012)
            call pargd (epoch)
            call pargd (slepj (epoch))
            call pargd (slepb (epoch))
            goto 111
110      continue
            if (sw0001.lt.1.or.sw0001.gt.4) goto 111
            goto (120,190,210,220),sw0001
111      continue
         call sfree (sp)
100      return
      end
      subroutine skinwe (fd, label, system, ctype, radecs, equinx, epoch
     *)
      integer fd
      integer ctype
      integer radecs
      double precision equinx
      double precision epoch
      integer*2 label(*)
      integer*2 system(*)
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
      integer radecr
      double precision slepj
      double precision slepb
      integer skwrdr
      integer sw0001,sw0002
      integer*2 st0001(30)
      integer*2 st0002(4)
      integer*2 st0003(38)
      integer*2 st0004(39)
      integer*2 st0005(48)
      integer*2 st0006(48)
      integer*2 st0007(33)
      integer*2 st0008(39)
      integer*2 st0009(33)
      integer*2 st0010(39)
      integer*2 st0011(38)
      integer*2 st0012(39)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /124,102,107, 52,124,102,107, 52/
      data (st0001(iyy),iyy= 9,16) / 45,110,111, 45,101,124,102,107/
      data (st0001(iyy),iyy=17,24) / 53,124,105, 99,114,115,124,103/
      data (st0001(iyy),iyy=25,30) / 97,112,112,116,124, 0/
      data st0002 / 70, 75, 53, 0/
      data (st0003(iyy),iyy= 1, 8) / 35, 32, 37,115, 58, 32, 37,115/
      data (st0003(iyy),iyy= 9,16) / 32, 32, 67,111,111,114,100,105/
      data (st0003(iyy),iyy=17,24) /110, 97,116,101,115, 58, 32,101/
      data (st0003(iyy),iyy=25,32) /113,117, 97,116,111,114,105, 97/
      data (st0003(iyy),iyy=33,38) /108, 32, 37,115, 10, 0/
      data (st0004(iyy),iyy= 1, 8) / 35, 32, 32, 32, 32, 32, 77, 74/
      data (st0004(iyy),iyy= 9,16) / 68, 58, 32, 37, 48, 46, 53,102/
      data (st0004(iyy),iyy=17,24) / 32, 69,112,111, 99,104, 58, 32/
      data (st0004(iyy),iyy=25,32) / 74, 37, 48, 46, 56,102, 32, 66/
      data (st0004(iyy),iyy=33,39) / 37, 48, 46, 56,102, 10, 0/
      data (st0005(iyy),iyy= 1, 8) / 35, 32, 32, 32, 32, 32, 69,113/
      data (st0005(iyy),iyy= 9,16) /117,105,110,111,120, 58, 32, 74/
      data (st0005(iyy),iyy=17,24) / 37, 48, 46, 51,102, 32, 69,112/
      data (st0005(iyy),iyy=25,32) /111, 99,104, 58, 32, 74, 37, 48/
      data (st0005(iyy),iyy=33,40) / 46, 56,102, 32, 77, 74, 68, 58/
      data (st0005(iyy),iyy=41,48) / 32, 37, 48, 46, 53,102, 10, 0/
      data (st0006(iyy),iyy= 1, 8) / 35, 32, 32, 32, 32, 32, 69,113/
      data (st0006(iyy),iyy= 9,16) /117,105,110,111,120, 58, 32, 66/
      data (st0006(iyy),iyy=17,24) / 37, 48, 46, 51,102, 32, 69,112/
      data (st0006(iyy),iyy=25,32) /111, 99,104, 58, 32, 66, 37, 48/
      data (st0006(iyy),iyy=33,40) / 46, 56,102, 32, 77, 74, 68, 58/
      data (st0006(iyy),iyy=41,48) / 32, 37, 48, 46, 53,102, 10, 0/
      data (st0007(iyy),iyy= 1, 8) / 35, 32, 37,115, 58, 32, 37,115/
      data (st0007(iyy),iyy= 9,16) / 32, 32, 67,111,111,114,100,105/
      data (st0007(iyy),iyy=17,24) /110, 97,116,101,115, 58, 32,101/
      data (st0007(iyy),iyy=25,32) / 99,108,105,112,116,105, 99, 10/
      data (st0007(iyy),iyy=33,33) / 0/
      data (st0008(iyy),iyy= 1, 8) / 35, 32, 32, 32, 32, 32, 77, 74/
      data (st0008(iyy),iyy= 9,16) / 68, 58, 32, 37, 48, 46, 53,102/
      data (st0008(iyy),iyy=17,24) / 32, 69,112,111, 99,104, 58, 32/
      data (st0008(iyy),iyy=25,32) / 74, 37, 48, 46, 56,102, 32, 66/
      data (st0008(iyy),iyy=33,39) / 37, 48, 46, 56,102, 10, 0/
      data (st0009(iyy),iyy= 1, 8) / 35, 32, 37,115, 58, 32, 37,115/
      data (st0009(iyy),iyy= 9,16) / 32, 32, 67,111,111,114,100,105/
      data (st0009(iyy),iyy=17,24) /110, 97,116,101,115, 58, 32,103/
      data (st0009(iyy),iyy=25,32) / 97,108, 97, 99,116,105, 99, 10/
      data (st0009(iyy),iyy=33,33) / 0/
      data (st0010(iyy),iyy= 1, 8) / 35, 32, 32, 32, 32, 32, 77, 74/
      data (st0010(iyy),iyy= 9,16) / 68, 58, 32, 37, 48, 46, 53,102/
      data (st0010(iyy),iyy=17,24) / 32, 69,112,111, 99,104, 58, 32/
      data (st0010(iyy),iyy=25,32) / 74, 37, 48, 46, 56,102, 32, 66/
      data (st0010(iyy),iyy=33,39) / 37, 48, 46, 56,102, 10, 0/
      data (st0011(iyy),iyy= 1, 8) / 35, 32, 37,115, 58, 32, 37,115/
      data (st0011(iyy),iyy= 9,16) / 32, 32, 67,111,111,114,100,105/
      data (st0011(iyy),iyy=17,24) /110, 97,116,101,115, 58, 32,115/
      data (st0011(iyy),iyy=25,32) /117,112,101,114,103, 97,108, 97/
      data (st0011(iyy),iyy=33,38) / 99,116,105, 99, 10, 0/
      data (st0012(iyy),iyy= 1, 8) / 35, 32, 32, 32, 32, 32, 77, 74/
      data (st0012(iyy),iyy= 9,16) / 68, 58, 32, 37, 48, 46, 53,102/
      data (st0012(iyy),iyy=17,24) / 32, 69,112,111, 99,104, 58, 32/
      data (st0012(iyy),iyy=25,32) / 74, 37, 48, 46, 56,102, 32, 66/
      data (st0012(iyy),iyy=33,39) / 37, 48, 46, 56,102, 10, 0/
         call smark (sp)
         call salloc (radecr, 255 , 2)
         sw0001=(ctype)
         goto 110
120      continue
            if (.not.(skwrdr (radecs, memc(radecr), 255 , st0001) .le. 0
     *      )) goto 130
               call xstrcy(st0002, memc(radecr), 255 )
130         continue
            call strupr (memc(radecr))
            call fprinf (fd, st0003)
            call pargsr (label)
            call pargsr (system)
            call pargsr (memc(radecr))
            sw0002=(radecs)
            goto 140
150         continue
               call fprinf (fd, st0004)
               call pargd (epoch)
               if (.not.(((epoch).eq.1.6d308))) goto 160
                  call pargd (1.6d308)
                  call pargd (1.6d308)
                  goto 161
160            continue
                  call pargd (slepj(epoch))
                  call pargd (slepb(epoch))
161            continue
            goto 141
170         continue
               call fprinf (fd, st0005)
               call pargd (equinx)
               call pargd (slepj(epoch))
               call pargd (epoch)
            goto 141
180         continue
               call fprinf (fd, st0006)
               call pargd (equinx)
               call pargd (slepb(epoch))
               call pargd (epoch)
               goto 141
140         continue
               sw0002=sw0002-2
               if (sw0002.lt.1.or.sw0002.gt.3) goto 180
               goto (170,170,150),sw0002
141         continue
         goto 111
190      continue
            call fprinf (fd, st0007)
            call pargsr (label)
            call pargsr (system)
            call fprinf (fd, st0008)
            call pargd (epoch)
            if (.not.(((epoch).eq.1.6d308))) goto 200
               call pargd (1.6d308)
               call pargd (1.6d308)
               goto 201
200         continue
               call pargd (slepj(epoch))
               call pargd (slepb(epoch))
201         continue
         goto 111
210      continue
            call fprinf (fd, st0009)
            call pargsr (label)
            call pargsr (system)
            call fprinf (fd, st0010)
            call pargd (epoch)
            call pargd (slepj(epoch))
            call pargd (slepb(epoch))
         goto 111
220      continue
            call fprinf (fd, st0011)
            call pargsr (label)
            call pargsr (system)
            call fprinf (fd, st0012)
            call pargd (epoch)
            call pargd (slepj(epoch))
            call pargd (slepb(epoch))
            goto 111
110      continue
            if (sw0001.lt.1.or.sw0001.gt.4) goto 111
            goto (120,190,210,220),sw0001
111      continue
         call sfree (sp)
100      return
      end
      subroutine skimpt (label, images, ctype, lngax, latax, wtype, 
     *ptype, radecs, equinx, epoch)
      integer ctype
      integer lngax
      integer latax
      integer wtype
      integer ptype
      integer radecs
      double precision equinx
      double precision epoch
      integer*2 label(*)
      integer*2 images(*)
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
      integer imname
      integer projsr
      integer wcsstr
      integer radecr
      double precision slepj
      double precision slepb
      integer skwrdr
      integer sw0001,sw0002
      integer*2 st0001(114)
      integer*2 st0002(7)
      integer*2 st0003(28)
      integer*2 st0004(6)
      integer*2 st0005(30)
      integer*2 st0006(4)
      integer*2 st0007(47)
      integer*2 st0008(32)
      integer*2 st0009(37)
      integer*2 st0010(48)
      integer*2 st0011(30)
      integer*2 st0012(48)
      integer*2 st0013(30)
      integer*2 st0014(51)
      integer*2 st0015(27)
      integer*2 st0016(37)
      integer*2 st0017(51)
      integer*2 st0018(27)
      integer*2 st0019(38)
      integer*2 st0020(51)
      integer*2 st0021(32)
      integer*2 st0022(37)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /124,108,105,110,124, 97,122,112/
      data (st0001(iyy),iyy= 9,16) /124,116, 97,110,124,115,105,110/
      data (st0001(iyy),iyy=17,24) /124,115,116,103,124, 97,114, 99/
      data (st0001(iyy),iyy=25,32) /124,122,112,110,124,122,101, 97/
      data (st0001(iyy),iyy=33,40) /124, 97,105,114,124, 99,121,112/
      data (st0001(iyy),iyy=41,48) /124, 99, 97,114,124,109,101,114/
      data (st0001(iyy),iyy=49,56) /124, 99,101, 97,124, 99,111,112/
      data (st0001(iyy),iyy=57,64) /124, 99,111,100,124, 99,111,101/
      data (st0001(iyy),iyy=65,72) /124, 99,111,111,124, 98,111,110/
      data (st0001(iyy),iyy=73,80) /124,112, 99,111,124,103,108,115/
      data (st0001(iyy),iyy=81,88) /124,112, 97,114,124, 97,105,116/
      data (st0001(iyy),iyy=89,96) /124,109,111,108,124, 99,115, 99/
      data (st0001(iyy),iyy=97,104) /124,113,115, 99,124,116,115, 99/
      data (st0001(iyy),iyy=105,112) /124,116,110,120,124,122,112,120/
      data (st0001(iyy),iyy=113,114) /124, 0/
      data st0002 /108,105,110,101, 97,114, 0/
      data (st0003(iyy),iyy= 1, 8) /124,108,111,103,105, 99, 97,108/
      data (st0003(iyy),iyy= 9,16) /124,116,118,124,112,104,121,115/
      data (st0003(iyy),iyy=17,24) /105, 99, 97,108,124,119,111,114/
      data (st0003(iyy),iyy=25,28) /108,100,124, 0/
      data st0004 /119,111,114,108,100, 0/
      data (st0005(iyy),iyy= 1, 8) /124,102,107, 52,124,102,107, 52/
      data (st0005(iyy),iyy= 9,16) / 45,110,111, 45,101,124,102,107/
      data (st0005(iyy),iyy=17,24) / 53,124,105, 99,114,115,124,103/
      data (st0005(iyy),iyy=25,30) / 97,112,112,116,124, 0/
      data st0006 / 70, 75, 53, 0/
      data (st0007(iyy),iyy= 1, 8) / 37,115, 58, 32, 37,115, 32, 37/
      data (st0007(iyy),iyy= 9,16) /115, 32, 32, 80,114,111,106,101/
      data (st0007(iyy),iyy=17,24) / 99,116,105,111,110, 58, 32, 37/
      data (st0007(iyy),iyy=25,32) /115, 32, 32, 82, 97, 47, 68,101/
      data (st0007(iyy),iyy=33,40) / 99, 32, 97,120,101,115, 58, 32/
      data (st0007(iyy),iyy=41,47) / 37,100, 47, 37,100, 10, 0/
      data (st0008(iyy),iyy= 1, 8) / 32, 32, 32, 32, 67,111,111,114/
      data (st0008(iyy),iyy= 9,16) /100,105,110, 97,116,101,115, 58/
      data (st0008(iyy),iyy=17,24) / 32,101,113,117, 97,116,111,114/
      data (st0008(iyy),iyy=25,32) /105, 97,108, 32, 37,115, 10, 0/
      data (st0009(iyy),iyy= 1, 8) / 32, 32, 32, 32, 77, 74, 68, 58/
      data (st0009(iyy),iyy= 9,16) / 32, 37, 48, 46, 53,102, 32, 69/
      data (st0009(iyy),iyy=17,24) /112,111, 99,104, 58, 32, 74, 37/
      data (st0009(iyy),iyy=25,32) / 48, 46, 56,102, 32, 66, 37, 48/
      data (st0009(iyy),iyy=33,37) / 46, 56,102, 10, 0/
      data (st0010(iyy),iyy= 1, 8) / 32, 32, 32, 32, 67,111,111,114/
      data (st0010(iyy),iyy= 9,16) /100,105,110, 97,116,101,115, 58/
      data (st0010(iyy),iyy=17,24) / 32,101,113,117, 97,116,111,114/
      data (st0010(iyy),iyy=25,32) /105, 97,108, 32, 37,115, 32, 69/
      data (st0010(iyy),iyy=33,40) /113,117,105,110,111,120, 58, 32/
      data (st0010(iyy),iyy=41,48) / 74, 37, 48, 46, 51,102, 10, 0/
      data (st0011(iyy),iyy= 1, 8) / 32, 32, 32, 32, 69,112,111, 99/
      data (st0011(iyy),iyy= 9,16) /104, 58, 32, 74, 37, 48, 46, 56/
      data (st0011(iyy),iyy=17,24) /102, 32, 77, 74, 68, 58, 32, 37/
      data (st0011(iyy),iyy=25,30) / 48, 46, 53,102, 10, 0/
      data (st0012(iyy),iyy= 1, 8) / 32, 32, 32, 32, 67,111,111,114/
      data (st0012(iyy),iyy= 9,16) /100,105,110, 97,116,101,115, 58/
      data (st0012(iyy),iyy=17,24) / 32,101,113,117, 97,116,111,114/
      data (st0012(iyy),iyy=25,32) /105, 97,108, 32, 37,115, 32, 69/
      data (st0012(iyy),iyy=33,40) /113,117,105,110,111,120, 58, 32/
      data (st0012(iyy),iyy=41,48) / 66, 37, 48, 46, 51,102, 10, 0/
      data (st0013(iyy),iyy= 1, 8) / 32, 32, 32, 32, 69,112,111, 99/
      data (st0013(iyy),iyy= 9,16) /104, 58, 32, 66, 37, 48, 46, 56/
      data (st0013(iyy),iyy=17,24) /102, 32, 77, 74, 68, 58, 32, 37/
      data (st0013(iyy),iyy=25,30) / 48, 46, 53,102, 10, 0/
      data (st0014(iyy),iyy= 1, 8) / 37,115, 58, 32, 37,115, 32, 37/
      data (st0014(iyy),iyy= 9,16) /115, 32, 32, 80,114,111,106,101/
      data (st0014(iyy),iyy=17,24) / 99,116,105,111,110, 58, 32, 37/
      data (st0014(iyy),iyy=25,32) /115, 32, 32, 69,108,111,110,103/
      data (st0014(iyy),iyy=33,40) / 47, 69,108, 97,116, 32, 97,120/
      data (st0014(iyy),iyy=41,48) /101,115, 58, 32, 37,100, 47, 37/
      data (st0014(iyy),iyy=49,51) /100, 10, 0/
      data (st0015(iyy),iyy= 1, 8) / 32, 32, 32, 32, 67,111,111,114/
      data (st0015(iyy),iyy= 9,16) /100,105,110, 97,116,101,115, 58/
      data (st0015(iyy),iyy=17,24) / 32,101, 99,108,105,112,116,105/
      data (st0015(iyy),iyy=25,27) / 99, 10, 0/
      data (st0016(iyy),iyy= 1, 8) / 32, 32, 32, 32, 77, 74, 68, 58/
      data (st0016(iyy),iyy= 9,16) / 32, 37, 48, 46, 53,102, 32, 69/
      data (st0016(iyy),iyy=17,24) /112,111, 99,104, 58, 32, 74, 37/
      data (st0016(iyy),iyy=25,32) / 48, 46, 56,102, 32, 66, 37, 48/
      data (st0016(iyy),iyy=33,37) / 46, 56,102, 10, 0/
      data (st0017(iyy),iyy= 1, 8) / 37,115, 58, 32, 37,115, 32, 37/
      data (st0017(iyy),iyy= 9,16) /115, 32, 32, 80,114,111,106,101/
      data (st0017(iyy),iyy=17,24) / 99,116,105,111,110, 58, 32, 37/
      data (st0017(iyy),iyy=25,32) /115, 32, 32, 71,108,111,110,103/
      data (st0017(iyy),iyy=33,40) / 47, 71,108, 97,116, 32, 97,120/
      data (st0017(iyy),iyy=41,48) /101,115, 58, 32, 37,100, 47, 37/
      data (st0017(iyy),iyy=49,51) /100, 10, 0/
      data (st0018(iyy),iyy= 1, 8) / 32, 32, 32, 32, 67,111,111,114/
      data (st0018(iyy),iyy= 9,16) /100,105,110, 97,116,101,115, 58/
      data (st0018(iyy),iyy=17,24) / 32,103, 97,108, 97, 99,116,105/
      data (st0018(iyy),iyy=25,27) / 99, 10, 0/
      data (st0019(iyy),iyy= 1, 8) / 32, 32, 32, 32, 77, 74, 68, 58/
      data (st0019(iyy),iyy= 9,16) / 32, 37, 48, 46, 53,102, 32, 32/
      data (st0019(iyy),iyy=17,24) / 69,112,111, 99,104, 58, 32, 74/
      data (st0019(iyy),iyy=25,32) / 37, 48, 46, 56,102, 32, 66, 37/
      data (st0019(iyy),iyy=33,38) / 48, 46, 56,102, 10, 0/
      data (st0020(iyy),iyy= 1, 8) / 37,115, 58, 32, 37,115, 32, 37/
      data (st0020(iyy),iyy= 9,16) /115, 32, 32, 80,114,111,106,101/
      data (st0020(iyy),iyy=17,24) / 99,116,105,111,110, 58, 32, 37/
      data (st0020(iyy),iyy=25,32) /115, 32, 32, 83,108,111,110,103/
      data (st0020(iyy),iyy=33,40) / 47, 83,108, 97,116, 32, 97,120/
      data (st0020(iyy),iyy=41,48) /101,115, 58, 32, 37,100, 47, 37/
      data (st0020(iyy),iyy=49,51) /100, 10, 0/
      data (st0021(iyy),iyy= 1, 8) / 32, 32, 32, 32, 67,111,111,114/
      data (st0021(iyy),iyy= 9,16) /100,105,110, 97,116,101,115, 58/
      data (st0021(iyy),iyy=17,24) / 32,115,117,112,101,114,103, 97/
      data (st0021(iyy),iyy=25,32) /108, 97, 99,116,105, 99, 10, 0/
      data (st0022(iyy),iyy= 1, 8) / 32, 32, 32, 32, 77, 74, 68, 58/
      data (st0022(iyy),iyy= 9,16) / 32, 37, 48, 46, 53,102, 32, 69/
      data (st0022(iyy),iyy=17,24) /112,111, 99,104, 58, 32, 74, 37/
      data (st0022(iyy),iyy=25,32) / 48, 46, 56,102, 32, 66, 37, 48/
      data (st0022(iyy),iyy=33,37) / 46, 56,102, 10, 0/
         call smark (sp)
         call salloc (imname, 255 , 2)
         call salloc (projsr, 255 , 2)
         call salloc (wcsstr, 255 , 2)
         call salloc (radecr, 255 , 2)
         call sscan (images)
         call gargwd (memc(imname), 255 )
         if (.not.(skwrdr (wtype, memc(projsr), 255 , st0001) .le. 0)) 
     *   goto 110
            call xstrcy(st0002, memc(projsr), 255 )
110      continue
         call strupr (memc(projsr))
         if (.not.(skwrdr (ptype, memc(wcsstr), 255 , st0003) .le. 0)) 
     *   goto 120
            call xstrcy(st0004, memc(wcsstr), 255 )
120      continue
         call strlwr (memc(wcsstr))
         sw0001=(ctype)
         goto 130
140      continue
            if (.not.(skwrdr (radecs, memc(radecr), 255 , st0005) .le. 0
     *      )) goto 150
               call xstrcy(st0006, memc(radecr), 255 )
150         continue
            call strupr (memc(radecr))
            call xprinf( st0007)
            call pargsr (label)
            call pargsr (memc(imname))
            call pargsr (memc(wcsstr))
            call pargsr (memc(projsr))
            call pargi (lngax)
            call pargi (latax)
            sw0002=(radecs)
            goto 160
170         continue
               call xprinf(st0008)
               call pargsr (memc(radecr))
               call xprinf(st0009)
               call pargd (epoch)
               if (.not.(((epoch).eq.1.6d308))) goto 180
                  call pargd (1.6d308)
                  call pargd (1.6d308)
                  goto 181
180            continue
                  call pargd (slepj(epoch))
                  call pargd (slepb(epoch))
181            continue
            goto 161
190         continue
               call xprinf(st0010)
               call pargsr (memc(radecr))
               call pargd (equinx)
               call xprinf(st0011)
               call pargd (slepj (epoch))
               call pargd (epoch)
            goto 161
200         continue
               call xprinf(st0012)
               call pargsr (memc(radecr))
               call pargd (equinx)
               call xprinf(st0013)
               call pargd (slepb (epoch))
               call pargd (epoch)
               goto 161
160         continue
               sw0002=sw0002-2
               if (sw0002.lt.1.or.sw0002.gt.3) goto 200
               goto (190,190,170),sw0002
161         continue
         goto 131
210      continue
            call xprinf( st0014)
            call pargsr (label)
            call pargsr (memc(imname))
            call pargsr (memc(wcsstr))
            call pargsr (memc(projsr))
            call pargi (lngax)
            call pargi (latax)
            call xprinf(st0015)
            call xprinf(st0016)
            call pargd (epoch)
            if (.not.(((epoch).eq.1.6d308))) goto 220
               call pargd (1.6d308)
               call pargd (1.6d308)
               goto 221
220         continue
               call pargd (slepj(epoch))
               call pargd (slepb(epoch))
221         continue
         goto 131
230      continue
            call xprinf( st0017)
            call pargsr (label)
            call pargsr (memc(imname))
            call pargsr (memc(wcsstr))
            call pargsr (memc(projsr))
            call pargi (lngax)
            call pargi (latax)
            call xprinf(st0018)
            call xprinf(st0019)
            call pargd (epoch)
            call pargd (slepj (epoch))
            call pargd (slepb (epoch))
         goto 131
240      continue
            call xprinf( st0020)
            call pargsr (label)
            call pargsr (memc(imname))
            call pargsr (memc(wcsstr))
            call pargsr (memc(projsr))
            call pargi (lngax)
            call pargi (latax)
            call xprinf(st0021)
            call xprinf(st0022)
            call pargd (epoch)
            call pargd (slepj (epoch))
            call pargd (slepb (epoch))
            goto 131
130      continue
            if (sw0001.lt.1.or.sw0001.gt.4) goto 131
            goto (140,210,230,240),sw0001
131      continue
         call sfree (sp)
100      return
      end
      subroutine skimwe (fd, label, images, ctype, lngax, latax, wtype, 
     *ptype, radecs, equinx, epoch)
      integer fd
      integer ctype
      integer lngax
      integer latax
      integer wtype
      integer ptype
      integer radecs
      double precision equinx
      double precision epoch
      integer*2 label(*)
      integer*2 images(*)
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
      integer imname
      integer projsr
      integer wcsstr
      integer radecr
      double precision slepj
      double precision slepb
      integer skwrdr
      integer sw0001,sw0002
      integer*2 st0001(114)
      integer*2 st0002(7)
      integer*2 st0003(28)
      integer*2 st0004(6)
      integer*2 st0005(30)
      integer*2 st0006(4)
      integer*2 st0007(49)
      integer*2 st0008(34)
      integer*2 st0009(39)
      integer*2 st0010(50)
      integer*2 st0011(32)
      integer*2 st0012(50)
      integer*2 st0013(32)
      integer*2 st0014(53)
      integer*2 st0015(29)
      integer*2 st0016(40)
      integer*2 st0017(53)
      integer*2 st0018(29)
      integer*2 st0019(39)
      integer*2 st0020(53)
      integer*2 st0021(34)
      integer*2 st0022(39)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /124,108,105,110,124, 97,122,112/
      data (st0001(iyy),iyy= 9,16) /124,116, 97,110,124,115,105,110/
      data (st0001(iyy),iyy=17,24) /124,115,116,103,124, 97,114, 99/
      data (st0001(iyy),iyy=25,32) /124,122,112,110,124,122,101, 97/
      data (st0001(iyy),iyy=33,40) /124, 97,105,114,124, 99,121,112/
      data (st0001(iyy),iyy=41,48) /124, 99, 97,114,124,109,101,114/
      data (st0001(iyy),iyy=49,56) /124, 99,101, 97,124, 99,111,112/
      data (st0001(iyy),iyy=57,64) /124, 99,111,100,124, 99,111,101/
      data (st0001(iyy),iyy=65,72) /124, 99,111,111,124, 98,111,110/
      data (st0001(iyy),iyy=73,80) /124,112, 99,111,124,103,108,115/
      data (st0001(iyy),iyy=81,88) /124,112, 97,114,124, 97,105,116/
      data (st0001(iyy),iyy=89,96) /124,109,111,108,124, 99,115, 99/
      data (st0001(iyy),iyy=97,104) /124,113,115, 99,124,116,115, 99/
      data (st0001(iyy),iyy=105,112) /124,116,110,120,124,122,112,120/
      data (st0001(iyy),iyy=113,114) /124, 0/
      data st0002 /108,105,110,101, 97,114, 0/
      data (st0003(iyy),iyy= 1, 8) /124,108,111,103,105, 99, 97,108/
      data (st0003(iyy),iyy= 9,16) /124,116,118,124,112,104,121,115/
      data (st0003(iyy),iyy=17,24) /105, 99, 97,108,124,119,111,114/
      data (st0003(iyy),iyy=25,28) /108,100,124, 0/
      data st0004 /119,111,114,108,100, 0/
      data (st0005(iyy),iyy= 1, 8) /124,102,107, 52,124,102,107, 52/
      data (st0005(iyy),iyy= 9,16) / 45,110,111, 45,101,124,102,107/
      data (st0005(iyy),iyy=17,24) / 53,124,105, 99,114,115,124,103/
      data (st0005(iyy),iyy=25,30) / 97,112,112,116,124, 0/
      data st0006 / 70, 75, 53, 0/
      data (st0007(iyy),iyy= 1, 8) / 35, 32, 37,115, 58, 32, 37,115/
      data (st0007(iyy),iyy= 9,16) / 32, 37,115, 32, 32, 80,114,111/
      data (st0007(iyy),iyy=17,24) /106,101, 99,116,105,111,110, 58/
      data (st0007(iyy),iyy=25,32) / 32, 37,115, 32, 32, 82, 97, 47/
      data (st0007(iyy),iyy=33,40) / 68,101, 99, 32, 97,120,101,115/
      data (st0007(iyy),iyy=41,48) / 58, 32, 37,100, 47, 37,100, 10/
      data (st0007(iyy),iyy=49,49) / 0/
      data (st0008(iyy),iyy= 1, 8) / 35, 32, 32, 32, 32, 32, 67,111/
      data (st0008(iyy),iyy= 9,16) /111,114,100,105,110, 97,116,101/
      data (st0008(iyy),iyy=17,24) /115, 58, 32,101,113,117, 97,116/
      data (st0008(iyy),iyy=25,32) /111,114,105, 97,108, 32, 37,115/
      data (st0008(iyy),iyy=33,34) / 10, 0/
      data (st0009(iyy),iyy= 1, 8) / 35, 32, 32, 32, 32, 32, 77, 74/
      data (st0009(iyy),iyy= 9,16) / 68, 58, 32, 37, 48, 46, 53,102/
      data (st0009(iyy),iyy=17,24) / 32, 69,112,111, 99,104, 58, 32/
      data (st0009(iyy),iyy=25,32) / 74, 37, 48, 46, 56,102, 32, 66/
      data (st0009(iyy),iyy=33,39) / 37, 48, 46, 56,102, 10, 0/
      data (st0010(iyy),iyy= 1, 8) / 35, 32, 32, 32, 32, 32, 67,111/
      data (st0010(iyy),iyy= 9,16) /111,114,100,105,110, 97,116,101/
      data (st0010(iyy),iyy=17,24) /115, 58, 32,101,113,117, 97,116/
      data (st0010(iyy),iyy=25,32) /111,114,105, 97,108, 32, 37,115/
      data (st0010(iyy),iyy=33,40) / 32, 69,113,117,105,110,111,120/
      data (st0010(iyy),iyy=41,48) / 58, 32, 74, 37, 48, 46, 51,102/
      data (st0010(iyy),iyy=49,50) / 10, 0/
      data (st0011(iyy),iyy= 1, 8) / 35, 32, 32, 32, 32, 32, 69,112/
      data (st0011(iyy),iyy= 9,16) /111, 99,104, 58, 32, 74, 37, 48/
      data (st0011(iyy),iyy=17,24) / 46, 56,102, 32, 77, 74, 68, 58/
      data (st0011(iyy),iyy=25,32) / 32, 37, 48, 46, 53,102, 10, 0/
      data (st0012(iyy),iyy= 1, 8) / 35, 32, 32, 32, 32, 32, 67,111/
      data (st0012(iyy),iyy= 9,16) /111,114,100,105,110, 97,116,101/
      data (st0012(iyy),iyy=17,24) /115, 58, 32,101,113,117, 97,116/
      data (st0012(iyy),iyy=25,32) /111,114,105, 97,108, 32, 37,115/
      data (st0012(iyy),iyy=33,40) / 32, 69,113,117,105,110,111,120/
      data (st0012(iyy),iyy=41,48) / 58, 32, 66, 37, 48, 46, 51,102/
      data (st0012(iyy),iyy=49,50) / 10, 0/
      data (st0013(iyy),iyy= 1, 8) / 35, 32, 32, 32, 32, 32, 69,112/
      data (st0013(iyy),iyy= 9,16) /111, 99,104, 58, 32, 66, 37, 48/
      data (st0013(iyy),iyy=17,24) / 46, 56,102, 32, 77, 74, 68, 58/
      data (st0013(iyy),iyy=25,32) / 32, 37, 48, 46, 53,102, 10, 0/
      data (st0014(iyy),iyy= 1, 8) / 35, 32, 37,115, 58, 32, 37,115/
      data (st0014(iyy),iyy= 9,16) / 32, 37,115, 32, 32, 80,114,111/
      data (st0014(iyy),iyy=17,24) /106,101, 99,116,105,111,110, 58/
      data (st0014(iyy),iyy=25,32) / 32, 37,115, 32, 32, 69,108,111/
      data (st0014(iyy),iyy=33,40) /110,103, 47, 69,108, 97,116, 32/
      data (st0014(iyy),iyy=41,48) / 97,120,101,115, 58, 32, 37,100/
      data (st0014(iyy),iyy=49,53) / 47, 37,100, 10, 0/
      data (st0015(iyy),iyy= 1, 8) / 35, 32, 32, 32, 32, 32, 67,111/
      data (st0015(iyy),iyy= 9,16) /111,114,100,105,110, 97,116,101/
      data (st0015(iyy),iyy=17,24) /115, 58, 32,101, 99,108,105,112/
      data (st0015(iyy),iyy=25,29) /116,105, 99, 10, 0/
      data (st0016(iyy),iyy= 1, 8) / 35, 32, 32, 32, 32, 32, 77, 74/
      data (st0016(iyy),iyy= 9,16) / 68, 58, 32, 37, 48, 46, 53,102/
      data (st0016(iyy),iyy=17,24) / 32, 32, 69,112,111, 99,104, 58/
      data (st0016(iyy),iyy=25,32) / 32, 74, 37, 48, 46, 56,102, 32/
      data (st0016(iyy),iyy=33,40) / 66, 37, 48, 46, 56,102, 10, 0/
      data (st0017(iyy),iyy= 1, 8) / 35, 32, 37,115, 58, 32, 37,115/
      data (st0017(iyy),iyy= 9,16) / 32, 37,115, 32, 32, 80,114,111/
      data (st0017(iyy),iyy=17,24) /106,101, 99,116,105,111,110, 58/
      data (st0017(iyy),iyy=25,32) / 32, 37,115, 32, 32, 71,108,111/
      data (st0017(iyy),iyy=33,40) /110,103, 47, 71,108, 97,116, 32/
      data (st0017(iyy),iyy=41,48) / 97,120,101,115, 58, 32, 37,100/
      data (st0017(iyy),iyy=49,53) / 47, 37,100, 10, 0/
      data (st0018(iyy),iyy= 1, 8) / 35, 32, 32, 32, 32, 32, 67,111/
      data (st0018(iyy),iyy= 9,16) /111,114,100,105,110, 97,116,101/
      data (st0018(iyy),iyy=17,24) /115, 58, 32,103, 97,108, 97, 99/
      data (st0018(iyy),iyy=25,29) /116,105, 99, 10, 0/
      data (st0019(iyy),iyy= 1, 8) / 35, 32, 32, 32, 32, 32, 77, 74/
      data (st0019(iyy),iyy= 9,16) / 68, 58, 32, 37, 48, 46, 53,102/
      data (st0019(iyy),iyy=17,24) / 32, 69,112,111, 99,104, 58, 32/
      data (st0019(iyy),iyy=25,32) / 74, 37, 48, 46, 56,102, 32, 66/
      data (st0019(iyy),iyy=33,39) / 37, 48, 46, 56,102, 10, 0/
      data (st0020(iyy),iyy= 1, 8) / 35, 32, 37,115, 58, 32, 37,115/
      data (st0020(iyy),iyy= 9,16) / 32, 37,115, 32, 32, 80,114,111/
      data (st0020(iyy),iyy=17,24) /106,101, 99,116,105,111,110, 58/
      data (st0020(iyy),iyy=25,32) / 32, 37,115, 32, 32, 83,108,111/
      data (st0020(iyy),iyy=33,40) /110,103, 47, 83,108, 97,116, 32/
      data (st0020(iyy),iyy=41,48) / 97,120,101,115, 58, 32, 37,100/
      data (st0020(iyy),iyy=49,53) / 47, 37,100, 10, 0/
      data (st0021(iyy),iyy= 1, 8) / 35, 32, 32, 32, 32, 32, 67,111/
      data (st0021(iyy),iyy= 9,16) /111,114,100,105,110, 97,116,101/
      data (st0021(iyy),iyy=17,24) /115, 58, 32,115,117,112,101,114/
      data (st0021(iyy),iyy=25,32) /103, 97,108, 97, 99,116,105, 99/
      data (st0021(iyy),iyy=33,34) / 10, 0/
      data (st0022(iyy),iyy= 1, 8) / 35, 32, 32, 32, 32, 32, 77, 74/
      data (st0022(iyy),iyy= 9,16) / 68, 58, 32, 37, 48, 46, 53,102/
      data (st0022(iyy),iyy=17,24) / 32, 69,112,111, 99,104, 58, 32/
      data (st0022(iyy),iyy=25,32) / 74, 37, 48, 46, 56,102, 32, 66/
      data (st0022(iyy),iyy=33,39) / 37, 48, 46, 56,102, 10, 0/
         call smark (sp)
         call salloc (imname, 255 , 2)
         call salloc (projsr, 255 , 2)
         call salloc (wcsstr, 255 , 2)
         call salloc (radecr, 255 , 2)
         call sscan (images)
         call gargwd (memc(imname), 255 )
         if (.not.(skwrdr (wtype, memc(projsr), 255 , st0001) .le. 0)) 
     *   goto 110
            call xstrcy(st0002, memc(projsr), 255 )
110      continue
         call strupr (memc(projsr))
         if (.not.(skwrdr (ptype, memc(wcsstr), 255 , st0003) .le. 0)) 
     *   goto 120
            call xstrcy(st0004, memc(wcsstr), 255 )
120      continue
         call strlwr (memc(wcsstr))
         sw0001=(ctype)
         goto 130
140      continue
            if (.not.(skwrdr (radecs, memc(radecr), 255 , st0005) .le. 0
     *      )) goto 150
               call xstrcy(st0006, memc(radecr), 255 )
150         continue
            call strupr (memc(radecr))
            call fprinf (fd, st0007)
            call pargsr (label)
            call pargsr (memc(imname))
            call pargsr (memc(wcsstr))
            call pargsr (memc(projsr))
            call pargi (lngax)
            call pargi (latax)
            sw0002=(radecs)
            goto 160
170         continue
               call fprinf (fd, st0008)
               call pargsr (memc(radecr))
               call fprinf (fd, st0009)
               call pargd (epoch)
               if (.not.(((epoch).eq.1.6d308))) goto 180
                  call pargd (1.6d308)
                  call pargd (1.6d308)
                  goto 181
180            continue
                  call pargd (slepj(epoch))
                  call pargd (slepb(epoch))
181            continue
            goto 161
190         continue
               call fprinf (fd, st0010)
               call pargsr (memc(radecr))
               call pargd (equinx)
               call fprinf (fd, st0011)
               call pargd (slepj(epoch))
               call pargd (epoch)
            goto 161
200         continue
               call fprinf (fd, st0012)
               call pargsr (memc(radecr))
               call pargd (equinx)
               call fprinf (fd, st0013)
               call pargd (slepb (epoch))
               call pargd (epoch)
               goto 161
160         continue
               sw0002=sw0002-2
               if (sw0002.lt.1.or.sw0002.gt.3) goto 200
               goto (190,190,170),sw0002
161         continue
         goto 131
210      continue
            call fprinf (fd, st0014)
            call pargsr (label)
            call pargsr (memc(imname))
            call pargsr (memc(wcsstr))
            call pargsr (memc(projsr))
            call pargi (lngax)
            call pargi (latax)
            call fprinf (fd, st0015)
            call fprinf (fd, st0016)
            call pargd (epoch)
            if (.not.(((epoch).eq.1.6d308))) goto 220
               call pargd (1.6d308)
               call pargd (1.6d308)
               goto 221
220         continue
               call pargd (slepj(epoch))
               call pargd (slepb(epoch))
221         continue
         goto 131
230      continue
            call fprinf (fd, st0017)
            call pargsr (label)
            call pargsr (memc(imname))
            call pargsr (memc(wcsstr))
            call pargsr (memc(projsr))
            call pargi (lngax)
            call pargi (latax)
            call fprinf (fd, st0018)
            call fprinf (fd, st0019)
            call pargd (epoch)
            call pargd (slepj(epoch))
            call pargd (slepb(epoch))
         goto 131
240      continue
            call fprinf (fd, st0020)
            call pargsr (label)
            call pargsr (memc(imname))
            call pargsr (memc(wcsstr))
            call pargsr (memc(projsr))
            call pargi (lngax)
            call pargi (latax)
            call fprinf (fd, st0021)
            call fprinf (fd, st0022)
            call pargd (epoch)
            call pargd (slepj(epoch))
            call pargd (slepb(epoch))
            goto 131
130      continue
            if (sw0001.lt.1.or.sw0001.gt.4) goto 131
            goto (140,210,230,240),sw0001
131      continue
         call sfree (sp)
100      return
      end
c     radecs  radecsys
c     equinx  equinox
c     images  imagesys
c     skwrdr  sk_wrdstr
c     skiiwe  sk_iiwrite
c     skiipt  sk_iiprint
c     skimwe  sk_imwrite
c     skinwe  sk_inwrite
c     skimpt  sk_imprint
c     skinpt  sk_inprint
c     projsr  projstr
c     gargwd  gargwrd
c     fprinf  fprintf
c     radecr  radecstr
c     pargsr  pargstr
