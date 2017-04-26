      subroutine sksavm (coo, mw, im)
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
      integer mw
      integer im
      logical xerpop
      logical xerflg
      common /xercom/ xerflg
      integer sw0001,sw0002
      integer*2 st0001(7)
      integer*2 st0002(3)
      integer*2 st0003(7)
      integer*2 st0004(4)
      integer*2 st0005(9)
      integer*2 st0006(4)
      integer*2 st0007(8)
      integer*2 st0008(8)
      integer*2 st0009(9)
      integer*2 st0010(7)
      integer*2 st0011(8)
      integer*2 st0012(8)
      integer*2 st0013(9)
      integer*2 st0014(4)
      integer*2 st0015(8)
      integer*2 st0016(8)
      integer*2 st0017(9)
      integer*2 st0018(5)
      integer*2 st0019(8)
      integer*2 st0020(8)
      integer*2 st0021(9)
      integer*2 st0022(6)
      integer*2 st0023(8)
      integer*2 st0024(8)
      integer*2 st0025(7)
      integer*2 st0026(5)
      integer*2 st0027(7)
      integer*2 st0028(5)
      integer*2 st0029(9)
      integer*2 st0030(8)
      integer*2 st0031(8)
      integer*2 st0032(7)
      integer*2 st0033(5)
      integer*2 st0034(7)
      integer*2 st0035(5)
      integer*2 st0036(9)
      integer*2 st0037(8)
      integer*2 st0038(8)
      integer*2 st0039(7)
      integer*2 st0040(5)
      integer*2 st0041(7)
      integer*2 st0042(5)
      integer*2 st0043(9)
      integer*2 st0044(8)
      integer*2 st0045(8)
      save
      integer iyy
      data st0001 / 97,120,116,121,112,101, 0/
      data st0002 /114, 97, 0/
      data st0003 / 97,120,116,121,112,101, 0/
      data st0004 /100,101, 99, 0/
      data (st0005(iyy),iyy= 1, 8) /114, 97,100,101, 99,115,121,115/
      data (st0005(iyy),iyy= 9, 9) / 0/
      data st0006 / 70, 75, 52, 0/
      data st0007 /101,113,117,105,110,111,120, 0/
      data st0008 /109,106,100, 45,119, 99,115, 0/
      data (st0009(iyy),iyy= 1, 8) /114, 97,100,101, 99,115,121,115/
      data (st0009(iyy),iyy= 9, 9) / 0/
      data st0010 / 70, 75, 52, 78, 79, 69, 0/
      data st0011 /101,113,117,105,110,111,120, 0/
      data st0012 /109,106,100, 45,119, 99,115, 0/
      data (st0013(iyy),iyy= 1, 8) /114, 97,100,101, 99,115,121,115/
      data (st0013(iyy),iyy= 9, 9) / 0/
      data st0014 / 70, 75, 53, 0/
      data st0015 /101,113,117,105,110,111,120, 0/
      data st0016 /109,106,100, 45,119, 99,115, 0/
      data (st0017(iyy),iyy= 1, 8) /114, 97,100,101, 99,115,121,115/
      data (st0017(iyy),iyy= 9, 9) / 0/
      data st0018 / 73, 67, 82, 83, 0/
      data st0019 /101,113,117,105,110,111,120, 0/
      data st0020 /109,106,100, 45,119, 99,115, 0/
      data (st0021(iyy),iyy= 1, 8) /114, 97,100,101, 99,115,121,115/
      data (st0021(iyy),iyy= 9, 9) / 0/
      data st0022 / 71, 65, 80, 80, 84, 0/
      data st0023 /101,113,117,105,110,111,120, 0/
      data st0024 /109,106,100, 45,119, 99,115, 0/
      data st0025 / 97,120,116,121,112,101, 0/
      data st0026 /101,108,111,110, 0/
      data st0027 / 97,120,116,121,112,101, 0/
      data st0028 /101,108, 97,116, 0/
      data (st0029(iyy),iyy= 1, 8) /114, 97,100,101, 99,115,121,115/
      data (st0029(iyy),iyy= 9, 9) / 0/
      data st0030 /101,113,117,105,110,111,120, 0/
      data st0031 /109,106,100, 45,119, 99,115, 0/
      data st0032 / 97,120,116,121,112,101, 0/
      data st0033 /103,108,111,110, 0/
      data st0034 / 97,120,116,121,112,101, 0/
      data st0035 /103,108, 97,116, 0/
      data (st0036(iyy),iyy= 1, 8) /114, 97,100,101, 99,115,121,115/
      data (st0036(iyy),iyy= 9, 9) / 0/
      data st0037 /101,113,117,105,110,111,120, 0/
      data st0038 /109,106,100, 45,119, 99,115, 0/
      data st0039 / 97,120,116,121,112,101, 0/
      data st0040 /115,108,111,110, 0/
      data st0041 / 97,120,116,121,112,101, 0/
      data st0042 /115,108, 97,116, 0/
      data (st0043(iyy),iyy= 1, 8) /114, 97,100,101, 99,115,121,115/
      data (st0043(iyy),iyy= 9, 9) / 0/
      data st0044 /101,113,117,105,110,111,120, 0/
      data st0045 /109,106,100, 45,119, 99,115, 0/
         sw0001=(memi(coo+12) )
         goto 110
120      continue
            call mwswas (mw, memi(coo+15) , st0001, st0002)
            call mwswas (mw, memi(coo+16) , st0003, st0004)
            sw0002=(memi(coo+13) )
            goto 130
140         continue
               call imastr (im, st0005, st0006)
               call imaddd (im, st0007, memd((((coo+8)-1)/2+1)) )
               call imaddd (im, st0008, memd((((coo+10)-1)/2+1)) )
            goto 131
150         continue
               call imastr (im, st0009, st0010)
               call imaddd (im, st0011, memd((((coo+8)-1)/2+1)) )
               call imaddd (im, st0012, memd((((coo+10)-1)/2+1)) )
            goto 131
160         continue
               call imastr (im, st0013, st0014)
               call imaddd (im, st0015, memd((((coo+8)-1)/2+1)) )
               call xerpsh
               call imdelf (im, st0016)
               if (.not.xerpop()) goto 170
170            continue
            goto 131
180         continue
               call imastr (im, st0017, st0018)
               call imaddd (im, st0019, memd((((coo+8)-1)/2+1)) )
               call xerpsh
               call imdelf (im, st0020)
               if (.not.xerpop()) goto 190
190            continue
            goto 131
200         continue
               call imastr (im, st0021, st0022)
               call xerpsh
               call imdelf (im, st0023)
               if (.not.xerpop()) goto 210
210            continue
               call imaddd (im, st0024, memd((((coo+10)-1)/2+1)) )
               goto 131
130         continue
               if (sw0002.lt.1.or.sw0002.gt.5) goto 131
               goto (140,150,160,180,200),sw0002
131         continue
         goto 111
220      continue
            call mwswas (mw, memi(coo+15) , st0025, st0026)
            call mwswas (mw, memi(coo+16) , st0027, st0028)
            call xerpsh
            call imdelf (im, st0029)
            if (.not.xerpop()) goto 230
230         continue
            call xerpsh
            call imdelf (im, st0030)
            if (.not.xerpop()) goto 240
240         continue
            call imaddd (im, st0031, memd((((coo+10)-1)/2+1)) )
         goto 111
250      continue
            call mwswas (mw, memi(coo+15) , st0032, st0033)
            call mwswas (mw, memi(coo+16) , st0034, st0035)
            call xerpsh
            call imdelf (im, st0036)
            if (.not.xerpop()) goto 260
260         continue
            call xerpsh
            call imdelf (im, st0037)
            if (.not.xerpop()) goto 270
270         continue
            call xerpsh
            call imdelf (im, st0038)
            if (.not.xerpop()) goto 280
280         continue
         goto 111
290      continue
            call mwswas (mw, memi(coo+15) , st0039, st0040)
            call mwswas (mw, memi(coo+16) , st0041, st0042)
            call xerpsh
            call imdelf (im, st0043)
            if (.not.xerpop()) goto 300
300         continue
            call xerpsh
            call imdelf (im, st0044)
            if (.not.xerpop()) goto 310
310         continue
            call xerpsh
            call imdelf (im, st0045)
            if (.not.xerpop()) goto 320
320         continue
            goto 111
110      continue
            if (sw0001.lt.1.or.sw0001.gt.4) goto 111
            goto (120,220,250,290),sw0001
111      continue
100      return
      end
      subroutine skctym (coo, im)
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
      integer im
      integer sp
      integer wtype
      integer key1
      integer key2
      integer attr
      integer skwrdr
      integer sw0001
      integer*2 st0001(8)
      integer*2 st0002(8)
      integer*2 st0003(7)
      integer*2 st0004(7)
      integer*2 st0005(114)
      integer*2 st0006(4)
      integer*2 st0007(9)
      integer*2 st0008(9)
      integer*2 st0009(9)
      integer*2 st0010(9)
      integer*2 st0011(9)
      integer*2 st0012(9)
      integer*2 st0013(9)
      integer*2 st0014(9)
      integer*2 st0015(7)
      integer*2 st0016(7)
      save
      integer iyy
      data st0001 / 67, 84, 89, 80, 69, 37,100, 0/
      data st0002 / 67, 84, 89, 80, 69, 37,100, 0/
      data st0003 / 76, 73, 78, 69, 65, 82, 0/
      data st0004 / 76, 73, 78, 69, 65, 82, 0/
      data (st0005(iyy),iyy= 1, 8) /124,108,105,110,124, 97,122,112/
      data (st0005(iyy),iyy= 9,16) /124,116, 97,110,124,115,105,110/
      data (st0005(iyy),iyy=17,24) /124,115,116,103,124, 97,114, 99/
      data (st0005(iyy),iyy=25,32) /124,122,112,110,124,122,101, 97/
      data (st0005(iyy),iyy=33,40) /124, 97,105,114,124, 99,121,112/
      data (st0005(iyy),iyy=41,48) /124, 99, 97,114,124,109,101,114/
      data (st0005(iyy),iyy=49,56) /124, 99,101, 97,124, 99,111,112/
      data (st0005(iyy),iyy=57,64) /124, 99,111,100,124, 99,111,101/
      data (st0005(iyy),iyy=65,72) /124, 99,111,111,124, 98,111,110/
      data (st0005(iyy),iyy=73,80) /124,112, 99,111,124,103,108,115/
      data (st0005(iyy),iyy=81,88) /124,112, 97,114,124, 97,105,116/
      data (st0005(iyy),iyy=89,96) /124,109,111,108,124, 99,115, 99/
      data (st0005(iyy),iyy=97,104) /124,113,115, 99,124,116,115, 99/
      data (st0005(iyy),iyy=105,112) /124,116,110,120,124,122,112,120/
      data (st0005(iyy),iyy=113,114) /124, 0/
      data st0006 /116, 97,110, 0/
      data (st0007(iyy),iyy= 1, 8) / 82, 65, 45, 45, 45, 37, 51,115/
      data (st0007(iyy),iyy= 9, 9) / 0/
      data (st0008(iyy),iyy= 1, 8) / 68, 69, 67, 45, 45, 37, 51,115/
      data (st0008(iyy),iyy= 9, 9) / 0/
      data (st0009(iyy),iyy= 1, 8) / 69, 76, 79, 78, 45, 37, 51,115/
      data (st0009(iyy),iyy= 9, 9) / 0/
      data (st0010(iyy),iyy= 1, 8) / 69, 76, 65, 84, 45, 37, 51,115/
      data (st0010(iyy),iyy= 9, 9) / 0/
      data (st0011(iyy),iyy= 1, 8) / 71, 76, 79, 78, 45, 37, 51,115/
      data (st0011(iyy),iyy= 9, 9) / 0/
      data (st0012(iyy),iyy= 1, 8) / 71, 76, 65, 84, 45, 37, 51,115/
      data (st0012(iyy),iyy= 9, 9) / 0/
      data (st0013(iyy),iyy= 1, 8) / 83, 76, 79, 78, 45, 37, 51,115/
      data (st0013(iyy),iyy= 9, 9) / 0/
      data (st0014(iyy),iyy= 1, 8) / 83, 76, 65, 84, 45, 37, 51,115/
      data (st0014(iyy),iyy= 9, 9) / 0/
      data st0015 / 76, 73, 78, 69, 65, 82, 0/
      data st0016 / 76, 73, 78, 69, 65, 82, 0/
         call smark (sp)
         call salloc (key1, 8, 2)
         call salloc (key2, 8, 2)
         call salloc (wtype, 3, 2)
         call salloc (attr, 8, 2)
         call sprinf (memc(key1), 8, st0001)
         call pargi (memi(coo+15) )
         call sprinf (memc(key2), 8, st0002)
         call pargi (memi(coo+16) )
         if (.not.(memi(coo+14) .le. 0 .or. memi(coo+14) .eq. 1)) goto 
     *   110
            call imastr (im, memc(key1), st0003)
            call imastr (im, memc(key2), st0004)
            call sfree (sp)
            goto 100
110      continue
         if (.not.(skwrdr (memi(coo+14) , memc(wtype), 3, st0005) .le. 0
     *   )) goto 120
            call xstrcy(st0006, memc(wtype), 3)
120      continue
         call strupr (memc(wtype))
         sw0001=(memi(coo+12) )
         goto 130
140      continue
            call sprinf (memc(attr), 8, st0007)
            call pargsr (memc(wtype))
            call imastr (im, memc(key1), memc(attr))
            call sprinf (memc(attr), 8, st0008)
            call pargsr (memc(wtype))
            call imastr (im, memc(key2), memc(attr))
         goto 131
150      continue
            call sprinf (memc(attr), 8, st0009)
            call pargsr (memc(wtype))
            call imastr (im, memc(key1), memc(attr))
            call sprinf (memc(attr), 8, st0010)
            call pargsr (memc(wtype))
            call imastr (im, memc(key2), memc(attr))
         goto 131
160      continue
            call sprinf (memc(attr), 8, st0011)
            call pargsr (memc(wtype))
            call imastr (im, memc(key1), memc(attr))
            call sprinf (memc(attr), 8, st0012)
            call pargsr (memc(wtype))
            call imastr (im, memc(key2), memc(attr))
         goto 131
170      continue
            call sprinf (memc(attr), 8, st0013)
            call pargsr (memc(wtype))
            call imastr (im, memc(key1), memc(attr))
            call sprinf (memc(attr), 8, st0014)
            call pargsr (memc(wtype))
            call imastr (im, memc(key2), memc(attr))
         goto 131
180      continue
            call imastr (im, memc(key1), st0015)
            call imastr (im, memc(key2), st0016)
            goto 131
130      continue
            if (sw0001.lt.1.or.sw0001.gt.4) goto 180
            goto (140,150,160,170),sw0001
131      continue
         call sfree (sp)
100      return
      end
c     sprinf  sprintf
c     skctym  sk_ctypeim
c     skwrdr  sk_wrdstr
c     sksavm  sk_saveim
c     mwswas  mw_swattrs
c     pargsr  pargstr
