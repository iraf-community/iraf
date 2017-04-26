      subroutine imgint (cp, wp)
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
      integer cp
      integer wp
      integer img
      logical xerpop
      logical xerflg
      common /xercom/ xerflg
      integer*2 st0001(12)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /105,109,103, 95,105,110,105,116/
      data (st0001(iyy),iyy= 9,12) / 58, 32, 10, 0/
         if (.not.(.false.)) goto 110
            call xprinf(st0001)
110      continue
         if (.not.(memi(cp+3) .eq. 0)) goto 120
            call xerpsh
            call xcallc(memi(cp+3) , 15, 10 )
            if (.not.xerpop()) goto 130
               goto 100
130         continue
120      continue
         img = memi(cp+3)
         memi(img ) = wp
         memi(img+1) = 0
         memi(img+3) = 0
         memi(img+4) = 0
         memi(img+5) = 0
         memi(img+6) = 0
         memr(img+9) = 0.0
         memr(img+10) = 0.0
         memi(img+11) = 1
100      return
      end
      subroutine imgcae (cp, objid, regid, ref)
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
      integer cp
      integer objid
      integer regid
      integer*2 ref(*)
      integer img
      integer im
      integer wp
      integer stat
      integer*2 alert(1023 +1)
      integer immap
      integer dspmmp
      integer mwsctn
      integer imgams
      integer imgdes
      integer imaccf
      integer skdecm
      logical xerpop
      logical xerflg
      common /xercom/ xerflg
      integer*2 st0001(13)
      integer*2 st0002(19)
      integer*2 st0003(1)
      integer*2 st0004(1)
      integer*2 st0005(6)
      integer*2 st0006(8)
      integer*2 st0007(6)
      integer*2 st0008(8)
      integer*2 st0009(9)
      integer*2 st0010(7)
      integer*2 st0011(7)
      integer*2 st0012(5)
      integer*2 st0013(5)
      integer*2 st0014(7)
      integer*2 st0015(7)
      integer*2 st0016(5)
      integer*2 st0017(5)
      integer*2 st0018(30)
      integer*2 st0019(1)
      integer*2 st0020(1)
      integer*2 st0021(4)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /105,109,103, 95, 99, 97, 99,104/
      data (st0001(iyy),iyy= 9,13) /101, 58, 32, 10, 0/
      data (st0002(iyy),iyy= 1, 8) / 85,110, 97, 98,108,101, 32,116/
      data (st0002(iyy),iyy= 9,16) /111, 32, 99, 97, 99,104,101, 10/
      data (st0002(iyy),iyy=17,19) / 37,115, 0/
      data st0003 / 0/
      data st0004 / 0/
      data st0005 /119,111,114,108,100, 0/
      data st0006 /108,111,103,105, 99, 97,108, 0/
      data st0007 /119,111,114,108,100, 0/
      data st0008 /108,111,103,105, 99, 97,108, 0/
      data (st0009(iyy),iyy= 1, 8) /112,104,121,115,105, 99, 97,108/
      data (st0009(iyy),iyy= 9, 9) / 0/
      data st0010 / 65, 84, 77, 49, 95, 49, 0/
      data st0011 / 65, 84, 77, 50, 95, 50, 0/
      data st0012 / 65, 84, 86, 49, 0/
      data st0013 / 65, 84, 86, 50, 0/
      data st0014 / 68, 84, 77, 49, 95, 49, 0/
      data st0015 / 68, 84, 77, 50, 95, 50, 0/
      data st0016 / 68, 84, 86, 49, 0/
      data st0017 / 68, 84, 86, 50, 0/
      data (st0018(iyy),iyy= 1, 8) / 85,110, 97, 98,108,101, 32,116/
      data (st0018(iyy),iyy= 9,16) /111, 32,100,101, 99,111,100,101/
      data (st0018(iyy),iyy=17,24) / 32,105,109, 97,103,101, 32, 87/
      data (st0018(iyy),iyy=25,30) / 67, 83, 10, 37,115, 0/
      data st0019 / 0/
      data st0020 / 0/
      data st0021 / 66, 80, 77, 0/
         if (.not.(.false.)) goto 110
            call xprinf(st0001)
110      continue
         img = memi(cp+3)
         wp = memi(img )
         call xerpsh
         memi(img+1) = immap (ref, 1 , 0)
         if (.not.xerpop()) goto 120
            call sprinf (alert, 255 , st0002)
            call pargsr (ref)
            call ximalt (alert, st0003, st0004)
            goto 100
120      continue
         memi(img+4) = 0
         memi(img+5) = 0
         memi(img+6) = 0
         call xerpsh
         stat = skdecm (memi(img+1) , st0005, memi(img+3) , memi(img+4) 
     *   )
         if (xerflg) goto 132
         if (.not.(stat .eq. -1 .or. memi(img+3) .eq. 0)) goto 140
            memi(img+11) = 1
140      continue
         if (.not.(memi(img+3) .ne. 0)) goto 150
            memi(img+5) = mwsctn (memi(img+3) , st0006, st0007, 3)
            if (xerflg) goto 132
            memi(img+6) = mwsctn (memi(img+3) , st0008, st0009, 3)
            if (xerflg) goto 132
            im = memi(img+1)
            if (.not.(imaccf(im,st0010) .eq. 1 .and. imaccf(im,st0011) .
     *      eq. 1 .and. imaccf(im,st0012) .eq. 1 .and. imaccf(im,st0013)
     *       .eq. 1)) goto 160
               memi(img+7) = imgams (im, memi(img+3) )
160         continue
            if (.not.(imaccf(im,st0014) .eq. 1 .and. imaccf(im,st0015) .
     *      eq. 1 .and. imaccf(im,st0016) .eq. 1 .and. imaccf(im,st0017)
     *       .eq. 1)) goto 170
               memi(img+8) = imgdes (im, memi(img+3) )
170         continue
150      continue
132      if (.not.xerpop()) goto 130
            call sprinf (alert, 255 , st0018)
            call pargsr (ref)
            call ximalt (alert, st0019, st0020)
            memi(img+11) = 1
130      continue
         if (.not.(memi(wp+2) .eq. 1)) goto 180
            call xerpsh
            memi(img+2) = dspmmp (st0021, memi(img+1) )
            if (.not.xerpop()) goto 190
               memi(img+2) = 0
190         continue
180      continue
         memi(cp) = objid
         memi(cp+1) = regid
         memi(cp+4) = memi(cp+4) + 1
         call xstrcy(ref, memc((((cp+6)-1)*2+1)) , 128)
100      return
      end
      subroutine imgune (cp, id)
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
      integer cp
      integer id
      integer img
      integer*2 st0001(15)
      integer*2 st0002(1)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /105,109,103, 95,117,110, 99, 97/
      data (st0001(iyy),iyy= 9,15) / 99,104,101, 58, 32, 10, 0/
      data st0002 / 0/
         if (.not.(.false.)) goto 110
            call xprinf(st0001)
110      continue
         memi(cp) = 0
         memi(cp+4) = 0
         call xstrcy(st0002, memc((((cp+6)-1)*2+1)) , 255 )
         img = memi(cp+3)
         if (.not.(memi(img+3) .ne. 0)) goto 120
            call mwcloe (memi(img+3) )
120      continue
         if (.not.(memi(img+2) .ne. 0)) goto 130
            call imunmp (memi(img+2) )
130      continue
         if (.not.(memi(img+1) .ne. 0)) goto 140
            call imunmp (memi(img+1) )
140      continue
         memi(img+1) = 0
         memi(img+2) = 0
         memi(img+3) = 0
         memi(img+5) = 0
         memi(img+6) = 0
         memi(img+4) = 0
         memr(img+9) = 0.0
         memr(img+10) = 0.0
         memi(img+11) = 0
         call xmfree(memi(cp+3) , 10 )
         memi(cp+3) = 0
100      return
      end
      subroutine imgwcn (cp, id, x, y)
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
      integer cp
      integer id
      real x
      real y
      integer img
      integer im
      integer wp
      integer co
      double precision dx
      double precision dy
      double precision wx
      double precision wy
      double precision pixval
      real rx
      real ry
      integer i
      integer bpm
      integer*2 buf(1023 +1)
      integer*2 msg(1023 +1)
      integer*2 wcs(32 +1)
      integer*2 xc(32 +1)
      integer*2 yc(32 +1)
      integer*2 xunits(32 +1)
      integer*2 yunits(32 +1)
      double precision skstad
      integer*2 st0001(15)
      integer*2 st0002(37)
      integer*2 st0003(29)
      integer*2 st0004(41)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /105,109,103, 95,119, 99,115,116/
      data (st0001(iyy),iyy= 9,15) /114, 97,110, 58, 32, 10, 0/
      data (st0002(iyy),iyy= 1, 8) /119, 99,115,116,114, 97,110, 32/
      data (st0002(iyy),iyy= 9,16) /123, 32,111, 98,106,101, 99,116/
      data (st0002(iyy),iyy=17,24) / 32, 37,100, 32,125, 32,123, 32/
      data (st0002(iyy),iyy=25,32) /114,101,103,105,111,110, 32, 37/
      data (st0002(iyy),iyy=33,37) /100, 32,125, 32, 0/
      data (st0003(iyy),iyy= 1, 8) /123, 32,112,105,120,118, 97,108/
      data (st0003(iyy),iyy= 9,16) / 32, 37, 57, 46, 57,103, 32,125/
      data (st0003(iyy),iyy=17,24) / 32,123, 32, 98,112,109, 32, 37/
      data (st0003(iyy),iyy=25,29) /100, 32,125, 10, 0/
      data (st0004(iyy),iyy= 1, 8) /123, 99,111,111,114,100, 32,123/
      data (st0004(iyy),iyy= 9,16) / 37, 57,115,125, 32,123, 37, 49/
      data (st0004(iyy),iyy=17,24) / 50,115,125, 32,123, 37, 49, 50/
      data (st0004(iyy),iyy=25,32) /115,125, 32,123, 37, 52,115,125/
      data (st0004(iyy),iyy=33,40) / 32,123, 37, 52,115,125,125, 10/
      data (st0004(iyy),iyy=41,41) / 0/
         if (.not.(.false.)) goto 110
            call xprinf(st0001)
110      continue
         img = memi(cp+3)
         co = memi(img+4)
         wp = memi(img )
         im = memi(img+1)
         dx = (dble(x) - skstad(co,1)) / skstad(co,3)
         dy = (dble(y) - skstad(co,2)) / skstad(co,4)
         rx = dx
         ry = dy
         call imggea (cp, id, rx, ry, pixval, bpm)
         call aclrc (msg, 1023 )
         call sprinf (msg, 1023 , st0002)
         call pargi (memi(cp) )
         call pargi (memi(cp+1) )
         call sprinf (buf, 1023 , st0003)
         call pargd (pixval)
         call pargi (bpm)
         call xstrct(buf, msg, 1023 )
         i=1
120      if (.not.(i .le. 4 )) goto 122
            call imgged (img, dx, dy, memi(memi(wp+3) +i-1), memc(memi(
     *      wp+4) +(32 *(i-1))), wx, wy)
            call imgcos (cp, i, wcs, xunits, yunits)
            call imgcot (cp, i, wx, wy, xc, yc)
            call sprinf (buf, 1023 , st0004)
            call pargsr (wcs)
            call pargsr (xc)
            call pargsr (yc)
            call pargsr (xunits)
            call pargsr (yunits)
            call xstrct(buf, msg, 1023 )
121         i=i+1
            goto 120
122      continue
         call wcspie (msg)
100      return
      end
      subroutine imgwct (cp, id)
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
      integer cp
      integer id
      integer img
      integer im
      integer mw
      integer*2 msg(1023 +1)
      integer*2 st0001(15)
      integer*2 st0002(43)
      integer*2 st0003(12)
      integer*2 st0004(11)
      integer*2 st0005(6)
      integer*2 st0006(7)
      integer*2 st0007(60)
      integer*2 st0008(2)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /105,109,103, 95,119, 99,115,108/
      data (st0001(iyy),iyy= 9,15) /105,115,116, 58, 32, 10, 0/
      data (st0002(iyy),iyy= 1, 8) /119, 99,115,108,105,115,116, 32/
      data (st0002(iyy),iyy= 9,16) /123, 78,111,110,101, 32, 76,111/
      data (st0002(iyy),iyy=17,24) /103,105, 99, 97,108, 32, 87,111/
      data (st0002(iyy),iyy=25,32) /114,108,100, 32, 80,104,121,115/
      data (st0002(iyy),iyy=33,40) /105, 99, 97,108, 32,108,105,110/
      data (st0002(iyy),iyy=41,43) /101, 32, 0/
      data (st0003(iyy),iyy= 1, 8) / 32, 65,109,112,108,105,102,105/
      data (st0003(iyy),iyy= 9,12) /101,114, 32, 0/
      data (st0004(iyy),iyy= 1, 8) / 32, 68,101,116,101, 99,116,111/
      data (st0004(iyy),iyy= 9,11) /114, 32, 0/
      data st0005 / 32, 67, 67, 68, 32, 0/
      data st0006 / 32,108,105,110,101, 32, 0/
      data (st0007(iyy),iyy= 1, 8) / 70, 75, 53, 32, 70, 75, 52, 32/
      data (st0007(iyy),iyy= 9,16) / 73, 67, 82, 83, 32, 71, 65, 80/
      data (st0007(iyy),iyy=17,24) / 80, 84, 32, 70, 75, 52, 45, 78/
      data (st0007(iyy),iyy=25,32) / 79, 45, 69, 32, 69, 99,108,105/
      data (st0007(iyy),iyy=33,40) /112,116,105, 99, 32, 71, 97,108/
      data (st0007(iyy),iyy=41,48) / 97, 99,116,105, 99, 32, 83,117/
      data (st0007(iyy),iyy=49,56) /112,101,114,103, 97,108, 97, 99/
      data (st0007(iyy),iyy=57,60) /116,105, 99, 0/
      data st0008 /125, 0/
         if (.not.(.false.)) goto 110
            call xprinf(st0001)
110      continue
         img = memi(cp+3)
         mw = memi(img+3)
         im = memi(img+1)
         call xstrcy(st0002, msg, 1023 )
         if (.not.(memi(img+7) .ne. 0)) goto 120
            call xstrct(st0003, msg, 1023 )
120      continue
         if (.not.(memi(img+8) .ne. 0)) goto 130
            call xstrct(st0004, msg, 1023 )
130      continue
         if (.not.(memi(img+7) .ne. 0 .or. memi(img+8) .ne. 0)) goto 140
            call xstrct(st0005, msg, 1023 )
140      continue
         call xstrct(st0006, msg, 1023 )
         if (.not.(mw .ne. 0)) goto 150
            call xstrct(st0007, msg, 1023 )
150      continue
         call xstrct(st0008, msg, 1023 )
         call wcspie (msg)
100      return
      end
      subroutine imggea (cp, id, x, y, pixval, bpmpix)
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
      integer cp
      integer id
      real x
      real y
      double precision pixval
      integer bpmpix
      integer img
      integer wp
      integer im
      integer bpm
      integer pix
      integer nl
      integer nc
      integer ix
      integer iy
      integer size
      integer x1
      integer x2
      integer y1
      integer y2
      integer imgs2r
      integer imgs2i
      integer*2 st0001(16)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /105,109,103, 95,103,101,116, 95/
      data (st0001(iyy),iyy= 9,16) /100, 97,116, 97, 58, 32, 10, 0/
         if (.not.(.false.)) goto 110
            call xprinf(st0001)
110      continue
         img = memi(cp+3)
         wp = memi(img )
         im = memi(img+1)
         bpm = memi(img+2)
         nc = meml(im+200 +1+8-1)
         nl = meml(im+200 +2+8-1)
         size = memi(wp+1)
         if (.not.(x .lt. 0.0 .or. y .lt. 0.0 .or. x .gt. nc .or. y .gt.
     *    nl)) goto 120
            goto 100
120      continue
         ix = int (x + 0.5) 
         iy = int (y + 0.5)
         ix = max (size/2+1, ix) 
         iy = max (size/2+1, iy)
         ix = min (ix, (nc-(size/2)-1)) 
         iy = min (iy, (nl-(size/2)-1))
         x1 = ix - size / 2 + 0.5
         x2 = ix + size / 2 + 0.5
         y1 = iy - size / 2 + 0.5
         y2 = iy + size / 2 + 0.5
         x1 = max (1, x1)
         x2 = min (nc, x2)
         y1 = max (1, y1)
         y2 = min (nl, y2)
         pix = imgs2r (im, int(x1), int(x2), int(y1), int(y2))
         if (.not.(bpm .ne. 0 .and. memi(wp+2) .eq. 1)) goto 130
            bpmpix = memi(imgs2i (bpm, ix, ix, iy, iy))
            goto 131
130      continue
            bpmpix = 0
131      continue
         pixval = memr(pix + ((size/2)*size) + (size/2)) * 1.0d0
         if (.not.(memi(wp+1) .gt. 1)) goto 140
            call imgseb (memr(pix), memi(wp+1) , x1, x2, y1, y2)
140      continue
100      return
      end
      subroutine imgobo (cp, id, temple)
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
      integer cp
      integer id
      integer*2 temple(*)
      integer im
      integer img
      integer*2 st0001(15)
      integer*2 st0002(7)
      integer*2 st0003(7)
      integer*2 st0004(96)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /105,109,103, 95,111, 98,106,105/
      data (st0001(iyy),iyy= 9,15) /110,102,111, 58, 32, 10, 0/
      data st0002 /105,109,103,104,100,114, 0/
      data st0003 /119, 99,115,104,100,114, 0/
      data (st0004(iyy),iyy= 1, 8) / 87, 67, 83, 68, 73, 77, 44, 67/
      data (st0004(iyy),iyy= 9,16) / 84, 89, 80, 69, 42, 44, 67, 82/
      data (st0004(iyy),iyy=17,24) / 80, 73, 88, 42, 44, 67, 82, 86/
      data (st0004(iyy),iyy=25,32) / 65, 76, 42, 44, 67, 68, 42, 44/
      data (st0004(iyy),iyy=33,40) / 67, 82, 79, 84, 65, 50, 44, 76/
      data (st0004(iyy),iyy=41,48) / 84, 86, 42, 44, 76, 84, 77, 42/
      data (st0004(iyy),iyy=49,56) / 44, 87, 83, 86, 42, 44, 87, 65/
      data (st0004(iyy),iyy=57,64) / 84, 42, 44, 82, 65, 42, 44, 68/
      data (st0004(iyy),iyy=65,72) / 69, 67, 42, 44, 69, 81, 85, 73/
      data (st0004(iyy),iyy=73,80) / 78, 79, 88, 44, 69, 80, 79, 67/
      data (st0004(iyy),iyy=81,88) / 72, 44, 77, 74, 68, 42, 44, 68/
      data (st0004(iyy),iyy=89,96) / 65, 84, 69, 45, 79, 66, 83, 0/
         if (.not.(.false.)) goto 110
            call xprinf(st0001)
110      continue
         img = memi(cp+3)
         im = memi(img+1)
         call imgser (im, st0002, temple)
         call imgser (im, st0003, st0004)
         call imgseo (im, cp)
         call imgses (im, cp)
100      return
      end
      subroutine imgser (im, object, temple)
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
      integer*2 object(*)
      integer*2 temple(*)
      integer sp
      integer hdr
      integer lbuf
      integer line
      integer field
      integer keyw
      integer dict
      integer ip
      integer lp
      integer list
      integer nlines
      integer in
      integer out
      integer i
      integer hdrsie
      logical keywfr
      integer stropn
      integer getlie
      integer stridx
      integer imgnfn
      integer strdic
      integer imofnu
      logical streq
      logical xerflg
      common /xercom/ xerflg
      integer*2 st0001(5)
      integer*2 st0002(2)
      integer*2 st0003(2)
      integer*2 st0004(2)
      integer*2 st0005(3)
      integer*2 st0006(2)
      integer*2 st0007(5)
      integer*2 st0008(2)
      integer*2 st0009(11)
      save
      integer iyy
      data st0001 / 37,115, 32,123, 0/
      data st0002 / 42, 0/
      data st0003 /124, 0/
      data st0004 /124, 0/
      data st0005 / 91,123, 0/
      data st0006 /125, 0/
      data st0007 / 37,115, 32,123, 0/
      data st0008 /125, 0/
      data (st0009(iyy),iyy= 1, 8) / 37,100, 32,123, 32, 10, 10, 10/
      data (st0009(iyy),iyy= 9,11) / 32,125, 0/
         hdrsie = (200 + memi(im+30) - (200 +1024 ) ) * 2 - 1
         hdrsie = hdrsie + 1023
         call smark (sp)
         call salloc (hdr, hdrsie, 2)
         call salloc (dict, hdrsie, 2)
         call salloc (field, 1023 , 2)
         call salloc (lbuf, 1023 , 2)
         call salloc (line, 1023 , 2)
         call salloc (keyw, 8, 2)
         in = stropn (memc((im+(200 +1024 ) -1)*2 + 1), hdrsie, 1 )
         if (xerflg) goto 100
         out = stropn (memc(hdr), hdrsie, 3)
         if (xerflg) goto 100
         call fprinf (out, st0001)
         call pargsr (object)
         keywfr = (.not.streq (temple, st0002))
         if (.not.(keywfr)) goto 110
            list = imofnu (im, temple)
            if (xerflg) goto 100
            call xstrcy(st0003, memc(dict), hdrsie)
120         if (.not.(imgnfn (list, memc(field), 255 ) .ne. -2).and.(.
     *      not.xerflg)) goto 121
            if (xerflg) goto 100
               call xstrct(memc(field), memc(dict), hdrsie)
               call xstrct(st0004, memc(dict), hdrsie)
               goto 120
121         continue
            call imcfnl (list)
110      continue
         nlines = 0
130      if (.not.(getlie (in, memc(lbuf)) .ne. -2).and.(.not.xerflg)) 
     *   goto 131
         if (xerflg) goto 100
            call aclrc (memc(line), 1023 )
            ip = lbuf
            lp = line
140         if (.not.(memc(ip) .ne. 0 .and. memc(ip) .ne. 10)) goto 141
               if (.not.(stridx (memc(ip), st0005) .gt. 0)) goto 150
                  memc(lp) = 92
                  lp = lp + 1
150            continue
               memc(lp) = memc(ip)
               ip = ip + 1
               lp = lp + 1
               goto 140
141         continue
            memc(lp) = 10
            memc(lp+1) = 0
            if (.not.(keywfr)) goto 160
               i=0
170            if (.not.(i .lt. 8 .and. .not.(memc(line+i).eq.32.or.memc
     *         (line+i).eq.9))) goto 172
                  memc(keyw+i) = memc(line+i)
171               i=i+1
                  goto 170
172            continue
               memc(keyw+i) = 0
               if (.not.(strdic (memc(keyw), memc(keyw), 8, memc(dict)) 
     *         .eq. 0).and.(.not.xerflg)) goto 180
               if (xerflg) goto 100
                  goto 130
180            continue
160         continue
            call putci (out, 32)
            if (xerflg) goto 100
            call putlie (out, memc(line))
            if (xerflg) goto 100
            nlines = nlines + 1
            if (.not.(mod(nlines,10) .eq. 0)) goto 190
               call fprinf (out, st0006)
               call xfcloe(out)
               call wcspie (memc(hdr))
               call aclrc (memc(hdr), hdrsie)
               out = stropn (memc(hdr), hdrsie, 3)
               if (xerflg) goto 100
               call fprinf (out, st0007)
               call pargsr (object)
190         continue
            goto 130
131      continue
         call fprinf (out, st0008)
         call xfcloe(in)
         call xfcloe(out)
         call wcspie (memc(hdr))
         call sprinf (memc(hdr), 1023 , st0009)
         call pargsr (object)
         call wcspie (memc(hdr))
         call sfree (sp)
100      return
      end
      subroutine imgses (im, cp)
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
      integer cp
      integer sp
      integer buf
      integer img
      integer co
      double precision cx
      double precision cy
      double precision cx1
      double precision cy1
      double precision dx
      double precision dy
      double precision x1
      double precision y1
      double precision cosa
      double precision sina
      double precision angle
      integer i
      integer j
      integer compx
      integer compy
      integer*4 axis(7 )
      integer*4 lv(7 )
      integer*4 pv1(7 )
      integer*4 pv2(7 )
      integer*2 st0001(24)
      integer*2 st0002(4)
      integer*2 st0003(4)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) / 99,111,109,112, 97,115,115, 32/
      data (st0001(iyy),iyy= 9,16) / 37,100, 32, 37,103, 32, 37,100/
      data (st0001(iyy),iyy=17,24) / 32, 37,100, 32, 37,115, 0, 0/
      data st0002 / 69, 32, 78, 0/
      data st0003 / 88, 32, 89, 0/
         call smark (sp)
         call salloc (buf, 1023 , 2)
         call aclrc (memc(buf), 1023 )
         img = memi(cp+3)
         co = memi(img+4)
         if (.not.(memi(img+5) .ne. 0)) goto 110
            if (.not.(memr(img+9) .gt. 0.0)) goto 120
               angle = -memr(img+9)
               goto 121
120         continue
               angle = memr(img+9) + 360.0
121         continue
            cosa = cos (((angle)/57.295779513082320877))
            sina = sin (((angle)/57.295779513082320877))
            cx = meml(im+200 +1+8-1) / 2.0d0
            cy = meml(im+200 +2+8-1) / 2.0d0
            call mwc2td (memi(img+5) , cx, cy, cx1, cy1)
            dx = cx + ( 10.0 * sina)
            dy = cy + ( 10.0 * cosa)
            call mwc2td (memi(img+5) , dx, dy, x1, y1)
            if (.not.(y1 .ge. cy1)) goto 130
               compy = 1
               goto 131
130         continue
               compy = -1
131         continue
            dx = cx + (-10.0 * cosa)
            dy = cy + ( 10.0 * sina)
            call mwc2td (memi(img+5) , dx, dy, x1, y1)
            if (.not.(x1 .ge. cx1)) goto 140
               compx = 1
               goto 141
140         continue
               compx = -1
141         continue
            goto 111
110      continue
            lv(1) = 0
            lv(2) = 0
            call imaplv (im, lv, pv1, 2)
            lv(1) = 1
            lv(2) = 1
            call imaplv (im, lv, pv2, 2)
            i = 1
            axis(1) = 1
            axis(2) = 2
            do 150 j = 1, 7 
               if (.not.(pv1(j) .ne. pv2(j))) goto 160
                  axis(i) = j
                  i = i + 1
160            continue
150         continue
151         continue
            compx = - (pv2(axis(1)) - pv1(axis(1)))
            compy = (pv2(axis(2)) - pv1(axis(2)))
111      continue
         call sprinf (memc(buf), 1023 , st0001)
         call pargi (memi(cp) )
         call pargr (memr(img+9) )
         call pargi (compx)
         call pargi (compy)
         if (.not.(memi(img+3) .ne. 0)) goto 170
            call pargsr (st0002)
            goto 171
170      continue
            call pargsr (st0003)
171      continue
         call wcspie (memc(buf))
         call sfree (sp)
100      return
      end
      subroutine imgseo (im, cp)
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
      integer cp
      integer sp
      integer co
      integer img
      integer mw
      integer buf
      integer proj
      integer radecr
      integer fd
      integer radecs
      integer ctype
      integer wtype
      integer ndim
      double precision crpix1
      double precision crpix2
      double precision crval1
      double precision crval2
      double precision cval1
      double precision cval2
      double precision xscale
      double precision yscale
      double precision xrot
      double precision yrot
      double precision r(7 )
      double precision w(7 )
      double precision cd(7 ,7 )
      integer idxstr
      integer skstai
      integer stropn
      integer mwstai
      double precision skstad
      double precision slepj
      double precision slepb
      logical fpequd
      integer sw0001,sw0002
      logical xerflg
      common /xercom/ xerflg
      integer*2 st0001(21)
      integer*2 st0002(15)
      integer*2 st0003(15)
      integer*2 st0004(29)
      integer*2 st0005(15)
      integer*2 st0006(15)
      integer*2 st0007(30)
      integer*2 st0008(4)
      integer*2 st0009(114)
      integer*2 st0010(8)
      integer*2 st0011(11)
      integer*2 st0012(52)
      integer*2 st0013(11)
      integer*2 st0014(9)
      integer*2 st0015(1)
      integer*2 st0016(9)
      integer*2 st0017(1)
      integer*2 st0018(14)
      integer*2 st0019(1)
      integer*2 st0020(7)
      integer*2 st0021(1)
      integer*2 st0022(25)
      integer*2 st0023(41)
      integer*2 st0024(53)
      integer*2 st0025(4)
      integer*2 st0026(4)
      integer*2 st0027(4)
      integer*2 st0028(4)
      integer*2 st0029(53)
      integer*2 st0030(4)
      integer*2 st0031(4)
      integer*2 st0032(4)
      integer*2 st0033(4)
      integer*2 st0034(58)
      integer*2 st0035(55)
      integer*2 st0036(57)
      integer*2 st0037(2)
      integer*2 st0038(2)
      integer*2 st0039(2)
      integer*2 st0040(2)
      integer*2 st0041(2)
      integer*2 st0042(2)
      integer*2 st0043(25)
      integer*2 st0044(7)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) / 87, 67, 83, 32, 73,110,102,111/
      data (st0001(iyy),iyy= 9,16) / 58, 10, 61, 61, 61, 61, 61, 61/
      data (st0001(iyy),iyy=17,21) / 61, 61, 61, 10, 0/
      data (st0002(iyy),iyy= 1, 8) / 82, 32,116,101,114,109, 58, 32/
      data (st0002(iyy),iyy= 9,15) / 37,103, 32, 37,103, 10, 0/
      data (st0003(iyy),iyy= 1, 8) / 87, 32,116,101,114,109, 58, 32/
      data (st0003(iyy),iyy= 9,15) / 37,103, 32, 37,103, 10, 0/
      data (st0004(iyy),iyy= 1, 8) / 32, 32, 32, 32, 99,100, 58, 32/
      data (st0004(iyy),iyy= 9,16) / 37,103, 32, 37,103, 10, 32, 32/
      data (st0004(iyy),iyy=17,24) / 32, 32, 32, 32, 32, 32, 37,103/
      data (st0004(iyy),iyy=25,29) / 32, 37,103, 10, 0/
      data (st0005(iyy),iyy= 1, 8) / 32,115, 99, 97,108,101, 58, 32/
      data (st0005(iyy),iyy= 9,15) / 37,103, 32, 37,103, 10, 0/
      data (st0006(iyy),iyy= 1, 8) / 32, 32, 32,114,111,116, 58, 32/
      data (st0006(iyy),iyy= 9,15) / 37,103, 32, 37,103, 10, 0/
      data (st0007(iyy),iyy= 1, 8) /124,102,107, 52,124,102,107, 52/
      data (st0007(iyy),iyy= 9,16) / 45,110,111, 45,101,124,102,107/
      data (st0007(iyy),iyy=17,24) / 53,124,105, 99,114,115,124,103/
      data (st0007(iyy),iyy=25,30) / 97,112,112,116,124, 0/
      data st0008 / 70, 75, 53, 0/
      data (st0009(iyy),iyy= 1, 8) /124,108,105,110,124, 97,122,112/
      data (st0009(iyy),iyy= 9,16) /124,116, 97,110,124,115,105,110/
      data (st0009(iyy),iyy=17,24) /124,115,116,103,124, 97,114, 99/
      data (st0009(iyy),iyy=25,32) /124,122,112,110,124,122,101, 97/
      data (st0009(iyy),iyy=33,40) /124, 97,105,114,124, 99,121,112/
      data (st0009(iyy),iyy=41,48) /124, 99, 97,114,124,109,101,114/
      data (st0009(iyy),iyy=49,56) /124, 99,101, 97,124, 99,111,112/
      data (st0009(iyy),iyy=57,64) /124, 99,111,100,124, 99,111,101/
      data (st0009(iyy),iyy=65,72) /124, 99,111,111,124, 98,111,110/
      data (st0009(iyy),iyy=73,80) /124,112, 99,111,124,103,108,115/
      data (st0009(iyy),iyy=81,88) /124,112, 97,114,124, 97,105,116/
      data (st0009(iyy),iyy=89,96) /124,109,111,108,124, 99,115, 99/
      data (st0009(iyy),iyy=97,104) /124,113,115, 99,124,116,115, 99/
      data (st0009(iyy),iyy=105,112) /124,116,110,120,124,122,112,120/
      data (st0009(iyy),iyy=113,114) /124, 0/
      data st0010 /108,111,103,105, 99, 97,108, 0/
      data (st0011(iyy),iyy= 1, 8) /119, 99,115,105,110,102,111, 32/
      data (st0011(iyy),iyy= 9,11) /123, 10, 0/
      data (st0012(iyy),iyy= 1, 8) / 32, 32, 32, 32, 32, 32, 80,114/
      data (st0012(iyy),iyy= 9,16) /111,106,101, 99,116,105,111,110/
      data (st0012(iyy),iyy=17,24) / 58, 32, 32, 37, 45, 54,115, 9/
      data (st0012(iyy),iyy=25,32) / 32, 32, 32, 32, 32, 32, 32, 32/
      data (st0012(iyy),iyy=33,40) / 32, 32, 32, 32, 83,121,115,116/
      data (st0012(iyy),iyy=41,48) /101,109, 58, 32, 32, 37,115, 32/
      data (st0012(iyy),iyy=49,52) / 37,115, 10, 0/
      data (st0013(iyy),iyy= 1, 8) / 69,113,117, 97,116,111,114,105/
      data (st0013(iyy),iyy= 9,11) / 97,108, 0/
      data (st0014(iyy),iyy= 1, 8) / 69, 99,108,105,112,116,105, 99/
      data (st0014(iyy),iyy= 9, 9) / 0/
      data st0015 / 0/
      data (st0016(iyy),iyy= 1, 8) / 71, 97,108, 97, 99,116,105, 99/
      data (st0016(iyy),iyy= 9, 9) / 0/
      data st0017 / 0/
      data (st0018(iyy),iyy= 1, 8) / 83,117,112,101,114, 71, 97,108/
      data (st0018(iyy),iyy= 9,14) / 97, 99,116,105, 99, 0/
      data st0019 / 0/
      data st0020 / 76,105,110,101, 97,114, 0/
      data st0021 / 0/
      data (st0022(iyy),iyy= 1, 8) / 32, 32, 32, 32, 32, 82, 97, 47/
      data (st0022(iyy),iyy= 9,16) / 68,101, 99, 32, 97,120,101,115/
      data (st0022(iyy),iyy=17,24) / 58, 32, 32, 37,100, 47, 37,100/
      data (st0022(iyy),iyy=25,25) / 0/
      data (st0023(iyy),iyy= 1, 8) / 32, 32, 32, 32, 32, 32, 32, 32/
      data (st0023(iyy),iyy= 9,16) / 32, 32, 32, 32, 32, 32, 32, 32/
      data (st0023(iyy),iyy=17,24) / 32, 32, 68,105,109,101,110,115/
      data (st0023(iyy),iyy=25,32) /105,111,110,115, 58, 32, 32, 37/
      data (st0023(iyy),iyy=33,40) /100, 32,120, 32, 37,100, 10, 10/
      data (st0023(iyy),iyy=41,41) / 0/
      data (st0024(iyy),iyy= 1, 8) / 32, 32, 32, 32, 32, 32, 67,101/
      data (st0024(iyy),iyy= 9,16) /110,116,101,114, 32, 80,111,115/
      data (st0024(iyy),iyy=17,24) / 58, 32, 37, 51,115, 58, 32, 32/
      data (st0024(iyy),iyy=25,32) / 37, 45, 49, 50, 72, 32, 32, 32/
      data (st0024(iyy),iyy=33,40) / 32, 32, 32, 32, 32, 32, 32, 32/
      data (st0024(iyy),iyy=41,48) / 37, 51,115, 58, 32, 32, 37, 45/
      data (st0024(iyy),iyy=49,53) / 49, 50,104, 10, 0/
      data st0025 / 32, 82, 65, 0/
      data st0026 / 76,111,110, 0/
      data st0027 / 68,101, 99, 0/
      data st0028 / 76, 97,116, 0/
      data (st0029(iyy),iyy= 1, 8) / 32, 32, 32, 82,101,102,101,114/
      data (st0029(iyy),iyy= 9,16) /101,110, 99,101, 32, 80,111,115/
      data (st0029(iyy),iyy=17,24) / 58, 32, 37, 51,115, 58, 32, 32/
      data (st0029(iyy),iyy=25,32) / 37, 45, 49, 50, 72, 32, 32, 32/
      data (st0029(iyy),iyy=33,40) / 32, 32, 32, 32, 32, 32, 32, 32/
      data (st0029(iyy),iyy=41,48) / 37, 51,115, 58, 32, 32, 37, 45/
      data (st0029(iyy),iyy=49,53) / 49, 50,104, 10, 0/
      data st0030 / 32, 82, 65, 0/
      data st0031 / 76,111,110, 0/
      data st0032 / 68,101, 99, 0/
      data st0033 / 76, 97,116, 0/
      data (st0034(iyy),iyy= 1, 8) / 32, 82,101,102,101,114,101,110/
      data (st0034(iyy),iyy= 9,16) / 99,101, 32, 80,105,120,101,108/
      data (st0034(iyy),iyy=17,24) / 58, 32, 32, 32, 88, 58, 32, 32/
      data (st0034(iyy),iyy=25,32) / 37, 45, 57, 46, 52,102, 32, 32/
      data (st0034(iyy),iyy=33,40) / 32, 32, 32, 32, 32, 32, 32, 32/
      data (st0034(iyy),iyy=41,48) / 32, 32, 32, 32, 32, 32, 89, 58/
      data (st0034(iyy),iyy=49,56) / 32, 32, 37, 45, 57, 46, 52,102/
      data (st0034(iyy),iyy=57,58) / 10, 0/
      data (st0035(iyy),iyy= 1, 8) / 32, 32, 32, 32, 32, 80,108, 97/
      data (st0035(iyy),iyy= 9,16) /116,101, 32, 83, 99, 97,108,101/
      data (st0035(iyy),iyy=17,24) / 58, 32, 32, 37, 45, 56,102, 32/
      data (st0035(iyy),iyy=25,32) / 32, 32, 32, 32, 32, 32, 32, 32/
      data (st0035(iyy),iyy=33,40) / 32, 32, 32, 32, 32, 82,111,116/
      data (st0035(iyy),iyy=41,48) / 32, 65,110,103,108,101, 58, 32/
      data (st0035(iyy),iyy=49,55) / 32, 37, 45, 56,102, 10, 0/
      data (st0036(iyy),iyy= 1, 8) / 32, 32, 32, 32, 32, 32, 32, 32/
      data (st0036(iyy),iyy= 9,16) / 32, 69,113,117,105,110,111,120/
      data (st0036(iyy),iyy=17,24) / 58, 32, 32, 37,115, 37, 56,102/
      data (st0036(iyy),iyy=25,32) / 32, 32, 32, 32, 32, 32, 32, 32/
      data (st0036(iyy),iyy=33,40) / 32, 32, 32, 32, 32, 32, 32, 32/
      data (st0036(iyy),iyy=41,48) / 32, 69,112,111, 99,104, 58, 32/
      data (st0036(iyy),iyy=49,56) / 32, 37,115, 37, 46, 54,102, 10/
      data (st0036(iyy),iyy=57,57) / 0/
      data st0037 / 74, 0/
      data st0038 / 74, 0/
      data st0039 / 32, 0/
      data st0040 / 32, 0/
      data st0041 / 66, 0/
      data st0042 / 66, 0/
      data (st0043(iyy),iyy= 1, 8) / 32, 32, 32, 32, 32, 32, 32, 32/
      data (st0043(iyy),iyy= 9,16) / 32, 32, 32, 32, 32, 77, 74, 68/
      data (st0043(iyy),iyy=17,24) / 58, 32, 32, 37, 46, 54,102, 10/
      data (st0043(iyy),iyy=25,25) / 0/
      data st0044 /125, 10, 32, 10, 32, 10, 0/
         call smark (sp)
         call salloc (buf, 1023 , 2)
         call salloc (proj, 255 , 2)
         call salloc (radecr, 255 , 2)
         fd = stropn (memc(buf), 1023 , 3)
         if (xerflg) goto 100
         img = memi(cp+3)
         co = memi(img+4)
         radecs = skstai (co, 8)
         ctype = skstai (co, 7)
         wtype = skstai (co, 9)
         mw = memi(img+3)
         if (.not.(mw .ne. 0)) goto 110
            ndim = mwstai (mw, 5 )
            call wcsgfm (mw, r, w, cd, ndim)
            crpix1 = r(1)
            crpix2 = r(2)
            crval1 = w(1)
            crval2 = w(2)
            xscale = sqrt (cd(1,1)**2 + cd(2,1)**2) * 3600.0d0
            yscale = sqrt (cd(1,2)**2 + cd(2,2)**2) * 3600.0d0
            xrot = 0.0
            yrot = 0.0
            if (.not.(.not.fpequd (cd(1,1), 0.0d0))) goto 120
               xrot = ((atan ( cd(2,1) / cd(1,1)))*57.295779513082320877
     *         d0)
120         continue
            if (.not.(.not.fpequd (cd(2,2), 0.0d0))) goto 130
               yrot = ((atan (-cd(1,2) / cd(2,2)))*57.295779513082320877
     *         d0)
130         continue
            goto 111
110      continue
            ndim = 2
            xscale = 1.0
            yscale = 1.0
            xrot = 0.0
            yrot = 0.0
111      continue
         if (.not.(.false.)) goto 140
            call xprinf(st0001)
            call xprinf(st0002)
            call pargd(r(1))
            call pargd(r(2))
            call xprinf(st0003)
            call pargd(w(1))
            call pargd(w(2))
            call xprinf(st0004)
            call pargd(cd(1,1))
            call pargd(cd(1,2))
            call pargd(cd(2,1))
            call pargd(cd(2,2))
            call xprinf(st0005)
            call pargd(xscale)
            call pargd(yscale)
            call xprinf(st0006)
            call pargd(xrot)
            call pargd(yrot)
140      continue
         memr(img+10) = (xscale + yscale) / 2.0d0
         memr(img+9) = xrot
         if (.not.(idxstr (radecs, memc(radecr), 255 , st0007) .le. 0)) 
     *   goto 150
            call xstrcy(st0008, memc(radecr), 255 )
150      continue
         call strupr (memc(radecr))
         if (.not.(idxstr (wtype, memc(proj), 255 , st0009) .le. 0)) 
     *   goto 160
            call xstrcy(st0010, memc(proj), 255 )
160      continue
         call strupr (memc(proj))
         call fprinf (fd, st0011)
         call fprinf (fd, st0012)
         call pargsr (memc(proj))
         sw0001=(ctype)
         goto 170
180      continue
            call pargsr (st0013)
            call pargsr (memc(radecr))
         goto 171
190      continue
            call pargsr (st0014)
            call pargsr (st0015)
         goto 171
200      continue
            call pargsr (st0016)
            call pargsr (st0017)
         goto 171
210      continue
            call pargsr (st0018)
            call pargsr (st0019)
         goto 171
220      continue
            call pargsr (st0020)
            call pargsr (st0021)
            goto 171
170      continue
            if (sw0001.lt.1.or.sw0001.gt.4) goto 220
            goto (180,190,200,210),sw0001
171      continue
         call fprinf (fd, st0022)
         call pargi (skstai (co, 10))
         call pargi (skstai (co, 11))
         call fprinf (fd, st0023)
         call pargi (meml(im+200 +1+8-1) )
         call pargi (meml(im+200 +2+8-1) )
         call fprinf (fd, st0024)
         if (.not.(ctype .eq. 1)) goto 230
            call pargsr (st0025)
            goto 231
230      continue
            call pargsr (st0026)
231      continue
         call pargd (cval1)
         if (.not.(ctype .eq. 1)) goto 240
            call pargsr (st0027)
            goto 241
240      continue
            call pargsr (st0028)
241      continue
         call pargd (cval2)
         call fprinf (fd, st0029)
         if (.not.(ctype .eq. 1)) goto 250
            call pargsr (st0030)
            goto 251
250      continue
            call pargsr (st0031)
251      continue
         call pargd (crval1)
         if (.not.(ctype .eq. 1)) goto 260
            call pargsr (st0032)
            goto 261
260      continue
            call pargsr (st0033)
261      continue
         call pargd (crval2)
         call fprinf (fd, st0034)
         call pargd (crpix1)
         call pargd (crpix2)
         call fprinf (fd, st0035)
         call pargr (memr(img+10) )
         call pargr (memr(img+9) )
         call fprinf (fd, st0036)
         sw0002=(radecs)
         goto 270
280      continue
            call pargsr (st0037) 
            call pargd (skstad(co,5))
            call pargsr (st0038) 
            call pargd (slepj(skstad(co,6)))
         goto 271
290      continue
            if (.not.(memi(img+11) .eq. 1)) goto 300
               call pargsr (st0039) 
               call pargd (1.6d308)
               call pargsr (st0040) 
               call pargd (1.6d308)
               goto 301
300         continue
               call pargsr (st0041)
               call pargd (skstad(co,5))
               call pargsr (st0042)
               call pargd (slepb(skstad(co,6)))
301         continue
            goto 271
270      continue
            if (sw0002.eq.3) goto 280
            if (sw0002.eq.4) goto 280
            goto 290
271      continue
         call fprinf (fd, st0043)
         call pargd (skstad(co,6))
         call fprinf (fd, st0044)
         call xfcloe(fd)
         call wcspie (memc(buf))
         call sfree (sp)
100      return
      end
      subroutine imgseb (pixtab, size, x1, x2, y1, y2)
      integer size
      integer x1
      integer x2
      integer y1
      integer y2
      real pixtab(*)
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
      integer buf
      integer el
      integer i
      integer j
      integer npix
      real pix
      real sum
      real sum2
      real mean
      real var
      real stdev
      real x
      real y
      integer*2 st0001(20)
      integer*2 st0002(10)
      integer*2 st0003(2)
      integer*2 st0004(5)
      integer*2 st0005(2)
      integer*2 st0006(10)
      integer*2 st0007(3)
      integer*2 st0008(2)
      integer*2 st0009(10)
      integer*2 st0010(3)
      integer*2 st0011(20)
      integer*2 st0012(2)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /112,105,120,116, 97, 98, 32,123/
      data (st0001(iyy),iyy= 9,16) / 10,123, 10,116, 97, 98,108,101/
      data (st0001(iyy),iyy=17,20) / 32,123, 10, 0/
      data (st0002(iyy),iyy= 1, 8) / 32,123, 37, 49, 48, 46, 49,102/
      data (st0002(iyy),iyy= 9,10) /125, 0/
      data st0003 / 10, 0/
      data st0004 /125, 10,125, 10, 0/
      data st0005 /123, 0/
      data (st0006(iyy),iyy= 1, 8) / 32,123, 37, 49, 48, 46, 49,102/
      data (st0006(iyy),iyy= 9,10) /125, 0/
      data st0007 /125, 10, 0/
      data st0008 /123, 0/
      data (st0009(iyy),iyy= 1, 8) / 32,123, 37, 49, 48, 46, 49,102/
      data (st0009(iyy),iyy= 9,10) /125, 0/
      data st0010 /125, 10, 0/
      data (st0011(iyy),iyy= 1, 8) / 32,123, 32, 37, 49, 48, 46, 50/
      data (st0011(iyy),iyy= 9,16) /102, 32, 37, 49, 48, 46, 52,102/
      data (st0011(iyy),iyy=17,20) / 32,125, 10, 0/
      data st0012 /125, 0/
         call smark (sp)
         call salloc (buf, (6*1023 ), 2)
         call salloc (el, 255 , 2)
         call xstrcy(st0001, memc(buf), (6*1023 ))
         sum = 0.0
         sum2 = 0.0
         npix = size * size
         i=size - 1
110      if (.not.(i .ge. 0)) goto 112
            j=1
120         if (.not.(j .le. size)) goto 122
               pix = pixtab((i * size) + j)
               sum = sum + pix
               sum2 = sum2 + (pix * pix)
               call sprinf (memc(el), 255 , st0002)
               call pargr (pix)
               call xstrct(memc(el), memc(buf), (6*1023 ))
121            j=j+1
               goto 120
122         continue
            call xstrct(st0003, memc(buf), (6*1023 ))
111         i=i-1
            goto 110
112      continue
         call xstrct(st0004, memc(buf), (6*1023 ))
         call xstrct(st0005, memc(buf), (6*1023 ))
         x = x1
130      if (.not.(x .le. x2)) goto 132
            call sprinf (memc(el), 255 , st0006)
            call pargr (x)
            call xstrct(memc(el), memc(buf), (6*1023 ))
131         x = x + 1.
            goto 130
132      continue
         call xstrct(st0007, memc(buf), (6*1023 ))
         call xstrct(st0008, memc(buf), (6*1023 ))
         y = y2
140      if (.not.(y .ge. y1)) goto 142
            call sprinf (memc(el), 255 , st0009)
            call pargr (y)
            call xstrct(memc(el), memc(buf), (6*1023 ))
141         y = y - 1.
            goto 140
142      continue
         call xstrct(st0010, memc(buf), (6*1023 ))
         mean = sum / real(npix)
         var = (sum2 - sum * mean) / real(npix - 1)
         if (.not.(var .le. 0)) goto 150
            stdev = 0.0
            goto 151
150      continue
            stdev = sqrt (var)
151      continue
         call sprinf (memc(el), 255 , st0011)
         call pargr (mean)
         call pargr (stdev)
         call xstrct(memc(el), memc(buf), (6*1023 ))
         call xstrct(st0012, memc(buf), (6*1023 ))
         call wcspie (memc(buf))
         call sfree (sp)
100      return
      end
      integer function imgams (im, mw)
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
      integer ct
      double precision r(7 )
      double precision w(7 )
      double precision cd(7 ,7 )
      double precision imgetd
      integer mwsctn
      integer*2 st0001(5)
      integer*2 st0002(5)
      integer*2 st0003(7)
      integer*2 st0004(7)
      integer*2 st0005(10)
      integer*2 st0006(8)
      integer*2 st0007(10)
      save
      integer iyy
      data st0001 / 65, 84, 86, 49, 0/
      data st0002 / 65, 84, 86, 50, 0/
      data st0003 / 65, 84, 77, 49, 95, 49, 0/
      data st0004 / 65, 84, 77, 50, 95, 50, 0/
      data (st0005(iyy),iyy= 1, 8) / 97,109,112,108,105,102,105,101/
      data (st0005(iyy),iyy= 9,10) /114, 0/
      data st0006 /108,111,103,105, 99, 97,108, 0/
      data (st0007(iyy),iyy= 1, 8) / 97,109,112,108,105,102,105,101/
      data (st0007(iyy),iyy= 9,10) /114, 0/
         r(1) = 0.0d0
         r(2) = 0.0d0
         w(1) = imgetd (im, st0001)
         w(2) = imgetd (im, st0002)
         cd(1,1) = imgetd (im, st0003)
         cd(1,2) = 0.0d0
         cd(2,1) = 0.0d0
         cd(2,2) = imgetd (im, st0004)
         call mwnewm (mw, st0005, 2)
         call mwswtd (mw, r, w, cd, 2)
         ct = mwsctn (mw, st0006, st0007, 3)
         call mwsdes (mw)
         imgams = (ct)
         goto 100
100      return
      end
      integer function imgdes (im, mw)
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
      integer ct
      double precision r(7 )
      double precision w(7 )
      double precision cd(7 ,7 )
      double precision imgetd
      integer mwsctn
      integer*2 st0001(5)
      integer*2 st0002(5)
      integer*2 st0003(7)
      integer*2 st0004(7)
      integer*2 st0005(9)
      integer*2 st0006(8)
      integer*2 st0007(9)
      save
      integer iyy
      data st0001 / 68, 84, 86, 49, 0/
      data st0002 / 68, 84, 86, 50, 0/
      data st0003 / 68, 84, 77, 49, 95, 49, 0/
      data st0004 / 68, 84, 77, 50, 95, 50, 0/
      data (st0005(iyy),iyy= 1, 8) /100,101,116,101, 99,116,111,114/
      data (st0005(iyy),iyy= 9, 9) / 0/
      data st0006 /108,111,103,105, 99, 97,108, 0/
      data (st0007(iyy),iyy= 1, 8) /100,101,116,101, 99,116,111,114/
      data (st0007(iyy),iyy= 9, 9) / 0/
         r(1) = 0.0d0
         r(2) = 0.0d0
         w(1) = imgetd (im, st0001)
         w(2) = imgetd (im, st0002)
         cd(1,1) = imgetd (im, st0003)
         cd(1,2) = 0.0d0
         cd(2,1) = 0.0d0
         cd(2,2) = imgetd (im, st0004)
         call mwnewm (mw, st0005, 2)
         call mwswtd (mw, r, w, cd, 2)
         ct = mwsctn (mw, st0006, st0007, 3)
         call mwsdes (mw)
         imgdes = (ct)
         goto 100
100      return
      end
      subroutine imgcos (cp, line, wcsnae, xunits, yunits)
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
      integer cp
      integer line
      integer*2 wcsnae(*)
      integer*2 xunits(*)
      integer*2 yunits(*)
      integer img
      integer co
      integer wp
      integer sp
      integer proj
      integer radecr
      integer xstrcp
      integer skstai
      integer idxstr
      integer sw0001,sw0002
      integer*2 st0001(5)
      integer*2 st0002(5)
      integer*2 st0003(5)
      integer*2 st0004(5)
      integer*2 st0005(5)
      integer*2 st0006(5)
      integer*2 st0007(5)
      integer*2 st0008(5)
      integer*2 st0009(9)
      integer*2 st0010(5)
      integer*2 st0011(5)
      integer*2 st0012(9)
      integer*2 st0013(5)
      integer*2 st0014(5)
      integer*2 st0015(14)
      integer*2 st0016(5)
      integer*2 st0017(5)
      integer*2 st0018(5)
      integer*2 st0019(5)
      integer*2 st0020(2)
      integer*2 st0021(2)
      integer*2 st0022(9)
      integer*2 st0023(3)
      integer*2 st0024(4)
      integer*2 st0025(4)
      integer*2 st0026(5)
      integer*2 st0027(5)
      integer*2 st0028(30)
      integer*2 st0029(4)
      integer*2 st0030(2)
      integer*2 st0031(3)
      integer*2 st0032(114)
      integer*2 st0033(7)
      integer*2 st0034(4)
      integer*2 st0035(4)
      integer*2 st0036(5)
      integer*2 st0037(6)
      integer*2 st0038(9)
      save
      integer iyy
      data st0001 / 32, 32, 82, 65, 0/
      data st0002 / 32, 68,101, 99, 0/
      data st0003 / 69, 76,111,110, 0/
      data st0004 / 69, 76, 97,116, 0/
      data st0005 / 71, 76,111,110, 0/
      data st0006 / 71, 76, 97,116, 0/
      data st0007 / 83, 76,111,110, 0/
      data st0008 / 83, 76, 97,116, 0/
      data (st0009(iyy),iyy= 1, 8) /101, 99,108,105,112,116,105, 99/
      data (st0009(iyy),iyy= 9, 9) / 0/
      data st0010 / 69, 76,111,110, 0/
      data st0011 / 69, 76, 97,116, 0/
      data (st0012(iyy),iyy= 1, 8) /103, 97,108, 97, 99,116,105, 99/
      data (st0012(iyy),iyy= 9, 9) / 0/
      data st0013 / 71, 76,111,110, 0/
      data st0014 / 71, 76, 97,116, 0/
      data (st0015(iyy),iyy= 1, 8) /115,117,112,101,114,103, 97,108/
      data (st0015(iyy),iyy= 9,14) / 97, 99,116,105, 99, 0/
      data st0016 / 83, 76,111,110, 0/
      data st0017 / 83, 76, 97,116, 0/
      data st0018 / 32, 32, 82, 65, 0/
      data st0019 / 32, 68,101, 99, 0/
      data st0020 / 88, 0/
      data st0021 / 89, 0/
      data (st0022(iyy),iyy= 1, 8) / 37,115, 45, 37,115, 45, 37,115/
      data (st0022(iyy),iyy= 9, 9) / 0/
      data st0023 / 69, 81, 0/
      data st0024 / 69, 67, 76, 0/
      data st0025 / 71, 65, 76, 0/
      data st0026 / 83, 71, 65, 76, 0/
      data st0027 / 85, 78, 75, 78, 0/
      data (st0028(iyy),iyy= 1, 8) /124,102,107, 52,124,102,107, 52/
      data (st0028(iyy),iyy= 9,16) / 45,110,111, 45,101,124,102,107/
      data (st0028(iyy),iyy=17,24) / 53,124,105, 99,114,115,124,103/
      data (st0028(iyy),iyy=25,30) / 97,112,112,116,124, 0/
      data st0029 / 70, 75, 53, 0/
      data st0030 / 45, 0/
      data st0031 / 45, 45, 0/
      data (st0032(iyy),iyy= 1, 8) /124,108,105,110,124, 97,122,112/
      data (st0032(iyy),iyy= 9,16) /124,116, 97,110,124,115,105,110/
      data (st0032(iyy),iyy=17,24) /124,115,116,103,124, 97,114, 99/
      data (st0032(iyy),iyy=25,32) /124,122,112,110,124,122,101, 97/
      data (st0032(iyy),iyy=33,40) /124, 97,105,114,124, 99,121,112/
      data (st0032(iyy),iyy=41,48) /124, 99, 97,114,124,109,101,114/
      data (st0032(iyy),iyy=49,56) /124, 99,101, 97,124, 99,111,112/
      data (st0032(iyy),iyy=57,64) /124, 99,111,100,124, 99,111,101/
      data (st0032(iyy),iyy=65,72) /124, 99,111,111,124, 98,111,110/
      data (st0032(iyy),iyy=73,80) /124,112, 99,111,124,103,108,115/
      data (st0032(iyy),iyy=81,88) /124,112, 97,114,124, 97,105,116/
      data (st0032(iyy),iyy=89,96) /124,109,111,108,124, 99,115, 99/
      data (st0032(iyy),iyy=97,104) /124,113,115, 99,124,116,115, 99/
      data (st0032(iyy),iyy=105,112) /124,116,110,120,124,122,112,120/
      data (st0032(iyy),iyy=113,114) /124, 0/
      data st0033 /108,105,110,101, 97,114, 0/
      data st0034 /102,107, 52, 0/
      data st0035 /102,107, 53, 0/
      data st0036 /105, 99,114,115, 0/
      data st0037 /103, 97,112,112,116, 0/
      data (st0038(iyy),iyy= 1, 8) /102,107, 52, 45,110,111, 45,101/
      data (st0038(iyy),iyy= 9, 9) / 0/
         img = memi(cp+3)
         co = memi(img+4)
         wp = memi(img )
         if (.not.(memi(memi(wp+3) +line-1) .eq. 4 )) goto 110
            sw0001=(skstai(co,7))
            goto 120
130         continue
               call xstrcy(st0001, xunits, 32 )
               call xstrcy(st0002, yunits, 32 )
            goto 121
140         continue
               call xstrcy(st0003, xunits, 32 )
               call xstrcy(st0004, yunits, 32 )
            goto 121
150         continue
               call xstrcy(st0005, xunits, 32 )
               call xstrcy(st0006, yunits, 32 )
            goto 121
160         continue
               call xstrcy(st0007, xunits, 32 )
               call xstrcy(st0008, yunits, 32 )
               goto 121
120         continue
               if (sw0001.lt.1.or.sw0001.gt.4) goto 121
               goto (130,140,150,160),sw0001
121         continue
            goto 111
110      continue
         if (.not.(memi(memi(wp+3) +line-1) .eq. 5 )) goto 170
            call xstrcy(memc(memi(wp+4) +(32 *(line-1))), wcsnae, 32 )
            call strlwr (wcsnae)
            if (.not.(xstrcp(wcsnae,st0009) .eq. 0)) goto 180
               call xstrcy(st0010, xunits, 32 )
               call xstrcy(st0011, yunits, 32 )
               goto 181
180         continue
            if (.not.(xstrcp(wcsnae,st0012) .eq. 0)) goto 190
               call xstrcy(st0013, xunits, 32 )
               call xstrcy(st0014, yunits, 32 )
               goto 191
190         continue
            if (.not.(xstrcp(wcsnae,st0015) .eq. 0)) goto 200
               call xstrcy(st0016, xunits, 32 )
               call xstrcy(st0017, yunits, 32 )
               goto 201
200         continue
               call xstrcy(st0018, xunits, 32 )
               call xstrcy(st0019, yunits, 32 )
201         continue
191         continue
181         continue
            goto 171
170      continue
            call xstrcy(st0020, xunits, 32 )
            call xstrcy(st0021, yunits, 32 )
171      continue
111      continue
         if (.not.(memi(memi(wp+3) +line-1) .ne. 4 )) goto 210
            call xstrcy(memc(memi(wp+4) +(32 *(line-1))), wcsnae, 32 )
            goto 211
210      continue
            call smark (sp)
            call salloc (radecr, 255 , 2)
            call salloc (proj, 255 , 2)
            call sprinf (wcsnae, 32 , st0022)
            sw0002=(skstai(co,7))
            goto 220
230         continue
               call pargsr (st0023)
            goto 221
240         continue
               call pargsr (st0024)
            goto 221
250         continue
               call pargsr (st0025)
            goto 221
260         continue
               call pargsr (st0026)
            goto 221
270         continue
               call pargsr (st0027)
               goto 221
220         continue
               if (sw0002.lt.1.or.sw0002.gt.4) goto 270
               goto (230,240,250,260),sw0002
221         continue
            if (.not.(skstai(co,7) .eq. 1)) goto 280
               if (.not.(idxstr(skstai(co,8), memc(radecr), 255 , st0028
     *         ) .le. 0)) goto 290
                  call xstrcy(st0029, memc(radecr), 255 )
290            continue
               call strupr (memc(radecr))
               call pargsr (memc(radecr))
               goto 281
280         continue
               if (.not.(skstai(co,7) .eq. 4)) goto 300
                  call pargsr (st0030)
                  goto 301
300            continue
                  call pargsr (st0031)
301            continue
281         continue
            if (.not.(idxstr(skstai(co,9), memc(proj), 255 , st0032) .le
     *      . 0)) goto 310
               call xstrcy(st0033, memc(proj), 255 )
310         continue
            call strupr (memc(proj))
            call pargsr (memc(proj))
            call sfree (sp)
211      continue
         if (.not.(xstrcp(wcsnae, st0034) .eq. 0 .or. xstrcp(wcsnae, 
     *   st0035) .eq. 0 .or. xstrcp(wcsnae, st0036) .eq. 0 .or. xstrcp(
     *   wcsnae, st0037) .eq. 0 .or. xstrcp(wcsnae, st0038) .eq. 0)) 
     *   goto 320
            call strupr (wcsnae)
            goto 321
320      continue
         if (.not.((wcsnae(1).ge.97.and.wcsnae(1).le.122))) goto 330
            wcsnae(1) = (wcsnae(1)+65-97)
330      continue
321      continue
100      return
      end
      subroutine imgcot (cp, line, xval, yval, xc, yc)
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
      integer cp
      integer line
      double precision xval
      double precision yval
      integer*2 xc(*)
      integer*2 yc(*)
      integer img
      integer co
      integer wp
      integer*2 xfmt(32 +1)
      integer*2 yfmt(32 +1)
      integer skstai
      logical streq
      integer*2 st0001(7)
      integer*2 st0002(7)
      integer*2 st0003(9)
      integer*2 st0004(9)
      integer*2 st0005(14)
      integer*2 st0006(3)
      integer*2 st0007(5)
      integer*2 st0008(5)
      integer*2 st0009(7)
      integer*2 st0010(7)
      integer*2 st0011(5)
      integer*2 st0012(5)
      integer*2 st0013(3)
      integer*2 st0014(7)
      integer*2 st0015(7)
      save
      integer iyy
      data st0001 / 37, 49, 48, 46, 50,102, 0/
      data st0002 / 37, 49, 48, 46, 50,102, 0/
      data (st0003(iyy),iyy= 1, 8) /101, 99,108,105,112,116,105, 99/
      data (st0003(iyy),iyy= 9, 9) / 0/
      data (st0004(iyy),iyy= 1, 8) /103, 97,108, 97, 99,116,105, 99/
      data (st0004(iyy),iyy= 9, 9) / 0/
      data (st0005(iyy),iyy= 1, 8) /115,117,112,101,114,103, 97,108/
      data (st0005(iyy),iyy= 9,14) / 97, 99,116,105, 99, 0/
      data st0006 / 37,104, 0/
      data st0007 / 37, 46, 50, 72, 0/
      data st0008 / 37, 46, 49,104, 0/
      data st0009 / 37, 49, 48, 46, 50,102, 0/
      data st0010 / 37, 49, 48, 46, 50,102, 0/
      data st0011 / 37, 46, 50, 72, 0/
      data st0012 / 37, 46, 49,104, 0/
      data st0013 / 37,104, 0/
      data st0014 / 37, 49, 48, 46, 50,102, 0/
      data st0015 / 37, 49, 48, 46, 50,102, 0/
         img = memi(cp+3)
         co = memi(img+4)
         wp = memi(img )
         if (.not.(memi(memi(wp+5) +line-1) .eq. 1 )) goto 110
            if (.not.(memi(img+3) .eq. 0)) goto 120
               call xstrcy(st0001, xfmt, 32 )
               call xstrcy(st0002, yfmt, 32 )
               goto 121
120         continue
               if (.not.(memi(memi(wp+3) +line-1) .eq. 4 .or. memi(memi(
     *         wp+3) +line-1) .eq. 5 )) goto 130
                  if (.not.(streq(memc(memi(wp+4) +(32 *(line-1))),
     *            st0003) .or. streq(memc(memi(wp+4) +(32 *(line-1))),
     *            st0004) .or. streq(memc(memi(wp+4) +(32 *(line-1))),
     *            st0005))) goto 140
                     call xstrcy(st0006, xfmt, 32 )
                     goto 141
140               continue
                     call xstrcy(st0007, xfmt, 32 )
141               continue
                  call xstrcy(st0008, yfmt, 32 )
                  goto 131
130            continue
                  call xstrcy(st0009, xfmt, 32 )
                  call xstrcy(st0010, yfmt, 32 )
131            continue
121         continue
            goto 111
110      continue
         if (.not.(memi(memi(wp+5) +line-1) .eq. 2 )) goto 150
            if (.not.(skstai(co, 7) .eq. 1)) goto 160
               call xstrcy(st0011, xfmt, 32 )
               goto 161
160         continue
               call xstrcy(st0012, xfmt, 32 )
161         continue
            call xstrcy(st0013, yfmt, 32 )
            goto 151
150      continue
            call xstrcy(st0014, xfmt, 32 )
            call xstrcy(st0015, yfmt, 32 )
151      continue
111      continue
         call sprinf (xc, 32 , xfmt)
         if (.not.(memi(memi(wp+5) +line-1) .ne. 4 )) goto 170
            call pargd (xval)
            goto 171
170      continue
            call pargd (((xval)/57.295779513082320877))
171      continue
         call sprinf (yc, 32 , yfmt)
         if (.not.(memi(memi(wp+5) +line-1) .ne. 4 )) goto 180
            call pargd (yval)
            goto 181
180      continue
            call pargd (((yval)/57.295779513082320877))
181      continue
100      return
      end
      subroutine imgged (img, x, y, system, wcsnae, wx, wy)
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
      integer img
      double precision x
      double precision y
      integer system
      double precision wx
      double precision wy
      integer*2 wcsnae(*)
      double precision ox
      double precision oy
      real epoch
      integer im
      integer co
      integer nco
      integer*2 buf(1023 +1)
      integer stat
      real imgetr
      integer imaccf
      integer skstai
      integer skdecr
      logical streq
      integer sw0001
      logical xerflg
      common /xercom/ xerflg
      integer*2 st0001(9)
      integer*2 st0002(6)
      integer*2 st0003(6)
      integer*2 st0004(6)
      integer*2 st0005(8)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /101, 99,108,105,112,116,105, 99/
      data (st0001(iyy),iyy= 9, 9) / 0/
      data st0002 /103, 97,112,112,116, 0/
      data st0003 / 69, 80, 79, 67, 72, 0/
      data st0004 / 69, 80, 79, 67, 72, 0/
      data st0005 / 37,115, 32, 37, 46, 49,102, 0/
         im = memi(img+1)
         co = memi(img+4)
         wx = x
         wy = y
         sw0001=(system)
         goto 110
120      continue
            wx = x
            wy = y
         goto 111
130      continue
            if (.not.(memi(img+6) .ne. 0)) goto 140
               call mwc2td (memi(img+6) , x, y, wx, wy)
140         continue
         goto 111
150      continue
            if (.not.(memi(img+5) .ne. 0)) goto 160
               call mwc2td (memi(img+5) , x, y, wx, wy)
160         continue
         goto 111
170      continue
            if (.not.(memi(img+7) .ne. 0)) goto 180
               call mwc2td (memi(img+7) , x, y, wx, wy)
180         continue
         goto 111
190      continue
         goto 111
200      continue
            if (.not.(memi(img+8) .ne. 0)) goto 210
               call mwc2td (memi(img+8) , x, y, wx, wy)
210         continue
         goto 111
220      continue
            if (.not.(streq (wcsnae, st0001) .or. streq (wcsnae, st0002)
     *      )) goto 230
               if (.not.(imaccf (im, st0003) .eq. 1)) goto 240
                  epoch = imgetr (im, st0004)
                  if (xerflg) goto 100
                  if (.not.(epoch .eq. 0.0 .or. ((epoch).eq.1.6e38))) 
     *            goto 250
                     epoch = 1950.0
250               continue
                  goto 241
240            continue
                  epoch = 1950.0
241            continue
               call sprinf (buf, 1023 , st0005)
               call pargsr (wcsnae)
               call pargr (epoch)
               goto 231
230         continue
               call xstrcy(wcsnae, buf, 1023 )
231         continue
            stat = skdecr (buf, nco, co)
            if (.not.(stat .ne. -1)) goto 260
               if (.not.(memi(img+5) .ne. 0)) goto 270
                  call mwc2td (memi(img+5) , x, y, ox, oy)
270            continue
               call sklltn (co, nco, ((ox)/57.295779513082320877), ((oy)
     *         /57.295779513082320877), 1.6d308, 1.6d308, 0.0d0, 0.0d0, 
     *         wx, wy)
               if (.not.(skstai(co,11) .lt. skstai(co,10))) goto 280
                  wx = ((wy)*57.295779513082320877)
                  wy = ((wx)*57.295779513082320877)
                  goto 281
280            continue
                  wx = ((wx)*57.295779513082320877)
                  wy = ((wy)*57.295779513082320877)
281            continue
260         continue
         goto 111
290      continue
         goto 111
300      continue
            wx = x
            wy = y
            goto 111
110      continue
            if (sw0001.lt.1.or.sw0001.gt.9) goto 300
            goto (120,300,130,150,220,170,190,200,290),sw0001
111      continue
100      return
      end
c     sprinf  sprintf
c     temple  template
c     skstad  sk_statd
c     wcspie  wcspix_message
c     imgser  img_send_header
c     radecs  radecsys
c     stropn  stropen
c     skstai  sk_stati
c     skdecr  sk_decwstr
c     imgcae  img_cache
c     mwstai  mw_stati
c     getlie  getline
c     skdecm  sk_decim
c     imgses  img_send_compass
c     imgseo  img_send_wcsinfo
c     ximalt  xim_alert
c     wcsnae  wcsname
c     bpmpix  bpm_pix
c     mwc2td  mw_c2trand
c     imgune  img_uncache
c     imgwcn  img_wcstran
c     mwswtd  mw_swtermd
c     sklltn  sk_lltran
c     mwsctn  mw_sctran
c     imunmp  imunmap
c     imgwct  img_wcslist
c     keywfr  keyw_filter
c     imgged  img_get_coord
c     fprinf  fprintf
c     imgint  img_init
c     imofnu  imofnlu
c     dspmmp  ds_pmmap
c     imggea  img_get_data
c     imgseb  img_send_pixtab
c     imgcos  img_coord_labels
c     imgcot  img_coord_fmt
c     imgobo  img_objinfo
c     putlie  putline
c     imgdes  img_det_wcs
c     hdrsie  hdr_size
c     radecr  radecstr
c     imgams  img_amp_wcs
c     pargsr  pargstr
c     mwcloe  mw_close
c     mwnewm  mw_newsystem
c     wcsgfm  wcs_gfterm
c     fpequd  fp_equald
c     mwsdes  mw_sdefwcs
