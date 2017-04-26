      integer function dspmmp (pmname, refim)
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
      integer refim
      integer*2 pmname(*)
      integer im
      integer*2 fname(255 +1)
      integer nowhie
      integer errcoe
      logical streq
      integer impmmp
      integer dspmip
      logical xerpop
      logical xerflg
      common /xercom/ xerflg
      integer sw0001
      integer*2 st0001(6)
      integer*2 st0002(4)
      integer*2 st0003(4)
      save
      data st0001 / 69, 77, 80, 84, 89, 0/
      data st0002 / 66, 80, 77, 0/
      data st0003 / 66, 80, 77, 0/
         if (.not.(nowhie (pmname, fname, 255 ) .eq. 0)) goto 110
            dspmmp = (0)
            goto 100
110      continue
         if (.not.(streq (fname, st0001))) goto 120
            dspmmp = (0)
            goto 100
120      continue
         if (.not.(fname(1) .eq. 33)) goto 130
            call xerpsh
            call imgstr (refim, fname(2), fname, 255 )
            if (.not.xerpop()) goto 140
               fname(1) = 0
140         continue
            goto 131
130      continue
         if (.not.(streq (fname, st0002))) goto 150
            call xerpsh
            call imgstr (refim, st0003, fname, 255 )
            if (.not.xerpop()) goto 160
               dspmmp = (0)
               goto 100
160         continue
150      continue
131      continue
         call xerpsh
         im = impmmp (fname, 1 , 0)
         if (.not.xerpop()) goto 170
            sw0001=(errcoe())
            goto 180
190         continue
               im = dspmip (fname, refim)
               if (xerflg) goto 100
            goto 181
200         continue
               call erract (2 )
               if (xerflg) goto 100
               goto 181
180         continue
               if (sw0001.eq.743) goto 190
               if (sw0001.eq.921) goto 190
               goto 200
181         continue
170      continue
         call xerpsh
         call dsmath (im, refim)
         if (.not.xerpop()) goto 210
            call erract (3 )
            if (xerflg) goto 100
210      continue
         dspmmp = (im)
         goto 100
100      return
      end
      integer function dspmip (pmname, refim)
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
      integer refim
      integer*2 pmname(*)
      integer i
      integer ndim
      integer npix
      integer val
      integer sp
      integer v1
      integer v2
      integer imin
      integer imout
      integer pm
      integer mw
      integer data
      integer imgnli
      integer immap
      integer pmnewk
      integer impmmo
      integer imgl1i
      integer mwopem
      logical xerflg
      common /xercom/ xerflg
      save
         call smark (sp)
         call salloc (v1, 7 , 5)
         call salloc (v2, 7 , 5)
         call amovkl (int(1), meml(v1), 7 )
         call amovkl (int(1), meml(v2), 7 )
         imin = immap (pmname, 1 , 0)
         if (xerflg) goto 100
         pm = pmnewk (imin, 27)
         ndim = memi(imin+200 +7)
         npix = meml(imin+200 +1+8-1)
110      if (.not.(imgnli (imin, data, meml(v1)) .ne. -2)) goto 111
            do 120 i = 0, npix-1 
               val = memi(data+i)
               if (.not.(val .lt. 0)) goto 130
                  memi(data+i) = 0
130            continue
120         continue
121         continue
            call pmplpi (pm, meml(v2), memi(data), 0, npix, 12 )
            call amovl (meml(v1), meml(v2), ndim)
            goto 110
111      continue
         imout = impmmo (pm, imin)
         data = imgl1i (imout)
         mw = mwopem (imin)
         if (xerflg) goto 100
         call mwsavm (mw, imout)
         call mwcloe (mw)
         call imunmp (imin)
         call sfree (sp)
         dspmip = (imout)
         goto 100
100      return
      end
      subroutine dsmath (im, refim)
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
      integer refim
      integer i
      integer j
      integer k
      integer l
      integer i1
      integer i2
      integer j1
      integer j2
      integer nc
      integer nl
      integer ncpm
      integer nlpm
      integer nx
      integer val
      double precision x1
      double precision x2
      double precision y1
      double precision y2
      double precision lt(6)
      double precision lt1(6)
      double precision lt2(6)
      integer*4 vold(7 )
      integer*4 vnew(7 )
      integer pm
      integer pmnew
      integer imnew
      integer mw
      integer ctx
      integer cty
      integer bufref
      integer bufpm
      integer imstai
      integer plopen
      integer mwopem
      integer impmmo
      integer imgl1i
      integer mwsctn
      logical pmempy
      logical pmliny
      logical xerflg
      common /xercom/ xerflg
      integer*2 st0001(40)
      integer*2 st0002(8)
      integer*2 st0003(9)
      integer*2 st0004(8)
      integer*2 st0005(9)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) / 73,109, 97,103,101, 32, 97,110/
      data (st0001(iyy),iyy= 9,16) /100, 32,109, 97,115,107, 32,104/
      data (st0001(iyy),iyy=17,24) / 97,118,101, 32, 97, 32,114,101/
      data (st0001(iyy),iyy=25,32) /108, 97,116,105,118,101, 32,114/
      data (st0001(iyy),iyy=33,40) /111,116, 97,116,105,111,110, 0/
      data st0002 /108,111,103,105, 99, 97,108, 0/
      data (st0003(iyy),iyy= 1, 8) /112,104,121,115,105, 99, 97,108/
      data (st0003(iyy),iyy= 9, 9) / 0/
      data st0004 /108,111,103,105, 99, 97,108, 0/
      data (st0005(iyy),iyy= 1, 8) /112,104,121,115,105, 99, 97,108/
      data (st0005(iyy),iyy= 9, 9) / 0/
         if (.not.(im .eq. 0)) goto 110
            goto 100
110      continue
         nc = meml(refim+200 +1+8-1)
         nl = meml(refim+200 +2+8-1)
         ncpm = meml(im+200 +1+8-1)
         nlpm = meml(im+200 +2+8-1)
         pm = imstai (im, 16 )
         if (.not.(pmempy(pm) .and. nc .eq. ncpm .and. nl .eq. nlpm)) 
     *   goto 120
            goto 100
120      continue
         mw = mwopem (im)
         if (xerflg) goto 100
         call mwgltd (mw, lt, lt(5), 2)
         call mwcloe (mw)
         mw = mwopem (refim)
         if (xerflg) goto 100
         call mwgltd (mw, lt2, lt2(5), 2)
         call mwcloe (mw)
         call mwinvd (lt, lt1, 2)
         call mwmmud (lt1, lt2, lt, 2)
         call mwvmud (lt, lt(5), lt(5), 2)
         lt(5) = lt2(5) - lt(5)
         lt(6) = lt2(6) - lt(6)
         do 130 i = 1, 6
            lt(i) = nint (1d6 * (lt(i)-int(lt(i)))) / 1d6 + int(lt(i))
130      continue
131      continue
         if (.not.(lt(2) .ne. 0. .or. lt(3) .ne. 0.)) goto 140
            call xerror(1, st0001)
            if (xerflg) goto 100
140      continue
         if (.not.(lt(1) .eq. 1d0 .and. lt(4) .eq. 1d0 .and. lt(5) .eq. 
     *   0d0 .and. lt(6) .eq. 0d0)) goto 150
            goto 100
150      continue
         mw = mwopem (im)
         if (xerflg) goto 100
         call mwsltd (mw, lt, lt(5), 2)
         ctx = mwsctn (mw, st0002, st0003, 1)
         cty = mwsctn (mw, st0004, st0005, 2)
         pmnew = plopen(0)
         if (xerflg) goto 100
         call plssie(pmnew, 2, meml(refim+200 +1+8-1) , 27)
         imnew = impmmo (pmnew, 0)
         bufref = imgl1i (imnew)
         call mwctrd (ctx, 1-0.5d0, x1, 1)
         call mwctrd (ctx, nc+0.5d0, x2, 1)
         i1 = max (1, nint(min(x1,x2)+1d-5))
         i2 = min (ncpm, nint(max(x1,x2)-1d-5))
         call mwctrd (cty, 1-0.5d0, y1, 1)
         call mwctrd (cty, nl+0.5d0, y2, 1)
         j1 = max (1, nint(min(y1,y2)+1d-5))
         j2 = min (nlpm, nint(max(y1,y2)-1d-5))
         if (.not.(i1 .le. i2 .and. j1 .le. j2)) goto 160
            nx = i2 - i1 + 1
            call xmallc(bufpm, nx, 4)
            call xmallc(bufref, nc, 4)
            vold(1) = i1
            vnew(1) = 1
            do 170 j = 1, nl 
               call mwctrd (cty, j-0.5d0, y1, 1)
               call mwctrd (cty, j+0.5d0, y2, 1)
               j1 = max (1, nint(min(y1,y2)+1d-5))
               j2 = min (nlpm, nint(max(y1,y2)-1d-5))
               if (.not.(j2 .lt. j1)) goto 180
                  goto 170
180            continue
               vnew(2) = j
               call aclri (memi(bufref), nc)
               do 190 l = j1, j2 
                  vold(2) = l
                  if (.not.(.not.pmliny (pm, vold))) goto 200
                     goto 190
200               continue
                  call pmglpi (pm, vold, memi(bufpm), 0, nx, 0)
                  do 210 i = 1, nc 
                     call mwctrd (ctx, i-0.5d0, x1, 1)
                     call mwctrd (ctx, i+0.5d0, x2, 1)
                     i1 = max (1, nint(min(x1,x2)+1d-5))
                     i2 = min (ncpm, nint(max(x1,x2)-1d-5))
                     if (.not.(i2 .lt. i1)) goto 220
                        goto 210
220                  continue
                     val = memi(bufref+i-1)
                     do 230 k = i1-vold(1), i2-vold(1)
                        val = max (val, memi(bufpm+k))
230                  continue
231                  continue
                     memi(bufref+i-1) = val
210               continue
211               continue
190            continue
191            continue
               call pmplpi (pmnew, vnew, memi(bufref), 0, nc, 12 )
170         continue
171         continue
            call xmfree(bufref, 4)
            call xmfree(bufpm, 4)
160      continue
         call mwcloe (mw)
         call imunmp (im)
         im = imnew
         call imseti (im, 16 , pmnew)
100      return
      end
c     pmliny  pm_linenotempty
c     mwmmud  mw_mmuld
c     errcoe  errcode
c     mwsltd  mw_sltermd
c     mwinvd  mw_invertd
c     impmmo  im_pmmapo
c     plssie  pl_ssize
c     mwctrd  mw_ctrand
c     pmempy  pm_empty
c     mwvmud  mw_vmuld
c     dsmath  ds_match
c     plopen  pl_open
c     mwsavm  mw_saveim
c     mwopem  mw_openim
c     imunmp  imunmap
c     mwsctn  mw_sctran
c     impmmp  im_pmmap
c     dspmip  ds_pmimmap
c     dspmmp  ds_pmmap
c     imstai  imstati
c     nowhie  nowhite
c     mwcloe  mw_close
c     pmnewk  pm_newmask
c     mwgltd  mw_gltermd
