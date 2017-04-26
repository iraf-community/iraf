      subroutine wcsgfm (mw, crpix, crval, cd, ndim)
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
      integer ndim
      double precision crpix(ndim)
      double precision crval(ndim)
      double precision cd(ndim,ndim)
      integer sp
      integer r
      integer wcd
      integer ltv
      integer ltm
      integer iltm
      integer alert
      integer errmsg
      integer i
      integer errcoe
      integer errget
      logical xerpop
      logical xerflg
      common /xercom/ xerflg
      integer*2 st0001(8)
      integer*2 st0002(26)
      integer*2 st0003(1)
      integer*2 st0004(1)
      save
      integer iyy
      data st0001 / 37,115, 10, 34, 37,115, 34, 0/
      data (st0002(iyy),iyy= 1, 8) / 69,114,114,111,114, 32,100,101/
      data (st0002(iyy),iyy= 9,16) / 99,111,100,105,110,103, 32,105/
      data (st0002(iyy),iyy=17,24) /109, 97,103,101, 32, 87, 67, 83/
      data (st0002(iyy),iyy=25,26) / 58, 0/
      data st0003 / 0/
      data st0004 / 0/
         call smark (sp)
         call salloc (r, ndim, 7)
         call salloc (wcd, ndim * ndim, 7)
         call salloc (ltv, ndim, 7)
         call salloc (ltm, ndim * ndim, 7)
         call salloc (iltm, ndim * ndim, 7)
         call xerpsh
         call mwgwtd (mw, memd(r), crval, memd(wcd), ndim)
         if (xerflg) goto 112
         call mwgltd (mw, memd(ltm), memd(ltv), ndim)
         if (xerflg) goto 112
         call mwvmud (memd(ltm), memd(r), crpix, ndim)
         call aaddd (crpix, memd(ltv), crpix, ndim)
         call mwinvd (memd(ltm), memd(iltm), ndim)
         call mwmmud (memd(wcd), memd(iltm), cd, ndim)
112      if (.not.xerpop()) goto 110
            call salloc (alert, 1023 , 2)
            call salloc (errmsg, 1023 , 2)
            call aclrd (cd, ndim*ndim)
            i=1
120         if (.not.(i .le. ndim)) goto 122
               crpix(i) = 1.0d0
               crval(i) = 1.0d0
               cd(i,i) = 1.0d0
121            i=i+1
               goto 120
122         continue
            errcoe = errget (memc(errmsg), 1023 )
            call sprinf (memc(alert), 255 , st0001)
            call pargsr (st0002)
            call pargsr (memc(errmsg))
            call ximalt (memc(alert), st0003, st0004)
110      continue
         call sfree (sp)
100      return
      end
c     sprinf  sprintf
c     mwinvd  mwinvertd
c     mwvmud  mwvmuld
c     errcoe  errcode
c     mwgwtd  mw_gwtermd
c     ximalt  xim_alert
c     mwmmud  mwmmuld
c     pargsr  pargstr
c     wcsgfm  wcs_gfterm
c     mwgltd  mw_gltermd
