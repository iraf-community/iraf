      subroutine unkint (cp, wp)
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
      logical xerpop
      logical xerflg
      common /xercom/ xerflg
      save
         if (.not.(memi(cp+3) .eq. 0)) goto 110
            call xerpsh
            call xcallc(memi(cp+3) , 1, 10 )
            if (.not.xerpop()) goto 120
               goto 100
120         continue
110      continue
         memi(memi(cp+3) ) = wp
100      return
      end
      subroutine unkcae (cp, objid, regid, ref)
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
      save
         memi(cp) = objid
         memi(cp+1) = regid
         memi(cp+4) = memi(cp+4) + 1
         call xstrcy(ref, memc((((cp+6)-1)*2+1)) , 128)
100      return
      end
      subroutine unkune (cp, id)
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
      integer*2 st0001(1)
      save
      data st0001 / 0/
         memi(cp) = 0
         memi(cp+4) = 0
         call xstrcy(st0001, memc((((cp+6)-1)*2+1)) , 255 )
         call xmfree(memi(cp+3) , 10 )
         memi(cp+3) = 0
100      return
      end
      subroutine unkwcn (cp, id, x, y)
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
      integer wp
      integer i
      integer*2 buf(1023 +1)
      integer*2 msg(1023 +1)
      integer*2 st0001(37)
      integer*2 st0002(27)
      integer*2 st0003(37)
      integer*2 st0004(5)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /119, 99,115,116,114, 97,110, 32/
      data (st0001(iyy),iyy= 9,16) /123, 32,111, 98,106,101, 99,116/
      data (st0001(iyy),iyy=17,24) / 32, 37,100, 32,125, 32,123, 32/
      data (st0001(iyy),iyy=25,32) /114,101,103,105,111,110, 32, 37/
      data (st0001(iyy),iyy=33,37) /100, 32,125, 32, 0/
      data (st0002(iyy),iyy= 1, 8) /123, 32,112,105,120,118, 97,108/
      data (st0002(iyy),iyy= 9,16) / 32, 48, 46, 48, 32,125, 32,123/
      data (st0002(iyy),iyy=17,24) / 32, 98,112,109, 32, 48, 32,125/
      data (st0002(iyy),iyy=25,27) / 32, 10, 0/
      data (st0003(iyy),iyy= 1, 8) /123, 99,111,111,114,100, 32,123/
      data (st0003(iyy),iyy= 9,16) / 37, 57,115,125, 32,123, 37, 49/
      data (st0003(iyy),iyy=17,24) / 50,103,125, 32,123, 37, 49, 50/
      data (st0003(iyy),iyy=25,32) /103,125, 32,123, 88,125, 32,123/
      data (st0003(iyy),iyy=33,37) / 89,125,125, 10, 0/
      data st0004 / 85, 78, 75, 78, 0/
         wp = memi(memi(cp+3) )
         call aclrc (msg, 1023 )
         call sprinf (msg, 1023 , st0001)
         call pargi (memi(cp) )
         call pargi (memi(cp+1) )
         call xstrct(st0002, msg, 1023 )
         i=1
110      if (.not.(i .le. 4 )) goto 112
            call sprinf (buf, 1023 , st0003)
            call pargsr (st0004)
            call pargr (x)
            call pargr (y)
            call xstrct(buf, msg, 1023 )
111         i=i+1
            goto 110
112      continue
         call wcspie (msg)
100      return
      end
      subroutine unkwct (cp, id)
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
      save
100      return
      end
      subroutine unkgea (cp, id, x, y, pixval)
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
      real pixval
      integer wp
      integer pix
      integer size
      integer x1
      integer x2
      integer y1
      integer y2
      save
         wp = memi(memi(cp+3) )
         size = memi(wp+1)
         x1 = x - size / 2 + 0.5
         x2 = x + size / 2 + 0.5
         y1 = y - size / 2 + 0.5
         y2 = y + size / 2 + 0.5
         pixval = 0.0
         if (.not.(size .gt. 1)) goto 110
            call xcallc(pix, size * size, 6)
            call imgseb (memr(pix), size, x1, x2, y1, y2)
            call xmfree(pix, 6)
110      continue
100      return
      end
      subroutine unkobo (cp, id, temple)
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
      integer sp
      integer buf
      integer*2 st0001(25)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) / 99,111,109,112, 97,115,115, 32/
      data (st0001(iyy),iyy= 9,16) / 37,100, 32, 48, 46, 48, 32, 45/
      data (st0001(iyy),iyy=17,24) / 49, 32, 49, 32, 88, 32, 89, 0/
      data (st0001(iyy),iyy=25,25) / 0/
         call smark (sp)
         call salloc (buf, 1023 , 2)
         call aclrc (memc(buf), 1023 )
         call sprinf (memc(buf), 1023 , st0001)
         call pargi (memi(cp) )
         call wcspie (memc(buf))
         call sfree (sp)
100      return
      end
c     sprinf  sprintf
c     temple  template
c     wcspie  wcspix_message
c     unkwct  unk_wcslist
c     unkint  unk_init
c     unkobo  unk_objinfo
c     unkcae  unk_cache
c     imgseb  img_send_pixtab
c     unkune  unk_uncache
c     unkwcn  unk_wcstran
c     pargsr  pargstr
c     unkgea  unk_getdata
