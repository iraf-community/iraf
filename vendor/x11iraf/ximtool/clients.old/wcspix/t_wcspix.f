      subroutine twcspx ()
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
      integer wp
      integer len
      integer discot
      integer ncmd
      integer*2 socket(255 +1)
      integer*2 cmd(255 +1)
      integer*2 messae(1023 +1)
      integer*2 buf(12 +1)
      integer objid
      integer regid
      real x
      real y
      integer*2 ref(255 +1)
      integer*2 temple(1023 +1)
      integer*2 param(255 +1)
      logical debug
      integer*4 clktie
      integer wpinit
      integer envges
      integer envgei
      integer strdic
      integer ximcot
      integer wpread
      integer ximinr
      logical xerpop
      logical xerflg
      common /xercom/ xerflg
      integer sw0001
      integer*2 st0001(7)
      integer*2 st0002(17)
      integer*2 st0003(7)
      integer*2 st0004(5)
      integer*2 st0005(13)
      integer*2 st0006(28)
      integer*2 st0007(8)
      integer*2 st0008(22)
      integer*2 st0009(73)
      integer*2 st0010(31)
      integer*2 st0011(35)
      integer*2 st0012(41)
      integer*2 st0013(8)
      integer*2 st0014(16)
      integer*2 st0015(38)
      integer*2 st0016(8)
      integer*2 st0017(25)
      integer*2 st0018(16)
      integer*2 st0019(27)
      integer*2 st0020(30)
      save
      integer iyy
      data st0001 / 73, 83, 77, 68, 69, 86, 0/
      data (st0002(iyy),iyy= 1, 8) /117,110,105,120, 58, 47,116,109/
      data (st0002(iyy),iyy= 9,16) /112, 47, 46, 73, 83, 77, 37,100/
      data (st0002(iyy),iyy=17,17) / 0/
      data st0003 /119, 99,115,112,105,120, 0/
      data st0004 /116,101,120,116, 0/
      data (st0005(iyy),iyy= 1, 8) / 87, 67, 83, 80, 73, 88, 95, 68/
      data (st0005(iyy),iyy= 9,13) / 69, 66, 85, 71, 0/
      data (st0006(iyy),iyy= 1, 8) /105,110,102,111, 32,123, 32, 37/
      data (st0006(iyy),iyy= 9,16) /115, 58, 32, 87, 67, 83, 80, 73/
      data (st0006(iyy),iyy=17,24) / 88, 32, 67,111,110,110,101, 99/
      data (st0006(iyy),iyy=25,28) /116,125, 10, 0/
      data st0007 /105,115,109, 95,109,115,103, 0/
      data (st0008(iyy),iyy= 1, 8) /109,101,115,115, 97,103,101, 58/
      data (st0008(iyy),iyy= 9,16) / 32, 39, 37,115, 39, 32,108,101/
      data (st0008(iyy),iyy=17,22) /110, 61, 37,100, 10, 0/
      data (st0009(iyy),iyy= 1, 8) /124,115,101,116,124,103,101,116/
      data (st0009(iyy),iyy= 9,16) /124,113,117,105,116,124,105,110/
      data (st0009(iyy),iyy=17,24) /105,116,105, 97,108,105,122,101/
      data (st0009(iyy),iyy=25,32) /124, 99, 97, 99,104,101,124,117/
      data (st0009(iyy),iyy=33,40) /110, 99, 97, 99,104,101, 9, 9/
      data (st0009(iyy),iyy=41,48) / 9, 32,124,119, 99,115,116,114/
      data (st0009(iyy),iyy=49,56) / 97,110,124,119, 99,115,108,105/
      data (st0009(iyy),iyy=57,64) /115,116,124,111, 98,106,105,110/
      data (st0009(iyy),iyy=65,72) /102,111,124,100,101, 98,117,103/
      data (st0009(iyy),iyy=73,73) / 0/
      data (st0010(iyy),iyy= 1, 8) /105,110,102,111, 32,123, 32, 37/
      data (st0010(iyy),iyy= 9,16) /115, 58, 32, 87, 67, 83, 80, 73/
      data (st0010(iyy),iyy=17,24) / 88, 32, 73,110,105,116,105, 97/
      data (st0010(iyy),iyy=25,31) /108,105,122,101,125, 10, 0/
      data (st0011(iyy),iyy= 1, 8) / 99, 97, 99,104,101, 58, 32,111/
      data (st0011(iyy),iyy= 9,16) / 98,106,105,100, 61, 37,100, 32/
      data (st0011(iyy),iyy=17,24) /114,101,103,105,100, 61, 37,100/
      data (st0011(iyy),iyy=25,32) / 32,114,101,102, 61, 39, 37,115/
      data (st0011(iyy),iyy=33,35) / 39, 10, 0/
      data (st0012(iyy),iyy= 1, 8) /105,110,102,111, 32,123, 32, 37/
      data (st0012(iyy),iyy= 9,16) /115, 58, 32, 87, 67, 83, 80, 73/
      data (st0012(iyy),iyy=17,24) / 88, 32, 67, 97, 99,104,101, 32/
      data (st0012(iyy),iyy=25,32) / 32, 32,111, 98,106,105,100, 61/
      data (st0012(iyy),iyy=33,40) / 37, 51,100, 32, 37,115,125, 10/
      data (st0012(iyy),iyy=41,41) / 0/
      data st0013 /105,115,109, 95,109,115,103, 0/
      data (st0014(iyy),iyy= 1, 8) /117,110, 99, 97, 99,104,101, 58/
      data (st0014(iyy),iyy= 9,16) / 32,105,100, 61, 37,100, 10, 0/
      data (st0015(iyy),iyy= 1, 8) /105,110,102,111, 32,123, 32, 37/
      data (st0015(iyy),iyy= 9,16) /115, 58, 32, 87, 67, 83, 80, 73/
      data (st0015(iyy),iyy=17,24) / 88, 32, 85,110, 99, 97, 99,104/
      data (st0015(iyy),iyy=25,32) /101, 32,111, 98,106,105,100, 61/
      data (st0015(iyy),iyy=33,38) / 37, 51,100,125, 10, 0/
      data st0016 /105,115,109, 95,109,115,103, 0/
      data (st0017(iyy),iyy= 1, 8) /119, 99,115,116,114, 97,110, 58/
      data (st0017(iyy),iyy= 9,16) / 32,105,100, 61, 37,100, 32, 32/
      data (st0017(iyy),iyy=17,24) / 40, 37,103, 44, 37,103, 41, 10/
      data (st0017(iyy),iyy=25,25) / 0/
      data (st0018(iyy),iyy= 1, 8) /119, 99,115,108,105,115,116, 58/
      data (st0018(iyy),iyy= 9,16) / 32,105,100, 61, 37,100, 10, 0/
      data (st0019(iyy),iyy= 1, 8) /111, 98,106,105,110,102,111, 58/
      data (st0019(iyy),iyy= 9,16) / 32,105,100, 61, 37,100, 32, 32/
      data (st0019(iyy),iyy=17,24) /116,101,109,112, 61, 39, 37,115/
      data (st0019(iyy),iyy=25,27) / 39, 10, 0/
      data (st0020(iyy),iyy= 1, 8) / 73, 83, 77, 32,100,101,102, 97/
      data (st0020(iyy),iyy= 9,16) /117,108,116, 58, 32,108,101,110/
      data (st0020(iyy),iyy=17,24) / 61, 37,100, 32,109,115,103, 61/
      data (st0020(iyy),iyy=25,30) / 39, 37,115, 39, 10, 0/
         call aclrc (messae, 1023 )
         call aclrc (cmd, 255 )
         call aclrc (socket, 255 )
         if (.not.(envges (st0001, socket, 255 ) .le. 0).and.(.not.
     *   xerflg)) goto 110
         if (xerflg) goto 100
            call xstrcy(st0002, socket, 255 )
110      continue
         if (.not.(ximcot (socket, st0003, st0004) .eq. -1)) goto 120
            goto 100
120      continue
         if (.not.(ximinr() .eq. -1)) goto 130
            goto 100
130      continue
         wp = wpinit ()
         call xerpsh
         memi(wp+6) = envgei (st0005)
         if (.not.xerpop()) goto 140
            memi(wp+6) = 0
140      continue
         call wpcnve (clktie(0), buf, 12 )
         call sprinf (messae, 1023 , st0006)
         call pargsr (buf)
         call ximmee (st0007, messae)
         discot = 1
         debug = (.false. .or. memi(wp+6) .gt. 0)
150      if (.not.(wpread (messae, len) .ne. -2).and.(.not.xerflg)) goto
     *    151
         if (xerflg) goto 100
            if (.not.(debug)) goto 160
               call eprinf(st0008)
               call pargsr (messae)
               call pargi (len)
160         continue
            if (.not.(len .le. 0)) goto 170
               discot = 0
               goto 151
170         continue
            call sscan (messae)
            call gargwd (cmd, 1023 )
            ncmd = strdic (cmd, cmd, 1023 , st0009)
            sw0001=(ncmd)
            goto 180
190         continue
               discot = 0
               goto 151
200         continue
               call wpcnve (clktie(0), buf, 12 )
               call sprinf (messae, 1023 , st0010)
               call pargsr (buf)
               call wpinie (wp)
            goto 181
210         continue
               call gargwd (ref, 255 )
               call gargi (objid)
               call gargi (regid)
               if (.not.(debug)) goto 220
                  call xprinf(st0011)
                  call pargi(objid)
                  call pargi(regid)
                  call pargsr(ref)
220            continue
               call wpcnve (clktie(0), buf, 12 )
               call sprinf (messae, 1023 , st0012)
               call pargsr (buf)
               call pargi (objid)
               call pargsr (ref)
               call ximmee (st0013, messae)
               call wpcace (wp, objid, regid, ref)
            goto 181
230         continue
               call gargi (objid)
               if (.not.(debug)) goto 240
                  call xprinf(st0014)
                  call pargi(objid) 
240            continue
               call wpcnve (clktie(0), buf, 12 )
               call sprinf (messae, 1023 , st0015)
               call pargsr (buf)
               call pargi (objid)
               call ximmee (st0016, messae)
               call wpunce (wp, objid)
            goto 181
250         continue
               call gargi (objid)
               call gargr (x) 
               call gargr (y)
               if (.not.(debug)) goto 260
                  call xprinf(st0017)
                  call pargi(objid)
                  call pargr (x)
                  call pargr (y)
260            continue
               call wpwcsn (wp, objid, x, y)
            goto 181
270         continue
               call gargi (objid)
               if (.not.(debug)) goto 280
                  call xprinf(st0018)
                  call pargi(objid)
280            continue
               call wpwcst (wp, objid)
            goto 181
290         continue
               call gargi (objid)
               call gargwd (temple, 255 )
               if (.not.(debug)) goto 300
                  call xprinf(st0019)
                  call pargi(objid)
                  call pargsr (temple)
300            continue
               call wpobjo (wp, objid, temple)
            goto 181
310         continue
               call gargwd (param, 255 )
               call wpsetr (wp, param)
            goto 181
320         continue
            goto 181
330         continue
               debug = .not.(debug)
            goto 181
340         continue
               if (.not.(debug)) goto 350
                  call eprinf (st0020)
                  call pargi(len)
                  call pargsr(messae)
350            continue
               goto 181
180         continue
               if (sw0001.lt.1.or.sw0001.gt.10) goto 340
               goto (310,320,190,200,210,230,250,270,290,330),sw0001
181         continue
            call aclrc (messae, 1023 )
            goto 150
151      continue
         call ximdit (discot)
         call wpshun (wp)
100      return
      end
      subroutine wpinie (wp)
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
      integer wp
      integer cp
      integer wpid2j
      integer i
      save
         i=0
110      if (.not.(i .lt. 256 )) goto 112
            cp = wpid2j (wp, i)
            if (.not.(cp .ne. 0 .and. memi(cp) .ne. 0)) goto 120
               call wpunce (wp, memi(cp) )
120         continue
111         i=i+1
            goto 110
112      continue
100      return
      end
      subroutine wpcace (wp, objid, regid, ref)
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
      integer wp
      integer objid
      integer regid
      integer*2 ref(*)
      integer cp
      integer i
      integer class
      integer*2 alert(255 +1)
      integer wpclas
      integer clncls
      integer cltabe(6 ,16 )
      integer*2 clnams(32 +1,16 )
      common /classm/ clncls, cltabe, clnams
      integer*2 st0001(29)
      integer*2 st0002(1)
      integer*2 st0003(1)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /119,112, 95, 99, 97, 99,104,101/
      data (st0001(iyy),iyy= 9,16) / 58, 32, 85,110, 97, 98,108,101/
      data (st0001(iyy),iyy=17,24) / 32,116,111, 32, 99, 97, 99,104/
      data (st0001(iyy),iyy=25,29) /101, 10, 37,115, 0/
      data st0002 / 0/
      data st0003 / 0/
         i=0
110      if (.not.(i .lt. 256 )) goto 112
            cp = memi(memi(wp ) +i)
            if (.not.(memi(cp+4) .eq. 0)) goto 120
               goto 112
120         continue
111         i=i+1
            goto 110
112      continue
         class = wpclas (ref)
         if (.not.(class .eq. -1)) goto 130
            call sprinf (alert, 255 , st0001)
            call pargsr (ref)
            call ximalt (alert, st0002, st0003)
            goto 100
130      continue
         memi(cp+2) = class
         if (.not.(class .ne. 0 .and. cltabe(1,class) .ne. 0)) goto 140
            call zcall2 (cltabe(1,class) , cp, wp)
140      continue
         if (.not.(class .ne. 0 .and. cltabe(2,class) .ne. 0)) goto 150
            call zcall4 (cltabe(2,class) , cp, objid, regid, ref)
150      continue
100      return
      end
      subroutine wpunce (wp, id)
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
      integer wp
      integer id
      integer cp
      integer wpid2j
      integer class
      integer clncls
      integer cltabe(6 ,16 )
      integer*2 clnams(32 +1,16 )
      common /classm/ clncls, cltabe, clnams
      save
         cp = wpid2j (wp, id)
         if (.not.(cp .eq. 0)) goto 110
            goto 100
110      continue
         class = memi(cp+2)
         if (.not.(class .ne. 0 .and. cltabe(3,class) .ne. 0)) goto 120
            call zcall2 (cltabe(3,class) , cp, id)
120      continue
         memi(cp+4) = 0
100      return
      end
      subroutine wpwcsn (wp, id, x, y)
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
      integer wp
      integer id
      real x
      real y
      integer cp
      integer wpid2j
      integer class
      integer clncls
      integer cltabe(6 ,16 )
      integer*2 clnams(32 +1,16 )
      common /classm/ clncls, cltabe, clnams
      save
         cp = wpid2j (wp, id)
         if (.not.(cp .eq. 0)) goto 110
            goto 100
110      continue
         class = memi(cp+2)
         if (.not.(class .ne. 0 .and. cltabe(4,class) .ne. 0)) goto 120
            call zcall4 (cltabe(4,class) , cp, id, x, y)
120      continue
100      return
      end
      subroutine wpwcst (wp, id)
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
      integer wp
      integer id
      integer cp
      integer wpid2j
      integer class
      integer clncls
      integer cltabe(6 ,16 )
      integer*2 clnams(32 +1,16 )
      common /classm/ clncls, cltabe, clnams
      save
         cp = wpid2j (wp, id)
         if (.not.(cp .eq. 0)) goto 110
            goto 100
110      continue
         class = memi(cp+2)
         if (.not.(class .ne. 0 .and. cltabe(5,class) .ne. 0)) goto 120
            call zcall2 (cltabe(5,class) , cp, id)
120      continue
100      return
      end
      subroutine wpobjo (wp, id, temple)
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
      integer wp
      integer id
      integer*2 temple(*)
      integer cp
      integer wpid2j
      integer class
      integer clncls
      integer cltabe(6 ,16 )
      integer*2 clnams(32 +1,16 )
      common /classm/ clncls, cltabe, clnams
      save
         cp = wpid2j (wp, id)
         if (.not.(cp .eq. 0)) goto 110
            goto 100
110      continue
         class = memi(cp+2)
         if (.not.(class .ne. 0 .and. cltabe(6,class) .ne. 0)) goto 120
            call zcall3 (cltabe(6,class) , cp, id, temple)
120      continue
100      return
      end
      subroutine wpsetr (wp, param)
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
      integer wp
      integer*2 param(255 +1)
      integer*2 arg(32 +1)
      integer*2 buf(32 +1)
      integer*2 msg(32 +1)
      integer line
      integer strdic
      integer clncls
      integer cltabe(6 ,16 )
      integer*2 clnams(32 +1,16 )
      integer sw0001,sw0002,sw0003
      common /classm/ clncls, cltabe, clnams
      integer*2 st0001(11)
      integer*2 st0002(23)
      integer*2 st0003(4)
      integer*2 st0004(4)
      integer*2 st0005(66)
      integer*2 st0006(12)
      integer*2 st0007(14)
      integer*2 st0008(30)
      integer*2 st0009(12)
      integer*2 st0010(13)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /115,101,116, 58, 32, 37,115, 32/
      data (st0001(iyy),iyy= 9,11) / 61, 32, 0/
      data (st0002(iyy),iyy= 1, 8) /124,112,115,105,122,101,124, 98/
      data (st0002(iyy),iyy= 9,16) /112,109,124,119, 99,115,124,102/
      data (st0002(iyy),iyy=17,23) /111,114,109, 97,116,124, 0/
      data st0003 / 37,100, 10, 0/
      data st0004 / 37,100, 10, 0/
      data (st0005(iyy),iyy= 1, 8) /124,110,111,110,101,124,108,111/
      data (st0005(iyy),iyy= 9,16) /103,105, 99, 97,108,124,112,104/
      data (st0005(iyy),iyy=17,24) /121,115,105, 99, 97,108,124,119/
      data (st0005(iyy),iyy=25,32) /111,114,108,100,124,115,107,121/
      data (st0005(iyy),iyy=33,40) / 9, 9, 9,124, 97,109,112,108/
      data (st0005(iyy),iyy=41,48) /105,102,105,101,114,124, 99, 99/
      data (st0005(iyy),iyy=49,56) /100,124,100,101,116,101, 99,116/
      data (st0005(iyy),iyy=57,64) /111,114,124,111,116,104,101,114/
      data (st0005(iyy),iyy=65,66) /124, 0/
      data (st0006(iyy),iyy= 1, 8) / 37,115, 32,108,105,110,101, 61/
      data (st0006(iyy),iyy= 9,12) / 37,100, 10, 0/
      data (st0007(iyy),iyy= 1, 8) /119, 99,115,116,121,112,101, 32/
      data (st0007(iyy),iyy= 9,14) / 37,115, 32, 37,100, 0/
      data (st0008(iyy),iyy= 1, 8) /124,100,101,102, 97,117,108,116/
      data (st0008(iyy),iyy= 9,16) /124,104,109,115,124,100,101,103/
      data (st0008(iyy),iyy=17,24) /114,101,101,115,124,114, 97,100/
      data (st0008(iyy),iyy=25,30) /105, 97,110,115,124, 0/
      data (st0009(iyy),iyy= 1, 8) / 37,115, 32,108,105,110,101, 61/
      data (st0009(iyy),iyy= 9,12) / 37,100, 10, 0/
      data (st0010(iyy),iyy= 1, 8) /119, 99,115,102,109,116, 32, 37/
      data (st0010(iyy),iyy= 9,13) /115, 32, 37,100, 0/
         if (.not.(.false.)) goto 110
            call xprinf(st0001)
            call pargsr(param) 
110      continue
         sw0001=(strdic (param, param, 32 , st0002))
         goto 120
130      continue
            call gargi (memi(wp+1) )
            if (.not.(.false.)) goto 140
               call xprinf(st0003)
               call pargi(memi(wp+1) ) 
140         continue
         goto 121
150      continue
            call gargi (memi(wp+2) )
            if (.not.(.false.)) goto 160
               call xprinf(st0004)
               call pargi(memi(wp+2) ) 
160         continue
         goto 121
170      continue
            call gargwd (buf, 255 )
            call gargi (line)
            call xstrcy(buf, arg, 32 )
            call strlwr (buf)
            sw0002=(strdic (buf, buf, 255 , st0005))
            goto 180
190         continue
               memi(memi(wp+3) +line-1) = 2
            goto 181
200         continue
               memi(memi(wp+3) +line-1) = 3
            goto 181
210         continue
               memi(memi(wp+3) +line-1) = 4
            goto 181
220         continue
               memi(memi(wp+3) +line-1) = 1
            goto 181
230         continue
               memi(memi(wp+3) +line-1) = 6
            goto 181
240         continue
               memi(memi(wp+3) +line-1) = 3
            goto 181
250         continue
               memi(memi(wp+3) +line-1) = 8
            goto 181
260         continue
               memi(memi(wp+3) +line-1) = 5
               goto 181
180         continue
               if (sw0002.lt.1.or.sw0002.gt.8) goto 260
               goto (220,190,200,210,260,230,240,250),sw0002
181         continue
            call xstrcy(buf, memc(memi(wp+4) +(32 *(line-1))), 32 )
            if (.not.(.false.)) goto 270
               call xprinf(st0006)
               call pargsr(buf)
               call pargi(line) 
270         continue
            call sprinf (msg, 255 , st0007)
            call pargsr (arg)
            call pargi (line)
            call wcspie (msg)
         goto 121
280      continue
            call gargwd (buf, 255 )
            call gargi (line)
            call xstrcy(buf, arg, 32 )
            call strlwr (buf)
            sw0003=(strdic (buf, buf, 255 , st0008))
            goto 290
300         continue
               memi(memi(wp+5) +line-1) = 1
            goto 291
310         continue
               memi(memi(wp+5) +line-1) = 2
            goto 291
320         continue
               memi(memi(wp+5) +line-1) = 3
            goto 291
330         continue
               memi(memi(wp+5) +line-1) = 4
            goto 291
340         continue
               memi(memi(wp+5) +line-1) = 1
               goto 291
290         continue
               if (sw0003.lt.1.or.sw0003.gt.4) goto 340
               goto (300,310,320,330),sw0003
291         continue
            if (.not.(.false.)) goto 350
               call xprinf(st0009)
               call pargsr(buf)
               call pargi(line) 
350         continue
            call sprinf (msg, 255 , st0010)
            call pargsr (arg)
            call pargi (line)
            call wcspie (msg)
            goto 121
120      continue
            if (sw0001.lt.1.or.sw0001.gt.4) goto 121
            goto (130,150,170,280),sw0001
121      continue
100      return
      end
      subroutine wpgetr (wp, param)
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
      integer wp
      integer*2 param(255 +1)
      integer strdic
      integer sw0001
      integer*2 st0001(11)
      integer*2 st0002(23)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /115,101,116, 58, 32, 37,115, 32/
      data (st0001(iyy),iyy= 9,11) / 61, 32, 0/
      data (st0002(iyy),iyy= 1, 8) /124,112,115,105,122,101,124, 98/
      data (st0002(iyy),iyy= 9,16) /112,109,124,119, 99,115,124,102/
      data (st0002(iyy),iyy=17,23) /111,114,109, 97,116,124, 0/
         if (.not.(.false.)) goto 110
            call xprinf(st0001)
            call pargsr(param) 
110      continue
         sw0001=(strdic (param, param, 32 , st0002))
         goto 120
130      continue
         goto 121
140      continue
         goto 121
150      continue
         goto 121
160      continue
            goto 121
120      continue
            if (sw0001.lt.1.or.sw0001.gt.4) goto 121
            goto (130,140,150,160),sw0001
121      continue
100      return
      end
      integer function wpinit ()
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
      integer wp
      integer i
      logical xerpop
      logical xerflg
      common /xercom/ xerflg
      integer*2 st0001(37)
      integer*2 st0002(5)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) / 69,114,114,111,114, 32,111,112/
      data (st0001(iyy),iyy= 9,16) /101,110,105,110,103, 32, 87, 67/
      data (st0001(iyy),iyy=17,24) / 83, 80, 73, 88, 32,116, 97,115/
      data (st0001(iyy),iyy=25,32) /107, 32,115,116,114,117, 99,116/
      data (st0001(iyy),iyy=33,37) /117,114,101, 46, 0/
      data st0002 /110,111,110,101, 0/
         call xerpsh
         call xcallc(wp, 7, 10 )
         if (.not.xerpop()) goto 110
            call xerror(0, st0001)
            if (xerflg) goto 100
110      continue
         call xcallc(memi(wp+3) , 4 , 4)
         call xcallc(memi(wp+5) , 4 , 4)
         call xcallc(memi(wp+4) , (32 *4 ), 2)
         i=1
120      if (.not.(i .le. 4 )) goto 122
            memi(memi(wp+5) +i-1) = 1
            memi(memi(wp+3) +i-1) = 2
            call xstrcy(st0002, memc(memi(wp+4) +(32 *(i-1))), 32 )
121         i=i+1
            goto 120
122      continue
         call xcallc(memi(wp ) , 256 , 10 )
         i=0
130      if (.not.(i .lt. 256 )) goto 132
            call xcallc(memi(memi(wp ) +i) , 135 , 10 )
131         i=i+1
            goto 130
132      continue
         memi(wp+1) = 0
         memi(wp+2) = 1
         call wpclat()
         wpinit = (wp)
         goto 100
100      return
      end
      integer function wpread (messae, len)
      integer len
      integer*2 messae(*)
      integer nread
      integer ximred
      logical xerflg
      common /xercom/ xerflg
      save
         nread = ximred (messae, len)
         if (xerflg) goto 100
         wpread = (nread)
         goto 100
100      return
      end
      subroutine wpshun (wp)
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
      integer wp
      integer i
      save
         call xmfree(memi(wp+4) , 2)
         call xmfree(memi(wp+5) , 4)
         call xmfree(memi(wp+3) , 4)
         i=0
110      if (.not.(i .lt. 256 )) goto 112
            call xmfree(memi(memi(wp ) +i) , 10 )
111         i=i+1
            goto 110
112      continue
         call xmfree(memi(wp ) , 10 )
         call xmfree(wp, 10 )
100      return
      end
      integer function wpclas (object)
      integer*2 object(*)
      integer n
      integer class
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
      integer*2 ch
      integer*2 buf(255 +1)
      integer xstrln
      integer stridx
      logical streq
      integer immap
      logical xerpop
      logical xerflg
      common /xercom/ xerflg
      integer*2 st0001(9)
      integer*2 st0002(8)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) / 47,100,101,118, 47,112,105,120/
      data (st0001(iyy),iyy= 9, 9) / 0/
      data st0002 /100,101,118, 36,112,105,120, 0/
         call imgime (object, buf, 255 )
         n = xstrln(buf) - 7
         if (.not.(streq (buf(n), st0001))) goto 110
            call xstrcy(st0002, buf, 255 )
            ch = 91
            n = stridx (ch, object)
            if (.not.(n .gt. 0)) goto 120
               call xstrct(object(n), buf, 255 )
120         continue
            call xstrcy(buf, object, 255 )
110      continue
         class = 1
         call xerpsh
         im = immap (object, 1 , 0)
         if (xerpop()) goto 130
            class = 2
            call imunmp (im)
130      continue
         wpclas = (class)
         goto 100
100      return
      end
      integer function wpid2j (wp, id)
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
      integer wp
      integer id
      integer i
      integer cp
      save
         i=0
110      if (.not.(i .lt. 256 )) goto 112
            cp = memi(memi(wp ) +i)
            if (.not.(memi(cp) .eq. id)) goto 120
               wpid2j = (cp)
               goto 100
120         continue
111         i=i+1
            goto 110
112      continue
         wpid2j = (0)
         goto 100
100      return
      end
      subroutine wpclat ()
      external imgint
      external imgcae
      external imgune
      external imgwcn
      external imgwct
      external imgobo
      external mefint
      external mefcae
      external mefune
      external mefwcn
      external mefwct
      external mefobo
      external mspint
      external mspcae
      external mspune
      external mspwcn
      external mspwct
      external mspobo
      external unkint
      external unkcae
      external unkune
      external unkwcn
      external unkwct
      external unkobo
      integer clncls
      integer cltabe(6 ,16 )
      integer*2 clnams(32 +1,16 )
      integer locpr
      common /classm/ clncls, cltabe, clnams
      integer*2 st0001(8)
      integer*2 st0002(6)
      integer*2 st0003(4)
      integer*2 st0004(10)
      save
      integer iyy
      data st0001 /117,110,107,110,111,119,110, 0/
      data st0002 /105,109, 97,103,101, 0/
      data st0003 /109,101,102, 0/
      data (st0004(iyy),iyy= 1, 8) /109,117,108,116,105,115,112,101/
      data (st0004(iyy),iyy= 9,10) / 99, 0/
         clncls = 0
         call wploas (st0001, locpr(unkint), locpr(unkcae), locpr(unkune
     *   ), locpr(unkwcn), locpr(unkwct), locpr(unkobo))
         call wploas (st0002, locpr(imgint), locpr(imgcae), locpr(imgune
     *   ), locpr(imgwcn), locpr(imgwct), locpr(imgobo))
         call wploas (st0003, locpr(mefint), locpr(mefcae), locpr(mefune
     *   ), locpr(mefwcn), locpr(mefwct), locpr(mefobo))
         call wploas (st0004, locpr(mspint), locpr(mspcae), locpr(mspune
     *   ), locpr(mspwcn), locpr(mspwct), locpr(mspobo))
100      return
      end
      subroutine wploas (name, init, cache, uncace, tran, list, info)
      integer init
      integer cache
      integer uncace
      integer tran
      integer list
      integer info
      integer*2 name(*)
      integer clncls
      integer cltabe(6 ,16 )
      integer*2 clnams(32 +1,16 )
      logical xerflg
      common /xercom/ xerflg
      common /classm/ clncls, cltabe, clnams
      save
         if (.not.(clncls + 1 .gt. 16 )) goto 110
            goto 100
110      continue
         clncls = clncls + 1
         cltabe(1,clncls) = init
         cltabe(2,clncls) = cache
         cltabe(3,clncls) = uncace
         cltabe(4,clncls) = tran
         cltabe(5,clncls) = list
         cltabe(6,clncls) = info
         call xstrcy(name, clnams(1,clncls) , 255 )
100      return
      end
      subroutine wcspie (messae)
      integer*2 messae(*)
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
      integer msgbuf
      integer msglen
      integer mlen
      integer ip
      integer xstrln
      integer*2 st0001(18)
      integer*2 st0002(4)
      integer*2 st0003(8)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) /100,101,108,105,118,101,114, 32/
      data (st0001(iyy),iyy= 9,16) /119, 99,115,112,105,120, 32,123/
      data (st0001(iyy),iyy=17,18) / 32, 0/
      data st0002 / 32,125, 0, 0/
      data st0003 /105,115,109, 95,109,115,103, 0/
         mlen = xstrln(messae)
         msglen = mlen + 64
         call smark (sp)
         call salloc (msgbuf, msglen, 2)
         call aclrc (memc(msgbuf), msglen)
         ip = 0
         call amovc (st0001, memc(msgbuf), 17) 
         ip = ip + 17
         call amovc (messae, memc(msgbuf+ip), mlen) 
         ip = ip + mlen
         call amovc (st0002, memc(msgbuf+ip), 2) 
         ip = ip + 2
         call ximmee (st0003, memc(msgbuf))
         call sfree (sp)
100      return
      end
      subroutine wpcnve (ltime, outstr, maxch)
      integer*4 ltime
      integer maxch
      integer*2 outstr(*)
      integer tm(8 )
      integer*2 st0001(14)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) / 37, 50,100, 58, 37, 48, 50,100/
      data (st0001(iyy),iyy= 9,14) / 58, 37, 48, 50,100, 0/
         call brktie (ltime, tm)
         call sprinf (outstr, maxch, st0001)
         call pargi (tm(3) )
         call pargi (tm(2) )
         call pargi (tm(1) )
100      return
      end
      subroutine dbgpre (wp, buf)
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
      integer wp
      integer*2 buf(*)
      integer cp
      integer wpid2j
      integer i
      integer*2 st0001(4)
      integer*2 st0002(23)
      save
      integer iyy
      data st0001 / 37,115, 10, 0/
      data (st0002(iyy),iyy= 1, 8) / 37, 51,100, 58, 32, 32,105,100/
      data (st0002(iyy),iyy= 9,16) / 61, 37,100, 32, 32,114,101,102/
      data (st0002(iyy),iyy=17,23) / 61, 39, 37,115, 39, 10, 0/
         call xprinf(st0001) 
         call pargsr (buf)
         i=0
110      if (.not.(i .lt. 256 )) goto 112
            cp = wpid2j (wp, i)
            if (.not.(memi(cp+3) .ne. 0)) goto 120
               call xprinf(st0002)
               call pargi(i)
               call pargi(memi(cp) )
               call pargsr(memc((((cp+6)-1)*2+1)) )
120         continue
111         i=i+1
            goto 110
112      continue
100      return
      end
c     temple  template
c     sprinf  sprintf
c     wpclas  wp_class
c     clncls  cl_nclass
c     wcspie  wcspix_message
c     classm  class_com
c     unkwct  unk_wcslist
c     mefcae  mef_cache
c     mspwct  msp_wcslist
c     cltabe  cl_table
c     unkint  unk_init
c     wpread  wp_read
c     mspint  msp_init
c     ximmee  xim_message
c     wpcace  wp_cache
c     imgcae  img_cache
c     messae  message
c     unkobo  unk_objinfo
c     mspobo  msp_objinfo
c     clktie  clktime
c     ximcot  xim_connect
c     wpshun  wp_shutdown
c     wpclat  wp_class_init
c     imgime  imgimage
c     mefune  mef_uncache
c     mefwcn  mef_wcstran
c     ximinr  xim_intrhandler
c     clnams  cl_names
c     gargwd  gargwrd
c     ximalt  xim_alert
c     brktie  brktime
c     twcspx  t_wcspix
c     wpunce  wp_uncache
c     wpwcsn  wp_wcstran
c     imgune  img_uncache
c     imgwcn  img_wcstran
c     envgei  envgeti
c     wpgetr  wp_getpar
c     mefwct  mef_wcslist
c     wpinie  wp_initialize
c     ximred  xim_read
c     mefint  mef_init
c     unkcae  unk_cache
c     wpwcst  wp_wcslist
c     imunmp  imunmap
c     imgwct  img_wcslist
c     mspcae  msp_cache
c     eprinf  eprintf
c     wpinit  wp_init
c     imgint  img_init
c     mefobo  mef_objinfo
c     envges  envgets
c     ximdit  xim_disconnect
c     discot  disconnect
c     dbgpre  dbg_printcache
c     wpcnve  wp_cnvdate
c     wpsetr  wp_setpar
c     wpid2j  wp_id2obj
c     wpobjo  wp_objinfo
c     imgobo  img_objinfo
c     unkune  unk_uncache
c     unkwcn  unk_wcstran
c     wploas  wp_load_class
c     uncace  uncache
c     pargsr  pargstr
c     mspune  msp_uncache
c     mspwcn  msp_wcstran
