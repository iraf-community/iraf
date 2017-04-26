      subroutine sksetd (coo, param, value)
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
      integer param
      double precision value
      logical xerflg
      common /xercom/ xerflg
      integer sw0001
      integer*2 st0001(46)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) / 83, 75, 89, 95, 83, 69, 84, 68/
      data (st0001(iyy),iyy= 9,16) / 58, 32, 85,110,107,110,111,119/
      data (st0001(iyy),iyy=17,24) /110, 32, 99,111,111,114,100,105/
      data (st0001(iyy),iyy=25,32) /110, 97,116,101, 32,115,121,115/
      data (st0001(iyy),iyy=33,40) /116,101,109, 32,112, 97,114, 97/
      data (st0001(iyy),iyy=41,46) /109,101,116,101,114, 0/
         sw0001=(param)
         goto 110
120      continue
            memd((((coo)-1)/2+1)) = value
         goto 111
130      continue
            memd((((coo+2)-1)/2+1)) = value
         goto 111
140      continue
            memd((((coo+4)-1)/2+1)) = value
         goto 111
150      continue
            memd((((coo+6)-1)/2+1)) = value
         goto 111
160      continue
            memd((((coo+8)-1)/2+1)) = value
         goto 111
170      continue
            memd((((coo+10)-1)/2+1)) = value
         goto 111
180      continue
            call xerror(0, st0001)
            if (xerflg) goto 100
            goto 111
110      continue
            if (sw0001.lt.1.or.sw0001.gt.6) goto 180
            goto (120,130,140,150,160,170),sw0001
111      continue
100      return
      end
      subroutine skseti (coo, param, value)
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
      integer param
      integer value
      logical xerflg
      common /xercom/ xerflg
      integer sw0001
      integer*2 st0001(46)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) / 83, 75, 89, 95, 83, 69, 84, 73/
      data (st0001(iyy),iyy= 9,16) / 58, 32, 85,110,107,110,111,119/
      data (st0001(iyy),iyy=17,24) /110, 32, 99,111,111,114,100,105/
      data (st0001(iyy),iyy=25,32) /110, 97,116,101, 32,115,121,115/
      data (st0001(iyy),iyy=33,40) /116,101,109, 32,112, 97,114, 97/
      data (st0001(iyy),iyy=41,46) /109,101,116,101,114, 0/
         sw0001=(param)
         goto 110
120      continue
            memi(coo+12) = value
         goto 111
130      continue
            memi(coo+13) = value
         goto 111
140      continue
            memi(coo+14) = value
         goto 111
150      continue
            memi(coo+15) = value
         goto 111
160      continue
            memi(coo+16) = value
         goto 111
170      continue
            memi(coo+17) = value
         goto 111
180      continue
            memi(coo+18) = value
         goto 111
190      continue
            memi(coo+19) = value
         goto 111
200      continue
            memi(coo+20) = value
         goto 111
210      continue
            memi(coo+21) = value
         goto 111
220      continue
            memi(coo+22) = value
         goto 111
230      continue
            memi(coo+23) = value
         goto 111
240      continue
            memi(coo+24) = value
         goto 111
250      continue
            call xerror(0, st0001)
            if (xerflg) goto 100
            goto 111
110      continue
            sw0001=sw0001-6
            if (sw0001.lt.1.or.sw0001.gt.14) goto 250
            goto (120,130,140,150,160,170,180,190,200,210,220,230,250,
     *      240),sw0001
111      continue
100      return
      end
      subroutine sksets (coo, param, value)
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
      integer param
      integer*2 value(*)
      logical xerflg
      common /xercom/ xerflg
      integer sw0001
      integer*2 st0001(48)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) / 83, 75, 89, 95, 83, 69, 84, 83/
      data (st0001(iyy),iyy= 9,16) / 84, 82, 58, 32, 85,110,107,110/
      data (st0001(iyy),iyy=17,24) /111,119,110, 32, 99,111,111,114/
      data (st0001(iyy),iyy=25,32) /100,105,110, 97,116,101, 32,115/
      data (st0001(iyy),iyy=33,40) /121,115,116,101,109, 32,112, 97/
      data (st0001(iyy),iyy=41,48) /114, 97,109,101,116,101,114, 0/
         sw0001=(param)
         goto 110
120      continue
            call xstrcy(value, memc((((coo+25)-1)*2+1)) , 255 )
         goto 111
130      continue
            call xerror(0, st0001)
            if (xerflg) goto 100
            goto 111
110      continue
            if (sw0001.eq.19) goto 120
            goto 130
111      continue
100      return
      end
c     sksetd  sk_setd
c     skseti  sk_seti
c     sksets  sk_sets
