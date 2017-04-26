      double precision function skstad (coo, param)
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
      logical xerflg
      common /xercom/ xerflg
      integer sw0001
      integer*2 st0001(47)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) / 83, 75, 89, 95, 83, 84, 65, 84/
      data (st0001(iyy),iyy= 9,16) / 68, 58, 32, 85,110,107,110,111/
      data (st0001(iyy),iyy=17,24) /119,110, 32, 99,111,111,114,100/
      data (st0001(iyy),iyy=25,32) /105,110, 97,116,101, 32,115,121/
      data (st0001(iyy),iyy=33,40) /115,116,101,109, 32,112, 97,114/
      data (st0001(iyy),iyy=41,47) / 97,109,101,116,101,114, 0/
      skstad = 0
         sw0001=(param)
         goto 110
120      continue
            skstad = (memd((((coo)-1)/2+1)) )
            goto 100
130      continue
            skstad = (memd((((coo+2)-1)/2+1)) )
            goto 100
140      continue
            skstad = (memd((((coo+4)-1)/2+1)) )
            goto 100
150      continue
            skstad = (memd((((coo+6)-1)/2+1)) )
            goto 100
160      continue
            skstad = (memd((((coo+8)-1)/2+1)) )
            goto 100
170      continue
            skstad = (memd((((coo+10)-1)/2+1)) )
            goto 100
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
      integer function skstai (coo, param)
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
      logical xerflg
      common /xercom/ xerflg
      integer sw0001
      integer*2 st0001(47)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) / 83, 75, 89, 95, 83, 84, 65, 84/
      data (st0001(iyy),iyy= 9,16) / 73, 58, 32, 85,110,107,110,111/
      data (st0001(iyy),iyy=17,24) /119,110, 32, 99,111,111,114,100/
      data (st0001(iyy),iyy=25,32) /105,110, 97,116,101, 32,115,121/
      data (st0001(iyy),iyy=33,40) /115,116,101,109, 32,112, 97,114/
      data (st0001(iyy),iyy=41,47) / 97,109,101,116,101,114, 0/
         sw0001=(param)
         goto 110
120      continue
            skstai = (memi(coo+12) )
            goto 100
130      continue
            skstai = (memi(coo+13) )
            goto 100
140      continue
            skstai = (memi(coo+14) )
            goto 100
150      continue
            skstai = (memi(coo+15) )
            goto 100
160      continue
            skstai = (memi(coo+16) )
            goto 100
170      continue
            skstai = (memi(coo+17) )
            goto 100
180      continue
            skstai = (memi(coo+18) )
            goto 100
190      continue
            skstai = (memi(coo+19) )
            goto 100
200      continue
            skstai = (memi(coo+20) )
            goto 100
210      continue
            skstai = (memi(coo+21) )
            goto 100
220      continue
            skstai = (memi(coo+22) )
            goto 100
230      continue
            skstai = (memi(coo+23) )
            goto 100
240      continue
            skstai = (memi(coo+24) )
            goto 100
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
      subroutine skstas (coo, param, value, maxch)
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
      integer*2 value
      integer maxch
      logical xerflg
      common /xercom/ xerflg
      integer sw0001
      integer*2 st0001(48)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) / 83, 75, 89, 95, 71, 69, 84, 83/
      data (st0001(iyy),iyy= 9,16) / 84, 82, 58, 32, 85,110,107,110/
      data (st0001(iyy),iyy=17,24) /111,119,110, 32, 99,111,111,114/
      data (st0001(iyy),iyy=25,32) /100,105,110, 97,116,101, 32,115/
      data (st0001(iyy),iyy=33,40) /121,115,116,101,109, 32,112, 97/
      data (st0001(iyy),iyy=41,48) /114, 97,109,101,116,101,114, 0/
         sw0001=(param)
         goto 110
120      continue
            call xstrcy(memc((((coo+25)-1)*2+1)) , value, maxch)
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
c     skstad  sk_statd
c     skstai  sk_stati
c     skstas  sk_stats
