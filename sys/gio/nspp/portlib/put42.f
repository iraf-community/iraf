      subroutine put42
      common /sysplt/ mmajx  ,mmajy  ,mminx  ,mminy  ,mxlab  ,mylab  ,
     1                mflg   ,mtype  ,mxa    ,mya    ,mxb    ,myb    ,
     2                mx     ,my     ,mtypex ,mtypey ,xxa    ,yya    ,
     3                xxb    ,yyb    ,xxc    ,yyc    ,xxd    ,yyd    ,
     4                xfactr ,yfactr ,xadd   ,yadd   ,xx     ,yy     ,
     5                mfmtx(3)       ,mfmty(3)       ,mumx   ,mumy   ,
     6                msizx  ,msizy  ,mxdec  ,mydec  ,mxor   ,mop(19),
     7                mname(19)      ,mxold  ,myold  ,mxmax  ,mymax  ,
     8                mxfac  ,myfac  ,modef  ,mf2er  ,mshftx ,mshfty ,
     9                mmgrx  ,mmgry  ,mmnrx  ,mmnry  ,mfrend ,mfrlst ,
     +                mcrout ,mpair1 ,mpair2 ,msblen ,mflcnt ,mjxmin ,
     1                mjymin ,mjxmax ,mjymax ,mnxsto ,mnysto ,mxxsto ,
     2                mxysto ,mprint ,msybuf(360)    ,mncpw  ,minst  ,
     3                mbufa  ,mbuflu ,mfwa(12)       ,mlwa(12)       ,
     4                mipair ,mbprs(16)      ,mbufl  ,munit  ,mbswap ,
     5                small
      mjxmax = max0(mx,mjxmax)
      mjymax = max0(my,mjymax)
      mjxmin = min0(mx,mjxmin)
      mjymin = min0(my,mjymin)
c
c test if increment instruction will work
c
      if (iabs(mx-mxold).gt.mxmax .or. iabs(my-myold).gt.mymax)
     1    go to 101
c
c construct increment instructions
c
      incx = (mx-mxold)/mxfac+160
      incy = (my-myold)/myfac+32+minst*128
c
c put instruction in buffer
c
      mbpair = ior(ishift(incx,8),incy)
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      mxold = mx
      myold = my
      return
  101 continue
c
c mx is first half of the instruction as it stands
c
      mbpair = mx
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
c
c my needs only pen bit
c
      mbpair = my+ishift(minst,15)
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      mxold = mx
      myold = my
      return
      end
