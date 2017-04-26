      subroutine frame
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
      if (modef .eq. 1) go to 101
      mbpair = ior(ishift(226,8),0)
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      if ((mipair+5) .gt. 16) call flushb
      mbpair = ior(ishift(231,8),8)
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      mbpair = mjxmin
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      mbpair = mjymin
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      mbpair = mjxmax
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      mbpair = mjymax
      mfrend = 1
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      call flushb
      if (mbuflu .gt. 0) call preout
      mjxmin = 32767
      mjymin = 32767
      mjxmax = 0
      mjymax = 0
      mxold = -9999
      myold = -9999
      mop(1) = 0
      mop(2) = 204
      mop(5) = 0
      mop(3) = 0
      mop(4) = 128
      mop(7) = 8
      mop(6) = ior(1,ishift(32767,1))
      mop(8) = 0
      mop(9) = 0
      mop(10) = 0
      mfrend = 0
      return
c
  101 call uliber (0,45h0frame call illegal between flash1 and flash2,
     1             45)
      call perror
      return
      end
