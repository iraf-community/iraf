      subroutine pixel0(dx1,dy1,n1,dx2,dy2,n2)
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
      data ipixop / 10/
      mbpair = ior(ishift(ior(192, ipixop), 8), 12)
      mipair = mipair + 1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      xx = dx1
      yy = dy1
      call dtran16
      mx1 = mx
      mbpair = mx
      mipair = mipair + 1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      my1=my
      mbpair=my
      mipair = mipair + 1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      mbpair=n1
      mipair = mipair + 1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      xx = dx2
      yy = dy2
      call dtran16
      mbpair=mx
      mipair = mipair + 1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      mbpair=my
      mipair = mipair + 1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      mbpair=n2
      mipair = mipair + 1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      if(n1*n2*(mx1*my-mx*my1) .ne. 0) return
      call uliber(0,35h vectors not independent in pixel0.,35)
      call perror
      end
