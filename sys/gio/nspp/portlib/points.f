      subroutine points (x,y,n,ichar,ipen)
      dimension       x(n)   ,y(n)
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
c     ray bovet patch to avoid small integers being set to 0
      integer x,y,xx,yy
c
      if (n .le. 0) return
      mbpair = 59394
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      if (ichar) 102,101,102
  101 mbpair = 256
      go to 103
  102 call ncgchr (ichar,1,1,jchar)
      mbpair = 512+jchar
  103 mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      xx = x(1)
      yy = y(1)
      call trans
      minst = 0
      call put42
      if (n .eq. 1) go to 105
      do 104 i=2,n
         xx = x(i)
         yy = y(i)
         call trans
         minst = ipen
         call put42
  104 continue
  105 mbpair = 59394
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      mbpair = 0
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      return
      end
