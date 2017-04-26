      subroutine tick4 (mgrx,mnrx,mgry,mnry)
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
c
c mmgrx(y) is the length in the x(y) direction of major tick marks
c and is therefor used on the y(x) axis (to be consistent with mx(y)dec
c of labmod).
c mgrx(y) is the length of x(y) axis major tick marks.
c similarly for mmnrx(y) and mnrx(y).
c
      mmgrx = isign(ishift(iabs(mgry),mshftx),mgry)
      mmgry = isign(ishift(iabs(mgrx),mshfty),mgrx)
      mmnrx = isign(ishift(iabs(mnry),mshftx),mnry)
      mmnry = isign(ishift(iabs(mnrx),mshfty),mnrx)
      return
      end
