      subroutine z8zpbd
c
c kpno: only obvious constants are initialized in this block data.
c all other initialization occurs in z8zpii.
c
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
      data mfmtx(1)/4h(e10/
      data mfmty(1)/4h(e10/
      data mfmty(2)/4h.3) /
      data mfmtx(2)/4h.3) /
c
      data mname(1)/4hcase/
      data mname(2)/4hintn/
      data mname(5)/4hfont/
      data mname(3)/4horen/
      data mname(4)/4hcsiz/
      data mname(7)/4hssiz/
      data mname(6)/4hdpat/
      data mname(8)/4hcent/
      data mname(9)/4hcolr/
c
c ********************** note to implementors **************************
c
c set to unit number for printer
c
      data mprint/6/
c
c ********************** note to implementors **************************
c
c set to unit number for plotter
c
      data munit/8/
c
c ********************** note to implementors **************************
c
c set to smallest positive number on the computer
c
      data small/1.e-25/
c
      end
