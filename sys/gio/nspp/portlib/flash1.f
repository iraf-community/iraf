      subroutine flash1 (ibuf,ibufl)
      dimension       ibuf(ibufl)
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
      mxold = -9999
      myold = -9999
      call mcflsh
      mbufa = loci(ibuf)
      mbufl = ibufl
      modef = 1
      mnxsto = mjxmin
      mnysto = mjymin
      mxxsto = mjxmax
      mxysto = mjymax
      mjxmin = 32767
      mjymin = 32767
      mjxmax = 0
      mjymax = 0
      mbuflu = 0
      return
c
  101 call uliber (0,
     1             48h0flash1 called consecutively without flash2 call,
     2             48)
      call perror
      return
      end
