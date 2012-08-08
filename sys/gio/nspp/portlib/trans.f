      subroutine trans
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
      integer xx,yy
c
      logical         intt
      equivalence     (zz,mz),(temp,itemp)
c     ray bovet patch to avoid small integers being set to 0
c     zz = xx
      mz = xx
      if (intt(zz)) go to 102
      if (mtypex .eq. 0) go to 101
      if (zz .le. 0.0)
     1    call uliber (0,35h0negative argument with log scaling,35)
      zz = amax1(zz,small)
      zz = xfactr*alog10(zz)+xadd
      go to 103
  101 zz = xfactr*zz+xadd
      go to 103
  102 zz = float(ishift(mz-1,mshftx))
  103 mx = max1(0.,amin1(32767.,zz))
c     ray bovet patch to avoid small integers being set to 0
c     zz = yy
      mz = yy
      if (intt(zz)) go to 105
      if (mtypey .eq. 0) go to 104
      if (zz .le. 0.0)
     1    call uliber (0,35h0negative argument with log scaling,35)
      zz = amax1(zz,small)
      zz = yfactr*alog10(zz)+yadd
      go to 106
  104 zz = yfactr*zz+yadd
      go to 106
  105 zz = float(ishift(mz-1,mshfty))
  106 my = max1(0.,amin1(32767.,zz))
      return
      end
