      subroutine flushb
c     dimension       idummy(1)
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
      external        z8zpbd
      if (mipair .eq. 16) go to 102
      if (mipair .eq. 0) return
      if (mipair.eq.2 .and. mflcnt.eq.0) return
      mipp1 = mipair+1
      do 101 i=mipp1,16
         mbprs(i) = 40992
  101 continue
  102 if (mbufa .eq. -9999) mbufa = loci(msybuf)
      mflcnt = mflcnt+1
      call packum (mbprs,16,mbufa+mbuflu)
      mbuflu = mbuflu+8
      mipair = 0
      if (modef .eq. 1) go to 103
      if (mbuflu+8 .le. mbufl) return
      if (mbuflu .gt. 0) call preout
      return
  103 if (mod(mbuflu,msblen) .eq. 0) go to 104
      if (mbuflu+8 .le. mbufl) return
  104 continue
      if (mbuflu .gt. 0) call preout
      return
      end
