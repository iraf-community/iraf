      subroutine flash2 (ipoint,ibuflu)
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
      dimension       idummy(1)
      if (modef .ne. 1) go to 101
      kpoint = ipoint
      if (kpoint.lt.0 .or. kpoint.gt.10) go to 102
      call flushb
      nextra = 5
      ibuflu = mbuflu+nextra
      if (mf2er .gt. 0) go to 103
      if (ibuflu .gt. mbufl) go to 103
      mfwa(kpoint+1) = mbufa
      mlwa(kpoint+1) = mbufa+mbuflu-1
      isub = mbufa+mbuflu-loci(idummy)
      idummy(isub+1) = mbuflu
      idummy(isub+2) = mjxmin
      idummy(isub+3) = mjymin
      idummy(isub+4) = mjxmax
      idummy(isub+5) = mjymax
      modef = 2
      mbufa = loci(msybuf)
      mbufl = msblen
      mbuflu = 0
      mbprs(1) = mpair1
      mbprs(2) = mpair2
      mipair = 2
      mflcnt = 0
      mxold = -9999
      myold = -9999
      mjxmin = mnxsto
      mjymin = mnysto
      mjxmax = mxxsto
      mjymax = mxysto
      return
c
  101 call uliber (0,29h0flash2 called without flash1,29)
      call perror
      return
  102 continue
c     write (mprint,1001) kpoint
c
      call uliber (0,38h0first argument to flash2 out of range,38)
      call perror
      return
  103 continue
      nlen = mf2er*mbufl+ibuflu
c     write (mprint,1002) nlen
c
      call uliber (0,23h0flash buffer too short,23)
      call perror
      return
c
c1001 format (27h0flash2 called with ipoint=,i5)
c1002 format (27h0flash buffer must be about,i8,11h words long)
c
      end
