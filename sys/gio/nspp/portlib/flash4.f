      subroutine flash4 (ifw,lwd,ipoint)
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
      dimension       ifw(1) ,lwd(1)
      jfwa = loci(ifw)
      jlwda = loci(lwd)
      kpoint = ipoint
      if (jfwa .gt. jlwda) go to 101
      if (kpoint.lt.0 .or. kpoint.gt.10) go to 102
      nextra = 5
      mfwa(kpoint+1) = jfwa
      mlwa(kpoint+1) = jlwda-nextra
      nwds = jlwda-jfwa
      modef = 4
      return
  101 continue
c     write (mprint,1001) jfwa,jlwda
c
      call uliber (0,38h0loci(ifw).gt.loci(lwd) in flash4 call,38)
      call perror
      return
  102 continue
c     write (mprint,1002) kpoint
c
      call uliber (0,43h0third argument out of range in flash4 call,43)
      call perror
      return
c
c1001 format (10h0loci(ifw)=,i10,10x,9hloci(lwd)=,i10)
c1002 format (27h0flash4 called with ipoint=,i5)
c
      end
