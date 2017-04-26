      subroutine flash3 (ipoint)
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
      if (modef .lt. 2) go to 102
      kpoint = ipoint
      if (kpoint.lt.0 .or. kpoint.gt.10) go to 103
      if (mfwa(kpoint+1) .eq. -9999) go to 102
      call mcflsh
      isave1 = mbufa
      isave2 = mbuflu
      mbufa = mfwa(kpoint+1)
      nlentg = mlwa(kpoint+1)-mbufa+1
      isub = mbufa+nlentg-loci(idummy)
      nusrwc = idummy(isub+1)
      if (nusrwc .ne. nlentg) go to 104
      modef = -3
  101 mbuflu = min0(nlentg,msblen)
      if (mbuflu .gt. 0) call preout
      nlentg = nlentg-msblen
      mbufa = mbufa+msblen
      if (nlentg .gt. 0) go to 101
      mbufa = isave1
      mbuflu = isave2
      mxold = -9999
      myold = -9999
      modef = 3
      mjxmin = min0(mjxmin,idummy(isub+2))
      mjymin = min0(mjymin,idummy(isub+3))
      mjxmax = max0(mjxmax,idummy(isub+4))
      mjymax = max0(mjymax,idummy(isub+5))
      return
  102 continue
c     write (mprint,1001) kpoint
c
      call uliber (0,
     1             48h0flash3 called without call to flash1 and flash2,
     2             48)
      call perror
      return
  103 continue
c     write (mprint,1001) kpoint
c
      call uliber (0,37h0argument out of range in flash3 call,37)
      call perror
      return
  104 continue
c     write (mprint,1001) kpoint
c
      call uliber (0,37h0user flash buffer has been corrupted,37)
      call perror
      return
c
c1001 format (27h0flash3 called with ipoint=,i5)
c
      end
