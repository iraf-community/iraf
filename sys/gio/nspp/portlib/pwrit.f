      subroutine pwrit (x,y,ichar,nchar,isize,ioren,icent)
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
      dimension       ichar(nchar)
      dimension       iwide(4)
      data wide,high,white /6.,7.,2./
      data iwide(1),iwide(2),iwide(3),iwide(4)/256,384,512,768/
c
c copy parameters into local variables
c
      kchar = nchar
      ksize = isize
      koren = ioren
c
c transform character size into metacode units.
c
      if (ksize .gt. 3) ksize = ishift(ksize,mshftx)
      if (ksize .le. 3) ksize = iwide(ksize+1)
      call optn (4hcsiz,ksize)
c
c transform orientation.
c
      if (koren .lt. 0) koren = koren+360
      if (koren .ge. 0) call optn (4horen,koren)
c
c pass on centering.
c
      call optn (4hcent,max0(0,min0(2,icent+1)))
c
c make coordinates global.
c
      xx = x
      yy = y
      call trans
c
c use real variables for convenience.
c
      fmx = mx
      fmy = my
c
c work with radians instead of degrees.
c 2*pi/360. approximately = .0174533
c
      angle = float(koren)*.0174533
c
c find starting point for string when considering centering option.
c
      cosa = cos(angle)
      sina = sin(angle)
      wide2 = ksize/2
      widen = float(ksize*kchar)-float(ksize)*white/wide
      if (icent) 103,101,102
  101 fmx = fmx-cosa*widen*.5
      fmy = fmy-sina*widen*.5
      go to 103
  102 fmx = fmx-cosa*widen
      fmy = fmy-sina*widen
  103 continue
      hgt2 = (3*ksize)/4
      nxul = fmx-cosa*wide2-sina*hgt2
      nyul = fmy+cosa*hgt2-sina*wide2
      nxll = fmx-cosa*wide2+sina*hgt2
      nyll = fmy-cosa*hgt2-sina*wide2
      nxur = fmx+cosa*widen+cosa*wide2-sina*hgt2
      nyur = fmy+sina*widen+cosa*hgt2+sina*wide2
      nxlr = fmx+cosa*widen+cosa*wide2+sina*hgt2
      nylr = fmy+sina*widen-cosa*hgt2+sina*wide2
      mjxmax = min0(32767,max0(mjxmax,nxul,nxll,nxur,nxlr))
      mjxmin = max0(0,min0(mjxmin,nxul,nxll,nxur,nxlr))
      mjymax = min0(32767,max0(mjymax,nyul,nyll,nyur,nylr))
      mjymin = max0(0,min0(mjymin,nyul,nyll,nyur,nylr))
      minst = 0
      call put42
      call putins (33,ichar,kchar)
      mxold = -9999
      myold = -9999
      return
      end
