      subroutine pixels(x0,y0,ni,nj,inten)
      integer*2 inten(1)
c	assume inten is a linear array rather than 2-d.  This is a change
c	from the original code.
c	assume nj == 1
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
      data ipixop / 10/
      mbpair = ior(ishift(ior(192, ipixop + 1), 8), 8)
      mipair = mipair + 1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      xx = x0
      yy = y0
      call tran16
      mbpair = mx
      mipair = mipair + 1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      mbpair=my
      mipair = mipair + 1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      mbpair=ni
      mipair = mipair + 1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      mbpair=nj
      mipair = mipair + 1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      nni = max0(1,(ni+iand(ni,1)))
      nnj = max0(1,nj)
      kmax=nni*nnj
      k=0
      do 200 j=1,nnj
      do 100 i=1,nni
      if(mod(k,254).ne.0) goto 90
      mbpair = ior(ishift(ior(192, ipixop+2),8), min0(254,kmax-k))
      mipair = mipair + 1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      mbpair = 0
   90 ik = iand ( i, 1)
c
c 14Nov85 mod so that arguments to ishift are of same type
      itmp = inten(i)
      mbpair = ior (ishift(iand(itmp,255),8*ik),mbpair)
c     mbpair = ior (ishift(iand(inten(i),255),8*ik),mbpair)
c
      if ( ik .ne. 0 ) go to 95
      mipair = mipair + 1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      mbpair = 0
   95 k = k + 1
  100 continue
  200 continue
      return
      end
