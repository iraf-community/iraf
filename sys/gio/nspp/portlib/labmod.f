      subroutine labmod (ifmtx,ifmty,numx,numy,isizx,isizy,ixdec,iydec,
     1                   ixor)
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
c     ray bovet ishft changed to ishfta patch
      dimension       ifmtx(3)       ,ifmty(3)       ,idec(2),ishfta(2)
      equivalence     (mxdec,idec(1)),(mshftx,ishfta(1))
      do 101 i=1,10
         call ncgchr (ifmtx,10,i,ichar)
         call ncpchr (mfmtx,10,i,ichar)
         call ncgchr (ifmty,10,i,ichar)
         call ncpchr (mfmty,10,i,ichar)
  101 continue
      mumx = numx
      mumy = numy
      if (max0(mumx,mumy) .gt. 20) go to 103
      msizx = isizx
      msizy = isizy
      mxdec = ixdec
      mydec = iydec
      do 102 i=1,2
c     ray bovet ishft changed to ishfta patch
         jdec = isign(ishift(iabs(idec(i)),ishfta(i)),idec(i))
         if (idec(i) .eq. 0) jdec = 655
         if (idec(i) .eq. 1) jdec = 0
         idec(i) = jdec
  102 continue
      mxor = ixor
      return
  103 continue
c     write (mprint,1001) mumx,mumy
c
      call uliber (0,36h0numx or numy .gt. 20 in labmod call,36)
      call perror
      return
c
c1001 format (6h0numx=,i5,6h numy=,i5)
c
      end
