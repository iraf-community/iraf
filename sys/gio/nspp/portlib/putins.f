      subroutine putins (nopcd,ins,nchar)
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
      dimension       ins(nchar)
      kopcd = nopcd
      kchar = nchar
c
c put in the two header bytes
c
      if (kopcd.lt.0 .or. kopcd.gt.63) go to 102
      mbpair = ior(ishift(kopcd+192,8),kchar)
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      if (kchar .eq. 0) return
      if (kchar.lt.0 .or. kchar.ge.255) go to 103
c
c put character string into instruction string
c
      do 101 i=1,kchar,2
         call ncgchr (ins,kchar,i,jcharl)
         call ncgchr (ins,kchar,i+1,jcharr)
         mbpair = ior(ishift(jcharl,8),jcharr)
         mipair = mipair+1
         mbprs(mipair) = mbpair
         if (mipair .ge. 16) call flushb
  101 continue
      return
  102 continue
c     write (mprint,1001) kopcd
c
      call uliber (0,40h0in putins call, nopcd .lt. 0 or .ge. 63,40)
      call perror
      return
  103 continue
c     write (mprint,1002) kchar
c
      call uliber (0,41h0in putins call, nchar .le. 0 or .ge. 255,41)
      call perror
      return
c
c1001 format (7h0nopcd=,i10)
c1002 format (7h0nchar=,i10)
c
      end
