      subroutine optn (iopnam,iopval)
      dimension       iopnam(1)      ,iopval(1)
      dimension       ichar(3)
      logical         skip
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
c
      data ihigh,ilow/2hhi,2hlo/
c
c find index for input name
c
      do 101 i=1,9
         iop = i
         if (jlm2(iopnam) .eq. jlm2(mname(i))) go to 102
  101 continue
c
      call uliber (0,36hounknown name in optn or getopt call,36)
      call perror
      return
  102 continue
      if (iop.ne.2 .and. iop.ne.9) iopv = iopval(1)
c
c     if character input for intensity, change to numeric
c
      if (iop .ne. 2) go to 105
      jchar = jlm2(iopval)
      if (jchar .ne. jlm2(ihigh)) go to 103
      iopv = 204
      go to 105
  103 if (jchar .ne. jlm2(ilow)) go to 104
      iopv = 127
      go to 105
  104 iopv = iopval(1)
  105 continue
c
c     reset option if necessary
c
      if (iop .ne. 9) go to 107
      skip = modef .eq. 0
      do 106 i=1,3
         call ncgchr (iopval,3,i,ichar(i))
         call ncgchr (mop(iop),3,i,jchar)
         skip = skip .and. (jchar .eq. ichar(i))
         call ncpchr (mop(iop),3,i,ichar(i))
  106 continue
      if (skip) go to 109
      nchar = 4
      mbpair = 1*nchar+58112
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      mbpair = ior(ishift(iop,8),ichar(1))
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      mbpair = ior(ishift(ichar(2),8),ichar(3))
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      go to 109
  107 continue
      if (mop(iop).eq.iopv .and. modef.eq.0) go to 109
      mop(iop) = iopv
      nchar = 2
      if (iop.eq.6 .or. iop.eq.3 .or. iop.eq.4 .or. iop.eq.7) nchar = 4
      mbpair = 1*nchar+58112
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      if (nchar .eq. 4) go to 108
      mbpair = ior(ishift(iand(iop,255),8),iand(iopv,255))
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      go to 109
  108 mbpair = ishift(iop,8)
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
      mbpair = iand(iopv,65535)
      mipair = mipair+1
      mbprs(mipair) = mbpair
      if (mipair .ge. 16) call flushb
  109 return
      end
