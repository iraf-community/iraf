      subroutine preout
      dimension       idummy(1)
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
c+kpno
c Initialization moved to z8zpii.f.
c
      common /nsplt1/  iclrfb ,isetfb ,ibpw   ,ifwd
c     data iclrfb/0/, isetfb/0/, ibpw/32/, ifwd/1/
c-kpno
c
      kbufa = mbufa
c
c entry while in flash1 mode will cause restart of filling user buffer
c if its size is exceded.  otherwise it is assumed fixed-length output
c record size is exceded, so place for 4 bytes is reserved in user
c buffer, to allow proper record formatting during flash3 call.
c
      if (modef .ne. 1) go to 101
      if (mbuflu+4 .le. mbufl) go to 113
      mbuflu = 0
      mf2er = mf2er+1
      go to 113
c
c if necessary, build masks for setting and clearing new-frame flag
c
  101 if (iclrfb .ne. 0) go to 103
      iposn = ibpw*ifwd-21
      isetfb = ishift(1,iposn)
      do 102 i=1,ibpw
         ibit = 1
         if (i .eq. (ibpw-iposn)) ibit = 0
         iclrfb = ior(ishift(iclrfb,1),ibit)
  102 continue
c
c in flash3 mode, copy any shorter-than-record-length user buffer into
c system buffer, to avoid possible addressing error during fixed-length
c write.
c
  103 if (modef .ne. -3) go to 105
      if (mbuflu .eq. msblen) go to 105
      isub = kbufa-loci(idummy)+1
      do 104 i=1,mbuflu
         msybuf(i) = idummy(isub)
         isub = isub+1
  104 continue
      kbufa = loci(msybuf)
c
c compute metacode byte count and put in first 16 bits of buffer.
c       *** note that we are directly manipulating the
c           first 32 bits of the output buffer here ***
c
  105 mcrout = mcrout+1
      nbytes = -3+(ibpw*mbuflu-1)/8
      isub = kbufa-loci(idummy)+1
      idummy(isub) = ior(idummy(isub),ishift(nbytes,ibpw-16))
c
c put in first-record-of-frame flag if appropriate.  otherwise insure
c frame flag is zeroed.  put buffer out via writeb.
c
      isub = kbufa-loci(idummy)+ifwd
      if (mfrlst .ne. 1) go to 106
      idummy(isub) = ior(idummy(isub),isetfb)
      mfrlst = 0
      go to 107
  106 idummy(isub) = iand(idummy(isub),iclrfb)
  107 if (mbuflu .eq. msblen) go to 109
      isub = kbufa+mbuflu-loci(idummy)
      do 108 i=mbuflu,msblen
         isub = isub+1
         idummy(isub) = 0
  108 continue
  109 call writeb (kbufa,mbuflu,munit)
c
c if this is last buffer of frame, call writeb with zero-byte-count
c record, so that it may arrange that such a record follows the last
c frame of the metafile (note that mbufa points to msybuf when get here)
c
      if (mfrend .ne. 1) go to 112
      mfrlst = 1
      isub = kbufa-loci(idummy)
      do 110 i=1,mbuflu
         isub = isub+1
         idummy(isub) = 0
  110 continue
      do 111 i=1,16
         mbprs(i) = 0
  111 continue
      mbprs(2) = ior(mpair2,2048)
      call packum (mbprs,16,kbufa)
      call writeb (kbufa,0,munit)
c
c finish up by reserving 4 bytes at start of next output buffer.
c
  112 mbuflu = 0
  113 mbprs(1) = mpair1
      mbprs(2) = mpair2
      mipair = 2
      mflcnt = 0
      return
      end
