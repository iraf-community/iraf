      subroutine set (xa,xb,ya,yb,xc,xd,yc,yd,itype)
c
c *************** KPNO -- name changed from set to sppset **********
c
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
      integer xa,xb,ya,yb,xxa,xxb,yya,yyb,zz
      logical         intt
      dimension       zz(4)  ,mz(4)  ,zc(2)  ,zd(2)  ,zfactr(2)      ,
     1                zadd(2),mtypez(2)
      dimension       mshftz(2)
      dimension       mes(2)
      equivalence     (xxc,zc(1))    ,(xxd,zd(1))    ,(xxa,zz(1))    ,
     1                (mxa,mz(1))    ,(xfactr,zfactr(1))     ,
     2                (xadd,zadd(1)) ,(mtypex,mtypez(1))     ,
     3                (mshftx,mshftz(1))     ,(temp,itemp)
      data mes(1),mes(2)/1hx,1hy/
      xxa = xa
      xxb = xb
      xxc = xc
      xxd = xd
      yya = ya
      yyb = yb
      yyc = yc
      yyd = yd
      mtype = itype
      mtypex = (mtype-1)/2
      mtypey = mod(mtype-1,2)
c
c find mxa, mxb, etc by mapping xxa, xxb, etc into integer space if they
c are not integers
c
      do 103 i=1,4
         k = i
         if (k .gt. 2) k = k-2
c     ray bovet patch to avoid small integers being set to 0
c        temp = zz(i)
         itemp = zz(i)
c        if (temp .lt. 0.0) go to 106
c
         if (.not.(intt(temp))) go to 101
         if (itemp.lt.0) go to 106
         itemp = ishift(itemp-1,mshftz(k))
         go to 102
c     ray bovet patch to avoid small integers being set to 0
c 101    itemp = temp*32767.
  101    if(temp.lt.0.0) go to 106
         itemp = temp*32767.
c
  102    if (itemp.lt.0 .or. itemp.gt.32767) go to 107
         mz(i) = itemp
  103 continue
c
c set up parameters for translating real input from frstpt, etc. to
c integer plotting space
c
      do 105 i=1,2
         prange = mz(i+2)-mz(i)
         urange = zd(i)-zc(i)
c
c test for no range
c
         if (urange.eq.0. .or. prange.eq.0.) go to 108
c
c test for log scaling
c
         if (mtypez(i) .eq. 0) go to 104
c
c test for error
c
         if (zc(i) .le. 0.) go to 109
         if (zd(i) .le. 0.) go to 110
         urange = alog10(zd(i)/zc(i))
         zfactr(i) = prange/urange
         zadd(i) = float(mz(i))-zfactr(i)*alog10(zc(i))
         go to 105
  104    zfactr(i) = prange/urange
         zadd(i) = float(mz(i))-zfactr(i)*zc(i)
  105 continue
      return
c
c error processing
c
  106 continue
      if (i.gt.1 .and. i.lt.4) i = 5-i
c     write (mprint,1001) i
c
      call uliber (0,53h0negative values not allowed in first 4 set argu
     1ments        ,53)
      call perror
      return
  107 continue
      if (i.gt.1 .and. i.lt.4) i = 5-i
c     write (mprint,1002) i
c
      call uliber (0,83h0first 4 set arguments must be real between 0 an
     1d 1 or integers between 1 and 32767,83)
      call perror
      return
  108 continue
      i1 = i*2+3
      i2 = i*2+4
c     write (mprint,1003) i1,i2
c
      call uliber (0,31h0no range in x or y in set call,31)
      call perror
      return
  109 continue
c 109 write (mprint,1004) mes(i)
      go to 111
  110 continue
c 110 write (mprint,1005) mes(i)
c
  111 call uliber (0,46h0non-positive argument to set with log scaling,
     1             46)
      call perror
      return
c
c1001 format (9h0argument,i2,9h negative)
c1002 format (9h0argument,i2,13h out of range)
c1003 format (10h0arguments,i2,4h and,i2,14h are identical)
c1004 format (1h0,a1,8hc .le. 0)
c1005 format (1h0,a1,8hd .le. 0)
c
      end
