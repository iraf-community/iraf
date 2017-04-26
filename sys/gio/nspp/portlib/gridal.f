      subroutine gridal (imajx,iminx,imajy,iminy,ixlab,iylab,iflg,x,y)
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
c non-compact version of gridal
c
c     ray bovet ishft changed to ishfta patch
      dimension       nmaj(2),nmin(2),nlab(2),nflg(2),num(2) ,zza(2) ,
     1                zzb(2) ,zzc(2) ,zzd(2) ,ichars(5)      ,
     2                ifmt(3,2)      ,iz(2)  ,iza(2) ,izb(2) ,imz(2) ,
     3                izdec(2)       ,isiz(2),imajl(2)       ,
     4                iminl(2)       ,itype(2)       ,zz(2)  ,
     5                ishfta(2)       ,izaa(2),izbb(2),kz(4)
c     ray bovet patch to avoid small integers being set to 0
      integer x,y,xx,yy
c
c
c     ray bovet ishft changed to ishfta patch
      equivalence     (xxa,zza(1))   ,(xxb,zzb(1))   ,(xxc,zzc(1))   ,
     1                (xxd,zzd(1))   ,(mfmtx(1),ifmt(1,1)),
     2                (mx,iz(1))     ,(mxa,iza(1))   ,(mxb,izb(1))   ,
     3                (majx,nmaj(1)) ,(minx,nmin(1)) ,(mumx,num(1))  ,
     4                (mxdec,izdec(1))       ,(msizx,isiz(1)),
     5                (mmgrx,imajl(1))       ,(mmnrx,iminl(1))       ,
     6                (mtypex,itype(1))      ,(xx,kz(1))     ,
     7                (xx,zz(1))     ,(mshftx,ishfta(1))
c
c set up variables for loop
c
      nmaj(1) = imajx
      nmaj(2) = imajy
      nmin(1) = iminx
      nmin(2) = iminy
      nlab(1) = ixlab
      nlab(2) = iylab
      nflg(1) = ishift(iflg,-2)-1
      nflg(2) = iand(iflg,3)-1
      izaa(1) = iza(1)
      izaa(2) = iza(2)
      izbb(1) = izb(1)
      izbb(2) = izb(2)
      if (nflg(1).le.0 .and. nflg(2).le.0) go to 101
      xx = x
      yy = y
      call trans
      if (nflg(2) .gt. 0) izaa(1) = mx
      if (nflg(1) .gt. 0) izaa(2) = my
      if (nflg(2) .gt. 0) izbb(1) = mx
      if (nflg(1) .gt. 0) izbb(2) = my
  101 continue
      call optn (4hdpat,65535)
      do 121 i=1,2
c
c i=1 for x axis with ticks in y direction
c i=2 for y axis with ticks in x direction
c
         if (nlab(i)) 121,102,102
  102    continue
c
c ior.ne.0 posibility for x only
c
         ixor = (2-i)*90*mxor
         imaj = max0(nmaj(i),1)
         imin = max0(nmin(i),1)
         begin = iza(i)
         biginc = float(izb(i)-iza(i))/float(imaj)
         smlinc = biginc/float(imin)
         start = zzc(i)
         dif = (zzd(i)-zzc(i))/float(imaj)
         iop = 3-i
c
c iop is the opposit axis to i
c
         idec = izdec(iop)
         if (idec .eq. 0) idec = izaa(iop)-izbb(iop)-655
         if (ixor .eq. i-1) go to 103
c
c labels and axis are orthogonal
c
         icent = isign(1,idec-1)
         go to 104
c
c labels and axis are parallel
c
  103    icent = 0
  104    continue
         if (itype(i) .eq. 0) go to 105
         fact = 10.**imaj
         if (zzc(i) .gt. zzd(i)) fact = 1./fact
         val = zzc(i)/fact
         delval = val
         if (imin.le.10 .and. imaj.eq.1) imin = 9
         if (imin .ne. 9) imin = 1
         imaj = abs(alog10(zzd(i)/zzc(i)))+1.0001
  105    imajp1 = imaj+1
         iminm1 = imin-1
         do 119 j=1,imajp1
            part = j-1
c
c draw major line or tick
c
            call optn (4hintn,4hhigh)
            if (itype(i) .ne. 0) go to 106
            iz(i) = begin+part*biginc
            go to 107
  106       val = val*fact
            zz(i) = val
            kz(iop) = 1
            call trans
            delval = delval*fact
            if (iz(i)-10 .gt. izb(i)) go to 120
  107       continue
            iz(iop) = izaa(iop)
            minst = 0
            call put42
            if (nflg(i)) 108,109,109
  108       iz(iop) = izb(iop)
            minst = 1
            call put42
            go to 111
  109       iz(iop) = izaa(iop)+imajl(iop)
            minst = 1
            call put42
            if (nflg(i)) 110,110,111
  110       iz(iop) = izb(iop)
            minst = 0
            call put42
            iz(iop) = izb(iop)-imajl(iop)
            minst = 1
            call put42
  111       continue
c
c form label if needed
c
            if (nlab(i) .le. 0) go to 112
            if (itype(i) .eq. 0) val = start+part*dif
            call encode (num(i),ifmt(1,i),ichars,val)
c     ray bovet ishft changed to ishfta patch
            imz(i) = ishift(iz(i),-ishfta(i))
            imz(iop) = max0(1,ishift(izaa(iop)-idec,-ishfta(iop)))
            njust = num(i)
            if (icent .eq. 0) call justfy (ichars,num(i),njust)
            call pwrit (imz(1),imz(2),ichars,njust,isiz(i),ixor,icent)
c
c put in minor ticks
c
  112       if (iminm1.le.0 .or. j.eq.imajp1) go to 119
            call optn (4hintn,3hlow)
            do 118 k=1,iminm1
               if (itype(i) .ne. 0) go to 113
               iz(i) = begin+part*biginc+float(k)*smlinc
               go to 114
  113          zz(i) = val+float(k)*delval
               if (zzc(i) .gt. zzd(i)) zzi = val-float(k)*delval*.1
               kz(iop) = 1
               call trans
               if (iz(i) .gt. izb(i)) go to 120
               if (iz(i) .lt. iza(i)) go to 118
  114          continue
               iz(iop) = izaa(iop)
               minst = 0
               call put42
               if (nflg(i)) 115,116,116
  115          iz(iop) = izb(iop)
               minst = 1
               call put42
               go to 118
  116          iz(iop) = izaa(iop)+iminl(iop)
               minst = 1
               call put42
               if (nflg(i)) 117,117,118
  117          iz(iop) = izb(iop)
               minst = 0
               call put42
               iz(iop) = izb(iop)-iminl(iop)
               minst = 1
               call put42
  118       continue
  119    continue
         call optn (4hintn,4hhigh)
  120    if (nflg(iop) .lt. 0) go to 121
c
c draw axis line
c
         iz(i) = iza(i)
         iz(iop) = izaa(iop)
         minst = 0
         call put42
         iz(i) = izb(i)
         iz(iop) = izaa(iop)
         minst = 1
         call put42
         if (nflg(i) .gt. 0) go to 121
         iz(i) = iza(i)
         iz(iop) = izb(iop)
         minst = 0
         call put42
         iz(i) = izb(i)
         iz(iop) = izb(iop)
         minst = 1
         call put42
  121 continue
      return
      end
