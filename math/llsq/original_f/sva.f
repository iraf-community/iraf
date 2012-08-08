      subroutine sva (a,mda,m,n,mdata,b,sing,names,iscale,d)
c     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 jun 12
c     to appear in 'solving least squares problems', prentice-hall, 1974
c		singular value analysis printout
c
c     iscale	   set by user to 1, 2, or 3 to select column scaling
c		   option.
c		   1   subr will use identity scaling and ignore the d()
c		       array.
c		   2   subr will scale nonzero cols to have unit euclide
c		       length and will store reciprocal lengths of
c		       original nonzero cols in d().
c		   3   user supplies col scale factors in d(). subr
c		       will mult col j by d(j) and remove the scaling
c		       from the soln at the end.
c
      dimension  a(mda,n), b(m), sing(01),d(n)
c     sing(3*n)
      dimension names(n)
      logical test
      double precision	 sb, dzero
      dzero=0.d0
      one=1.
      zero=0.
      if (m.le.0 .or. n.le.0) return
      np1=n+1
      write (6,270)
      write (6,260) m,n,mdata
      go to (60,10,10), iscale
c
c     apply column scaling to a
c
   10	   do 50 j=1,n
	   a1=d(j)
	   go to (20,20,40), iscale
   20	   sb=dzero
		do 30 i=1,m
   30		sb=sb+dble(a(i,j))**2
	   a1=dsqrt(sb)
	   if (a1.eq.zero) a1=one
	   a1=one/a1
	   d(j)=a1
   40		do 50 i=1,m
   50		a(i,j)=a(i,j)*a1
      write (6,280) iscale,(d(j),j=1,n)
      go to 70
   60 continue
      write (6,290)
   70 continue
c
c     obtain  sing. value decomp. of scaled matrix.
c
c**********************************************************
      call svdrs (a,mda,m,n,b,1,1,sing)
c**********************************************************
c
c     print the v matrix.
c
      call mfeout (a,mda,n,n,names,1)
      if (iscale.eq.1) go to 90
c
c     replace v by d*v in the array a()
c
	   do 80 i=1,n
		do 80 j=1,n
   80		a(i,j)=d(i)*a(i,j)
c
c     g  now in  b array.  v now in a array.
c
c
c     obtain summary output.
c
   90 continue
      write (6,220)
c
c     compute cumulative sums of squares of components of
c     g and store them in sing(i), i=minmn+1,...,2*minmn+1
c
      sb=dzero
      minmn=min0(m,n)
      minmn1=minmn+1
      if (m.eq.minmn) go to 110
	   do 100 i=minmn1,m
  100	   sb=sb+dble(b(i))**2
  110 sing(2*minmn+1)=sb
	   do 120 jj=1,minmn
	   j=minmn+1-jj
	   sb=sb+dble(b(j))**2
	   js=minmn+j
  120	   sing(js)=sb
      a3=sing(minmn+1)
      a4=sqrt(a3/float(max0(1,mdata)))
      write (6,230) a3,a4
c
      nsol=0
c
c
c
	   do 160 k=1,minmn
	   if (sing(k).eq.zero) go to 130
	   nsol=k
	   pi=b(k)/sing(k)
	   a1=one/sing(k)
	   a2=b(k)**2
	   a3=sing(minmn1+k)
	   a4=sqrt(a3/float(max0(1,mdata-k)))
	   test=sing(k).ge.100..or.sing(k).lt..001
	   if (test) write (6,240) k,sing(k),pi,a1,b(k),a2,a3,a4
	   if (.not.test) write (6,250) k,sing(k),pi,a1,b(k),a2,a3,a4
	   go to 140
  130	   write (6,240) k,sing(k)
	   pi=zero
  140		do 150 i=1,n
		a(i,k)=a(i,k)*pi
  150		if (k.gt.1) a(i,k)=a(i,k)+a(i,k-1)
  160	   continue
c
c     compute and print values of ynorm and rnorm.
c
      write (6,300)
      j=0
      ysq=zero
      go to 180
  170 j=j+1
      ysq=ysq+(b(j)/sing(j))**2
  180 ynorm=sqrt(ysq)
      js=minmn1+j
      rnorm=sqrt(sing(js))
      yl=-1000.
      if (ynorm.gt.0.) yl=alog10(ynorm)
      rl=-1000.
      if (rnorm.gt.0.) rl=alog10(rnorm)
      write (6,310) j,ynorm,rnorm,yl,rl
      if (j.lt.nsol) go to 170
c
c     compute values of xnorm and rnorm for a sequence of values of
c     the levenberg-marquardt parameter.
c
      if (sing(1).eq.zero) go to 210
      el=alog10(sing(1))+one
      el2=alog10(sing(nsol))-one
      del=(el2-el)/20.
      ten=10.
      aln10=alog(ten)
      write (6,320)
	   do 200 ie=1,21
c     compute	     alamb=10.**el
	   alamb=exp(aln10*el)
	   ys=0.
	   js=minmn1+nsol
	   rs=sing(js)
		do 190 i=1,minmn
		sl=sing(i)**2+alamb**2
		ys=ys+(b(i)*sing(i)/sl)**2
		rs=rs+(b(i)*(alamb**2)/sl)**2
  190		continue
	   ynorm=sqrt(ys)
	   rnorm=sqrt(rs)
	   rl=-1000.
	   if (rnorm.gt.zero) rl=alog10(rnorm)
	   yl=-1000.
	   if (ynorm.gt.zero) yl=alog10(ynorm)
	   write (6,330) alamb,ynorm,rnorm,el,yl,rl
	   el=el+del
  200	   continue
c
c     print candidate solutions.
c
  210 if (nsol.ge.1) call mfeout (a,mda,n,nsol,names,2)
      return
  220 format (42h0 index  sing. value       p coef         ,48hrecip. s.
     1v.             g coef            g**2  ,39h            c.s.s.
     2    n.s.r.c.s.s.)
  230 format (1h ,5x,1h0,88x,1pe15.4,1pe17.4)
  240 format (1h ,i6,e12.4,1p(e15.4,4x,e15.4,4x,e15.4,4x,e15.4,4x,e15.4,
     12x,e15.4))
  250 format (1h ,i6,f12.4,1p(e15.4,4x,e15.4,4x,e15.4,4x,e15.4,4x,e15.4,
     12x,e15.4))
  260 format (5h0m = ,i6,8h,   n = ,i4,12h,   mdata = ,i8)
  270 format (45h0singular value analysis of the least squares,42h probl
     1em,  a*x=b,  scaled as  (a*d)*y=b  .)
  280 format (19h0scaling option no.,i2,18h.  d is a diagonal,46h matrix
     1 with the following diagonal elements../(5x,10e12.4))
  290 format (50h0scaling option no. 1.   d is the identity matrix./1x)
  300 format (6h0index,13x,28h         ynorm         rnorm,14x,28h  log1
     10(ynorm)  log10(rnorm)/1x)
  310 format (1h ,i4,14x,2e14.5,14x,2f14.5)
  320 format (54h0norms of solution and residual vectors for a range of,
     144h values of the levenberg-marquardt parameter,9h, lambda./1h0,4x
     2,42h        lambda         ynorm         rnorm,42h log10(lambda)
     3log10(ynorm)  log10(rnorm))
  330 format (5x,3e14.5,3f14.5)
      end
