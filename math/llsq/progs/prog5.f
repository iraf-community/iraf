c     prog5
c     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 jun 12
c     to appear in 'solving least squares problems', prentice-hall, 1974
c	   example of the use of subroutines bndacc and bndsol to solve
c     sequentially the banded least squares problem which arises in
c     spline curve fitting.
c
      dimension x(12),y(12),b(10),g(12,5),c(12),q(4),cov(12)
c
c	 define functions p1 and p2 to be used in constructing
c	 cubic splines over uniformly spaced breakpoints.
c
      p1(t)=.25*t**2*t
      p2(t)=-(1.-t)**2*(1.+t)*.75+1.
      zero=0.
c
      write (6,230)
      mdg=12
      nband=4
      m=12
c				   set ordinates of data
      y(1)=2.2
      y(2)=4.0
      y(3)=5.0
      y(4)=4.6
      y(5)=2.8
      y(6)=2.7
      y(7)=3.8
      y(8)=5.1
      y(9)=6.1
      y(10)=6.3
      y(11)=5.0
      y(12)=2.0
c				   set abcissas of data
	   do 10 i=1,m
   10	   x(i)=2*i
c
c     begin loop thru cases using increasing nos of breakpoints.
c
	   do 150 nbp=5,10
	   nc=nbp+2
c				   set breakpoints
	   b(1)=x(1)
	   b(nbp)=x(m)
	   h=(b(nbp)-b(1))/float(nbp-1)
	   if (nbp.le.2) go to 30
		do 20 i=3,nbp
   20		b(i-1)=b(i-2)+h
   30	   continue
	   write (6,240) nbp,(b(i),i=1,nbp)
c
c     initialize ir and ip before first call to bndacc.
c
	   ir=1
	   ip=1
	   i=1
	   jt=1
   40	   mt=0
   50	   continue
	   if (x(i).gt.b(jt+1)) go to 60
c
c			 set row  for ith data point
c
	   u=(x(i)-b(jt))/h
	   ig=ir+mt
	   g(ig,1)=p1(1.-u)
	   g(ig,2)=p2(1.-u)
	   g(ig,3)=p2(u)
	   g(ig,4)=p1(u)
	   g(ig,5)=y(i)
	   mt=mt+1
	   if (i.eq.m) go to 60
	   i=i+1
	   go to 50
c
c		    send block of data to processor
c
   60	   continue
	   call bndacc (g,mdg,nband,ip,ir,mt,jt)
	   if (i.eq.m) go to 70
	   jt=jt+1
	   go to 40
c		    compute solution c()
   70	   continue
	   call bndsol (1,g,mdg,nband,ip,ir,c,nc,rnorm)
c		   write solution coefficients
	   write (6,180) (c(l),l=1,nc)
	   write (6,210) rnorm
c
c	       compute and print x,y,yfit,r=y-yfit
c
	   write (6,160)
	   jt=1
		do 110 i=1,m
   80		if (x(i).le.b(jt+1)) go to 90
		jt=jt+1
		go to 80
c
   90		u=(x(i)-b(jt))/h
		q(1)=p1(1.-u)
		q(2)=p2(1.-u)
		q(3)=p2(u)
		q(4)=p1(u)
		yfit=zero
		     do 100 l=1,4
		     ic=jt-1+l
  100		     yfit=yfit+c(ic)*q(l)
		r=y(i)-yfit
		write (6,170) i,x(i),y(i),yfit,r
  110		continue
c
c     compute residual vector norm.
c
	   if (m.le.nc) go to 150
	   sigsq=(rnorm**2)/float(m-nc)
	   sigfac=sqrt(sigsq)
	   write (6,220) sigfac
	   write (6,200)
c
c     compute and print cols. of covariance.
c
		do 140 j=1,nc
		     do 120 i=1,nc
  120		     cov(i)=zero
		cov(j)=1.
		call bndsol (2,g,mdg,nband,ip,ir,cov,nc,rdummy)
		call bndsol (3,g,mdg,nband,ip,ir,cov,nc,rdummy)
c
c    compute the jth col. of the covariance matrix.
		     do 130 i=1,nc
  130		     cov(i)=cov(i)*sigsq
  140		write (6,190) (l,j,cov(l),l=1,nc)
  150	   continue
      stop
  160 format (4h0  i,8x,1hx,10x,1hy,6x,4hyfit,4x,8hr=y-yfit/1x)
  170 format (1x,i3,4x,f6.0,4x,f6.2,4x,f6.2,4x,f8.4)
  180 format (4h0c =,10f10.5/(4x,10f10.5))
  190 format (4(02x,2i5,e15.7))
  200 format (46h0covariance matrix of the spline coefficients.)
  210 format (9h0rnorm	=,e15.8)
  220 format (9h0sigfac =,e15.8)
  230 format (50h1prog5.    execute a sequence of cubic spline fits,27h
     1to a discrete set of data.)
  240 format (1x////,11h0new case..,/47h0number of breakpoints, includin
     1g endpoints, is,i5/14h0breakpoints =,10f10.5,/(14x,10f10.5))
      end
