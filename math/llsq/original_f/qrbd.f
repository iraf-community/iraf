c     subroutine qrbd (ipass,q,e,nn,v,mdv,nrv,c,mdc,ncc)
c     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 jun 12
c     to appear in 'solving least squares problems', prentice-hall, 1974
c	   qr algorithm for singular values of a bidiagonal matrix.
c
c     the bidiagonal matrix
c
c			(q1,e2,0...    )
c			(   q2,e3,0... )
c		 d=	(	.      )
c			(	  .   0)
c			(	    .en)
c			(	   0,qn)
c
c		  is pre and post multiplied by
c		  elementary rotation matrices
c		  ri and pi so that
c
c		  rk...r1*d*p1**(t)...pk**(t) = diag(s1,...,sn)
c
c		  to within working accuracy.
c
c  1. ei and qi occupy e(i) and q(i) as input.
c
c  2. rm...r1*c replaces 'c' in storage as output.
c
c  3. v*p1**(t)...pm**(t) replaces 'v' in storage as output.
c
c  4. si occupies q(i) as output.
c
c  5. the si's are nonincreasing and nonnegative.
c
c     this code is based on the paper and 'algol' code..
c ref..
c  1. reinsch,c.h. and golub,g.h. 'singular value decomposition
c     and least squares solutions' (numer. math.), vol. 14,(1970).
c
      subroutine qrbd (ipass,q,e,nn,v,mdv,nrv,c,mdc,ncc)
      logical wntv ,havers,fail
      dimension q(nn),e(nn),v(mdv,nn),c(mdc,ncc)
      zero=0.
      one=1.
      two=2.
c
      n=nn
      ipass=1
      if (n.le.0) return
      n10=10*n
      wntv=nrv.gt.0
      havers=ncc.gt.0
      fail=.false.
      nqrs=0
      e(1)=zero
      dnorm=zero
	   do 10 j=1,n
   10	   dnorm=amax1(abs(q(j))+abs(e(j)),dnorm)
	   do 200 kk=1,n
	   k=n+1-kk
c
c     test for splitting or rank deficiencies..
c	  first make test for last diagonal term, q(k), being small.
   20	    if(k.eq.1) go to 50
	    if(diff(dnorm+q(k),dnorm)) 50,25,50
c
c     since q(k) is small we will make a special pass to
c     transform e(k) to zero.
c
   25	   cs=zero
	   sn=-one
		do 40 ii=2,k
		i=k+1-ii
		f=-sn*e(i+1)
		e(i+1)=cs*e(i+1)
		call g1 (q(i),f,cs,sn,q(i))
c	  transformation constructed to zero position (i,k).
c
		if (.not.wntv) go to 40
		     do 30 j=1,nrv
   30		     call g2 (cs,sn,v(j,i),v(j,k))
c	       accumulate rt. transformations in v.
c
   40		continue
c
c	  the matrix is now bidiagonal, and of lower order
c	  since e(k) .eq. zero..
c
   50		do 60 ll=1,k
		l=k+1-ll
		if(diff(dnorm+e(l),dnorm)) 55,100,55
   55		if(diff(dnorm+q(l-1),dnorm)) 60,70,60
   60		continue
c     this loop can't complete since e(1) = zero.
c
	   go to 100
c
c	  cancellation of e(l), l.gt.1.
   70	   cs=zero
	   sn=-one
		do 90 i=l,k
		f=-sn*e(i)
		e(i)=cs*e(i)
		if(diff(dnorm+f,dnorm)) 75,100,75
   75		call g1 (q(i),f,cs,sn,q(i))
		if (.not.havers) go to 90
		     do 80 j=1,ncc
   80		     call g2 (cs,sn,c(i,j),c(l-1,j))
   90		continue
c
c	  test for convergence..
  100	   z=q(k)
	   if (l.eq.k) go to 170
c
c	  shift from bottom 2 by 2 minor of b**(t)*b.
	   x=q(l)
	   y=q(k-1)
	   g=e(k-1)
	   h=e(k)
	   f=((y-z)*(y+z)+(g-h)*(g+h))/(two*h*y)
	   g=sqrt(one+f**2)
	   if (f.lt.zero) go to 110
	   t=f+g
	   go to 120
  110	   t=f-g
  120	   f=((x-z)*(x+z)+h*(y/t-h))/x
c
c	  next qr sweep..
	   cs=one
	   sn=one
	   lp1=l+1
		do 160 i=lp1,k
		g=e(i)
		y=q(i)
		h=sn*g
		g=cs*g
		call g1 (f,h,cs,sn,e(i-1))
		f=x*cs+g*sn
		g=-x*sn+g*cs
		h=y*sn
		y=y*cs
		if (.not.wntv) go to 140
c
c	       accumulate rotations (from the right) in 'v'
		     do 130 j=1,nrv
  130		     call g2 (cs,sn,v(j,i-1),v(j,i))
  140		call g1 (f,h,cs,sn,q(i-1))
		f=cs*g+sn*y
		x=-sn*g+cs*y
		if (.not.havers) go to 160
		     do 150 j=1,ncc
  150		     call g2 (cs,sn,c(i-1,j),c(i,j))
c	       apply rotations from the left to
c	       right hand sides in 'c'..
c
  160		continue
	   e(l)=zero
	   e(k)=f
	   q(k)=x
	   nqrs=nqrs+1
	   if (nqrs.le.n10) go to 20
c	   return to 'test for splitting'.
c
	   fail=.true.
c     ..
c     cutoff for convergence failure. 'nqrs' will be 2*n usually.
  170	   if (z.ge.zero) go to 190
	   q(k)=-z
	   if (.not.wntv) go to 190
		do 180 j=1,nrv
  180		v(j,k)=-v(j,k)
  190	   continue
c	  convergence. q(k) is made nonnegative..
c
  200	   continue
      if (n.eq.1) return
	   do 210 i=2,n
	   if (q(i).gt.q(i-1)) go to 220
  210	   continue
      if (fail) ipass=2
      return
c     ..
c     every singular value is in order..
  220	   do 270 i=2,n
	   t=q(i-1)
	   k=i-1
		do 230 j=i,n
		if (t.ge.q(j)) go to 230
		t=q(j)
		k=j
  230		continue
	   if (k.eq.i-1) go to 270
	   q(k)=q(i-1)
	   q(i-1)=t
	   if (.not.havers) go to 250
		do 240 j=1,ncc
		t=c(i-1,j)
		c(i-1,j)=c(k,j)
  240		c(k,j)=t
  250	   if (.not.wntv) go to 270
		do 260 j=1,nrv
		t=v(j,i-1)
		v(j,i-1)=v(j,k)
  260		v(j,k)=t
  270	   continue
c	  end of ordering algorithm.
c
      if (fail) ipass=2
      return
      end
