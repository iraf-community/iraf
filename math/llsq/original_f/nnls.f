c     subroutine nnls  (a,mda,m,n,b,x,rnorm,w,zz,index,mode)
c     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 june 15
c     to appear in 'solving least squares problems', prentice-hall, 1974
c
c	  **********   nonnegative least squares   **********
c
c     given an m by n matrix, a, and an m-vector, b,  compute an
c     n-vector, x, which solves the least squares problem
c
c		       a * x = b  subject to x .ge. 0
c
c     a(),mda,m,n     mda is the first dimensioning parameter for the
c		      array, a().   on entry a() contains the m by n
c		      matrix, a.	   on exit a() contains
c		      the product matrix, q*a , where q is an
c		      m by m orthogonal matrix generated implicitly by
c		      this subroutine.
c     b()     on entry b() contains the m-vector, b.   on exit b() con-
c	      tains q*b.
c     x()     on entry x() need not be initialized.  on exit x() will
c	      contain the solution vector.
c     rnorm   on exit rnorm contains the euclidean norm of the
c	      residual vector.
c     w()     an n-array of working space.  on exit w() will contain
c	      the dual solution vector.   w will satisfy w(i) = 0.
c	      for all i in set p  and w(i) .le. 0. for all i in set z
c     zz()     an m-array of working space.
c     index()	  an integer working array of length at least n.
c		  on exit the contents of this array define the sets
c		  p and z as follows..
c
c		  index(1)   thru index(nsetp) = set p.
c		  index(iz1) thru index(iz2)   = set z.
c		  iz1 = nsetp + 1 = npp1
c		  iz2 = n
c     mode    this is a success-failure flag with the following
c	      meanings.
c	      1     the solution has been computed successfully.
c	      2     the dimensions of the problem are bad.
c		    either m .le. 0 or n .le. 0.
c	      3    iteration count exceeded.  more than 3*n iterations.
c
      subroutine nnls (a,mda,m,n,b,x,rnorm,w,zz,index,mode)
      dimension a(mda,n), b(m), x(n), w(n), zz(m)
      integer index(n)
      zero=0.
      one=1.
      two=2.
      factor=0.01
c
      mode=1
      if (m.gt.0.and.n.gt.0) go to 10
      mode=2
      return
   10 iter=0
      itmax=3*n
c
c		     initialize the arrays index() and x().
c
	  do 20 i=1,n
	  x(i)=zero
   20	  index(i)=i
c
      iz2=n
      iz1=1
      nsetp=0
      npp1=1
c			      ******  main loop begins here  ******
   30 continue
c		   quit if all coefficients are already in the solution.
c			 or if m cols of a have been triangularized.
c
      if (iz1.gt.iz2.or.nsetp.ge.m) go to 350
c
c	  compute components of the dual (negative gradient) vector w().
c
	  do 50 iz=iz1,iz2
	  j=index(iz)
	  sm=zero
	      do 40 l=npp1,m
   40	      sm=sm+a(l,j)*b(l)
   50	  w(j)=sm
c				    find largest positive w(j).
   60 wmax=zero
	  do 70 iz=iz1,iz2
	  j=index(iz)
	  if (w(j).le.wmax) go to 70
	  wmax=w(j)
	  izmax=iz
   70	  continue
c
c	      if wmax .le. 0. go to termination.
c	      this indicates satisfaction of the kuhn-tucker conditions.
c
      if (wmax) 350,350,80
   80 iz=izmax
      j=index(iz)
c
c     the sign of w(j) is ok for j to be moved to set p.
c     begin the transformation and check new diagonal element to avoid
c     near linear dependence.
c
      asave=a(npp1,j)
      call h12 (1,npp1,npp1+1,m,a(1,j),1,up,dummy,1,1,0)
      unorm=zero
      if (nsetp.eq.0) go to 100
	  do 90 l=1,nsetp
   90	  unorm=unorm+a(l,j)**2
  100 unorm=sqrt(unorm)
      if (diff(unorm+abs(a(npp1,j))*factor,unorm)) 130,130,110
c
c     col j is sufficiently independent.  copy b into zz, update zz and
c   > solve for ztest ( = proposed new value for x(j) ).
c
  110	  do 120 l=1,m
  120	  zz(l)=b(l)
      call h12 (2,npp1,npp1+1,m,a(1,j),1,up,zz,1,1,1)
      ztest=zz(npp1)/a(npp1,j)
c
c				      see if ztest is positive
c     reject j as a candidate to be moved from set z to set p.
c     restore a(npp1,j), set w(j)=0., and loop back to test dual
c
      if (ztest) 130,130,140
c
c     coeffs again.
c
  130 a(npp1,j)=asave
      w(j)=zero
      go to 60
c
c     the index  j=index(iz)  has been selected to be moved from
c     set z to set p.	 update b,  update indices,  apply householder
c     transformations to cols in new set z,  zero subdiagonal elts in
c     col j,  set w(j)=0.
c
  140	  do 150 l=1,m
  150	  b(l)=zz(l)
c
      index(iz)=index(iz1)
      index(iz1)=j
      iz1=iz1+1
      nsetp=npp1
      npp1=npp1+1
c
      if (iz1.gt.iz2) go to 170
	  do 160 jz=iz1,iz2
	  jj=index(jz)
  160	  call h12 (2,nsetp,npp1,m,a(1,j),1,up,a(1,jj),1,mda,1)
  170 continue
c
      if (nsetp.eq.m) go to 190
	  do 180 l=npp1,m
  180	  a(l,j)=zero
  190 continue
c
      w(j)=zero
c				 solve the triangular system.
c				 store the solution temporarily in zz().
      assign 200 to next
      go to 400
  200 continue
c
c			******	secondary loop begins here ******
c
c			   iteration counter.
c
  210 iter=iter+1
      if (iter.le.itmax) go to 220
      mode=3
      write (6,440)
      go to 350
  220 continue
c
c		     see if all new constrained coeffs are feasible.
c				   if not compute alpha.
c
      alpha=two
	  do 240 ip=1,nsetp
	  l=index(ip)
	  if (zz(ip)) 230,230,240
c
  230	  t=-x(l)/(zz(ip)-x(l))
	  if (alpha.le.t) go to 240
	  alpha=t
	  jj=ip
  240	  continue
c
c	   if all new constrained coeffs are feasible then alpha will
c	   still = 2.	 if so exit from secondary loop to main loop.
c
      if (alpha.eq.two) go to 330
c
c	   otherwise use alpha which will be between 0. and 1. to
c	   interpolate between the old x and the new zz.
c
	  do 250 ip=1,nsetp
	  l=index(ip)
  250	  x(l)=x(l)+alpha*(zz(ip)-x(l))
c
c	 modify a and b and the index arrays to move coefficient i
c	 from set p to set z.
c
      i=index(jj)
  260 x(i)=zero
c
      if (jj.eq.nsetp) go to 290
      jj=jj+1
	  do 280 j=jj,nsetp
	  ii=index(j)
	  index(j-1)=ii
	  call g1 (a(j-1,ii),a(j,ii),cc,ss,a(j-1,ii))
	  a(j,ii)=zero
	      do 270 l=1,n
	      if (l.ne.ii) call g2 (cc,ss,a(j-1,l),a(j,l))
  270	      continue
  280	  call g2 (cc,ss,b(j-1),b(j))
  290 npp1=nsetp
      nsetp=nsetp-1
      iz1=iz1-1
      index(iz1)=i
c
c	 see if the remaining coeffs in set p are feasible.  they should
c	 be because of the way alpha was determined.
c	 if any are infeasible it is due to round-off error.  any
c	 that are nonpositive will be set to zero
c	 and moved from set p to set z.
c
	  do 300 jj=1,nsetp
	  i=index(jj)
	  if (x(i)) 260,260,300
  300	  continue
c
c	  copy b( ) into zz( ).  then solve again and loop back.
c

	  do 310 i=1,m
  310	  zz(i)=b(i)
      assign 320 to next
      go to 400
  320 continue
      go to 210
c		       ******  end of secondary loop  ******
c
  330	  do 340 ip=1,nsetp
	  i=index(ip)
  340	  x(i)=zz(ip)
c	 all new coeffs are positive.  loop back to beginning.
      go to 30
c
c			 ******  end of main loop  ******
c
c			 come to here for termination.
c		      compute the norm of the final residual vector.
c
  350 sm=zero
      if (npp1.gt.m) go to 370
	  do 360 i=npp1,m
  360	  sm=sm+b(i)**2
      go to 390
  370	  do 380 j=1,n
  380	  w(j)=zero
  390 rnorm=sqrt(sm)
      return
c
c     the following block of code is used as an internal subroutine
c     to solve the triangular system, putting the solution in zz().
c
  400	  do 430 l=1,nsetp
	  ip=nsetp+1-l
	  if (l.eq.1) go to 420
	      do 410 ii=1,ip
  410	      zz(ii)=zz(ii)-a(ii,jj)*zz(ip+1)
  420	  jj=index(ip)
  430	  zz(ip)=zz(ip)/a(ip,jj)
      go to next, (200,320)
  440 format (35h0 nnls quitting on iteration count.)
      end
