c     prog1
c     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 jun 12
c     to appear in 'solving least squares problems', prentice-hall, 1974
c	 demonstrate algorithms hft and hs1 for solving least squares
c     problems and algorithm cov for omputing the associated covariance
c     matrices.
c
      dimension a(8,8),h(8),b(8)
      real gen,anoise
      double precision sm
      data mda/8/
c
	   do 180 noise=1,2
	   anoise=0.
	   if (noise.eq.2) anoise=1.e-4
	   write (6,230)
	   write (6,240) anoise
c     initialize the data generation function
c    ..
	   dummy=gen(-1.)
		do 180 mn1=1,6,5
		mn2=mn1+2
		     do 180 m=mn1,mn2
		     do 180 n=mn1,mn2
		     np1=n+1
		     write (6,250) m,n
c     generate data
c    ..
			  do 10 i=1,m
			       do 10 j=1,n
   10			       a(i,j)=gen(anoise)
			  do 20 i=1,m
   20			  b(i)=gen(anoise)
		     if(m .lt. n) go to 180
c
c     ******  begin algorithm hft  ******
c    ..
			  do 30 j=1,n
   30		   call h12 (1,j,j+1,m,a(1,j),1,h(j),a(1,j+1),1,mda,n-j)
c    ..
c     the algorithm 'hft' is completed.
c
c     ******  begin algorithm hs1  ******
c     apply the transformations  q(n)...q(1)=q to b
c     replacing the previous contents of the array, b .
c    ..
			  do 40 j=1,n
   40			      call h12 (2,j,j+1,m,a(1,j),1,h(j),b,1,1,1)
c     solve the triangular system for the solution x.
c     store x in the array, b .
c    ..
			  do 80 k=1,n
			  i=np1-k
			  sm=0.d0
			  if (i.eq.n) go to 60
			  ip1=i+1
			       do 50 j=ip1,n
   50			       sm=sm+a(i,j)*dble(b(j))
   60			  if (a(i,i)) 80,70,80
   70			  write (6,260)
			  go to 180
   80			  b(i)=(b(i)-sm)/a(i,i)
c      compute length of residual vector.
c     ..
		     srsmsq=0.
		     if (n.eq.m) go to 100
		     mmn=m-n
			  do 90 j=1,mmn
			  npj=n+j
   90			  srsmsq=srsmsq+b(npj)**2
		     srsmsq=sqrt(srsmsq)
c     ******  begin algorithm  cov  ******
c     compute unscaled covariance matrix   ((a**t)*a)**(-1)
c    ..
  100			  do 110 j=1,n
  110			  a(j,j)=1./a(j,j)
		     if (n.eq.1) go to 140
		     nm1=n-1
			  do 130 i=1,nm1
			  ip1=i+1
			       do 130 j=ip1,n
			       jm1=j-1
			       sm=0.d0
				    do 120 l=i,jm1
  120				    sm=sm+a(i,l)*dble(a(l,j))
  130			       a(i,j)=-sm*a(j,j)
c    ..
c     the upper triangle of a has been inverted
c     upon itself.
  140			  do 160 i=1,n
			       do 160 j=i,n
			       sm=0.d0
				    do 150 l=j,n
  150				    sm=sm+a(i,l)*dble(a(j,l))
  160			       a(i,j)=sm
c    ..
c     the upper triangular part of the
c     symmetric matrix (a**t*a)**(-1) has
c     replaced the upper triangular part of
c     the a array.
		     write (6,200) (i,b(i),i=1,n)
		     write (6,190) srsmsq
		     write (6,210)
			  do 170 i=1,n
  170			  write (6,220) (i,j,a(i,j),j=i,n)
  180		     continue
      stop
  190 format (1h0,8x,17hresidual length =,e12.4)
  200 format (1h0,8x,34hestimated parameters,  x=a**(+)*b,,22h computed
     1by 'hft,hs1'//(9x,i6,e16.8,i6,e16.8,i6,e16.8,i6,e16.8,i6,e16.8))
  210 format (1h0,8x,31hcovariance matrix (unscaled) of,22h estimated pa
     1rameters.,19h computed by 'cov'./1x)
  220 format (9x,2i3,e16.8,2i3,e16.8,2i3,e16.8,2i3,e16.8,2i3,e16.8)
  230 format (52h1 prog1.    this program demonstrates the algorithms,19
     1h hft, hs1, and cov.//40h caution.. 'prog1' does no checking for ,
     252hnear rank deficient matrices.  results in such cases,20h may be
     3 meaningless.,/34h such cases are handled by 'prog2',11h or 'pro
     4')
  240 format (1h0,54hthe relative noise level of the generated data will
     1 be,e16.4)
  250 format (1h0////9h0   m   n/1x,2i4)
  260 format (1h0,8x,36h******  terminating this case due to,37h a divis
     1or being exactly zero. ******)
      end
