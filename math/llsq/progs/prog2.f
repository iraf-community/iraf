c     prog2
c     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 jun 12
c     to appear in 'solving least squares problems', prentice-hall, 1974
c	 demonstrate algorithm	hfti  for solving least squares problems
c     and algorithm  cov  for computing the associated unscaled
c     covariance matrix.
c
      dimension a(8,8),h(8),b(8),g(8)
      real gen,anoise
      integer ip(8)
      double precision sm
      data mda /8/
c
	  do 180 noise=1,2
	  anorm=500.
	  anoise=0.
	  tau=.5
	  if (noise.eq.1) go to 10
	  anoise=1.e-4
	  tau=anorm*anoise*10.
   10	  continue
c     initialize the data generation function
c    ..
	  dummy=gen(-1.)
	  write (6,230)
	  write (6,240) anoise,anorm,tau
c
	      do 180 mn1=1,6,5
	      mn2=mn1+2
		  do 180 m=mn1,mn2
		      do 180 n=mn1,mn2
		      write (6,250) m,n
c     generate data
c    ..
			  do 20 i=1,m
			      do 20 j=1,n
   20			      a(i,j)=gen(anoise)
			  do 30 i=1,m
   30			  b(i)=gen(anoise)
c
c     ****** call hfti	 ******
c
		      call hfti(a,mda,m,n,b,1,1,tau,krank,srsmsq,h,g,ip)
c
c
		      write (6,260) krank
		      write (6,200) (i,b(i),i=1,n)
		      write (6,190) srsmsq
		      if (krank.lt.n) go to 180
c     ******  algorithm cov bigins here  ******
c    ..
			  do 40 j=1,n
   40			  a(j,j)=1./a(j,j)
		      if (n.eq.1) go to 70
		      nm1=n-1
			  do 60 i=1,nm1
			  ip1=i+1
			      do 60 j=ip1,n
			      jm1=j-1
			      sm=0.d0
				  do 50 l=i,jm1
   50				  sm=sm+a(i,l)*dble(a(l,j))
   60			      a(i,j)=-sm*a(j,j)
c    ..
c     the upper triangle of a has been inverted
c     upon itself.
   70			  do 90 i=1,n
			      do 90 j=i,n
			      sm=0.d0
				  do 80 l=j,n
   80				  sm=sm+a(i,l)*dble(a(j,l))
   90			      a(i,j)=sm
		      if (n.lt.2) go to 160
			  do 150 ii=2,n
			  i=n+1-ii
			  if (ip(i).eq.i) go to 150
			  k=ip(i)
			  tmp=a(i,i)
			  a(i,i)=a(k,k)
			  a(k,k)=tmp
			  if (i.eq.1) go to 110
			      do 100 l=2,i
			      tmp=a(l-1,i)
			      a(l-1,i)=a(l-1,k)
  100			      a(l-1,k)=tmp
  110			  ip1=i+1
			  km1=k-1
			  if (ip1.gt.km1) go to 130
			      do 120 l=ip1,km1
			      tmp=a(i,l)
			      a(i,l)=a(l,k)
  120			      a(l,k)=tmp
  130			  if (k.eq.n) go to 150
			  kp1=k+1
			      do 140 l=kp1,n
			      tmp=a(i,l)
			      a(i,l)=a(k,l)
  140			      a(k,l)=tmp
  150			  continue
  160		      continue
c    ..
c     covariance has been computed and repermuted.
c     the upper triangular part of the
c     symmetric matrix (a**t*a)**(-1) has
c     replaced the upper triangular part of
c     the a array.
		      write (6,210)
			  do 170 i=1,n
  170			  write (6,220) (i,j,a(i,j),j=i,n)
  180		      continue
      stop
  190 format (1h0,8x,17hresidual length =,e12.4)
  200 format (1h0,8x,34hestimated parameters,  x=a**(+)*b,,22h computed
     1by 'hfti'   //(9x,i6,e16.8,i6,e16.8,i6,e16.8,i6,e16.8,i6,e16.8))
  210 format (1h0,8x,31hcovariance matrix (unscaled) of,22h estimated pa
     1rameters.,19h computed by 'cov'./1x)
  220 format (9x,2i3,e16.8,2i3,e16.8,2i3,e16.8,2i3,e16.8,2i3,e16.8)
  230 format (52h1 prog2.     this program demonstates the algorithms,16
     1h hfti  and  cov.)
  240 format (1h0,54hthe relative noise level of the generated data will
     1 be,e16.4/33h0the matrix norm is approximately,e12.4/43h0the absol
     2ute pseudorank tolerance, tau, is,e12.4)
  250 format (1h0////9h0   m   n/1x,2i4)
  260 format (1h0,8x,12hpseudorank =,i4)
      end
