c     prog3
c     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 jun 12
c     to appear in 'solving least squares problems', prentice-hall, 1974
c	   demonstrate the use of subroutine   svdrs  to compute the
c     singular value decomposition of a matrix, a, and solve a least
c     squares problem,	a*x=b.
c
c     the s.v.d.  a= u*(s,0)**t*v**t is
c     computed so that..
c     (1)  u**t*b replaces the m+1 st. col. of	b.
c
c     (2)  u**t   replaces the m by m identity in
c     the first m cols. of  b.
c
c     (3) v replaces the first n rows and cols.
c     of a.
c
c     (4) the diagonal entries of the s matrix
c     replace the first n entries of the array s.
c
c     the array s( ) must be dimensioned at least 3*n .
c
      dimension a(8,8),b(8,9),s(24),x(8),aa(8,8)
      real gen,anoise
      double precision sm
      data mda,mdb/8,8/
c
      do 150 noise=1,2
      anoise = 0.
      rho = 1.e-3
      if(noise .eq. 1) go to 5
      anoise = 1.e-4
      rho = 10. * anoise
    5 continue
      write(6,230)
      write(6,240) anoise,rho
c     initialize data generation function
c    ..
      dummy= gen(-1.)
c
	   do 150 mn1=1,6,5
	   mn2=mn1+2
		do 150 m=mn1,mn2
		do 150 n=mn1,mn2
		write (6,160) m,n
		     do 20 i=1,m
			  do 10 j=1,m
   10			  b(i,j)=0.
		     b(i,i)=1.
			  do 20 j=1,n
			  a(i,j)= gen(anoise)
   20			  aa(i,j)=a(i,j)
		     do 30 i=1,m
   30		     b(i,m+1)= gen(anoise)
c
c     the arrays are now filled in..
c     compute the s.v.d.
c     ******************************************************
		call svdrs (a,mda,m,n,b,mdb,m+1,s)
c     ******************************************************
c
		write (6,170)
		write (6,220) (i,s(i),i=1,n)
		write (6,180)
		write (6,220) (i,b(i,m+1),i=1,m)
c
c     test for disparity of ratio of singular values.
c    ..
		krank=n
		tau=rho*s(1)
		     do 40 i=1,n
		     if (s(i).le.tau) go to 50
   40		     continue
		go to 55
   50		krank=i-1
   55 write(6,250) tau, krank
c     compute solution vector assuming pseudorank is krank
c     ..
   60		     do 70 i=1,krank
   70		     b(i,m+1)=b(i,m+1)/s(i)
		     do 90 i=1,n
		     sm=0.d0
			  do 80 j=1,krank
   80			  sm=sm+a(i,j)*dble(b(j,m+1))
   90		     x(i)=sm
c     compute predicted norm of residual vector.
c     ..
		srsmsq=0.
		if (krank.eq.m) go to 110
		kp1=krank+1
		     do 100 i=kp1,m
  100		     srsmsq=srsmsq+b(i,m+1)**2
		srsmsq=sqrt(srsmsq)
c
  110		continue
		write (6,190)
		write (6,220) (i,x(i),i=1,n)
		write (6,200) srsmsq
c     compute the frobenius norm of  a**t- v*(s,0)*u**t.
c
c     compute  v*s  first.
c
		minmn=min0(m,n)
		     do 120 j=1,minmn
			  do 120 i=1,n
  120			  a(i,j)=a(i,j)*s(j)
		dn=0.
		     do 140 j=1,m
			  do 140 i=1,n
			  sm=0.d0
			       do 130 l=1,minmn
  130			       sm=sm+a(i,l)*dble(b(l,j))
c     computed difference of (i,j) th entry
c     of  a**t-v*(s,0)*u**t.
c     ..
			  t=aa(j,i)-sm
  140			  dn=dn+t**2
		dn=sqrt(dn)/(sqrt(float(n))*s(1))
		write (6,210) dn
  150		continue
      stop
  160 format (1h0////9h0   m   n/1x,2i4)
  170 format (1h0,8x,25hsingular values of matrix)
  180 format (1h0,8x,30htransformed right side, u**t*b)
  190 format (1h0,8x,33hestimated parameters,  x=a**(+)*b,21h computed b
     1y 'svdrs' )
  200 format (1h0,8x,24hresidual vector length =,e12.4)
  210 format (1h0,8x,43hfrobenius norm (a-u*(s,0)**t*v**t)/(sqrt(n),
     *22h*spectral norm of a) =,e12.4)
  220 format (9x,i5,e16.8,i5,e16.8,i5,e16.8,i5,e16.8,i5,e16.8)
  230 format(51h1prog3.     this program demonstrates the algorithm,
     * 9h, svdrs. )
  240 format(55h0the relative noise level of the generated data will be,
     *e16.4/44h0the relative tolerance, rho, for pseudorank,
     *17h determination is,e16.4)
  250 format(1h0,8x,36habsolute pseudorank tolerance, tau =,
     *e12.4,10x,12hpseudorank =,i5)
      end
