      subroutine hfti (a,mda,m,n,b,mdb,nb,tau,krank,rnorm,h,g,ip)
c     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 jun 12
c     to appear in 'solving least squares problems', prentice-hall, 1974
c	   solve least squares problem using algorithm, hfti.
c
      dimension a(mda,n),b(mdb,nb),h(n),g(n),rnorm(nb)
      integer ip(n)
      double precision sm,dzero
      szero=0.
      dzero=0.d0
      factor=0.001
c
      k=0
      ldiag=min0(m,n)
      if (ldiag.le.0) go to 270
	  do 80 j=1,ldiag
	  if (j.eq.1) go to 20
c
c     update squared column lengths and find lmax
c    ..
	  lmax=j
	      do 10 l=j,n
	      h(l)=h(l)-a(j-1,l)**2
	      if (h(l).gt.h(lmax)) lmax=l
   10	      continue
	  if(diff(hmax+factor*h(lmax),hmax)) 20,20,50
c
c     compute squared column lengths and find lmax
c    ..
   20	  lmax=j
	      do 40 l=j,n
	      h(l)=0.
		  do 30 i=j,m
   30		  h(l)=h(l)+a(i,l)**2
	      if (h(l).gt.h(lmax)) lmax=l
   40	      continue
	  hmax=h(lmax)
c    ..
c     lmax has been determined
c
c     do column interchanges if needed.
c    ..
   50	  continue
	  ip(j)=lmax
	  if (ip(j).eq.j) go to 70
	      do 60 i=1,m
	      tmp=a(i,j)
	      a(i,j)=a(i,lmax)
   60	      a(i,lmax)=tmp
	  h(lmax)=h(j)
c
c     compute the j-th transformation and apply it to a and b.
c    ..
   70	  call h12 (1,j,j+1,m,a(1,j),1,h(j),a(1,j+1),1,mda,n-j)
   80	  call h12 (2,j,j+1,m,a(1,j),1,h(j),b,1,mdb,nb)
c
c     determine the pseudorank, k, using the tolerance, tau.
c    ..
	  do 90 j=1,ldiag
	  if (abs(a(j,j)).le.tau) go to 100
   90	  continue
      k=ldiag
      go to 110
  100 k=j-1
  110 kp1=k+1
c
c     compute the norms of the residual vectors.
c
      if (nb.le.0) go to 140
	  do 130 jb=1,nb
	  tmp=szero
	  if (kp1.gt.m) go to 130
	      do 120 i=kp1,m
  120	      tmp=tmp+b(i,jb)**2
  130	  rnorm(jb)=sqrt(tmp)
  140 continue
c					    special for pseudorank = 0
      if (k.gt.0) go to 160
      if (nb.le.0) go to 270
	  do 150 jb=1,nb
	      do 150 i=1,n
  150	      b(i,jb)=szero
      go to 270
c
c     if the pseudorank is less than n compute householder
c     decomposition of first k rows.
c    ..
  160 if (k.eq.n) go to 180
	  do 170 ii=1,k
	  i=kp1-ii
  170	  call h12 (1,i,kp1,n,a(i,1),mda,g(i),a,mda,1,i-1)
  180 continue
c
c
      if (nb.le.0) go to 270
	  do 260 jb=1,nb
c
c     solve the k by k triangular system.
c    ..
	      do 210 l=1,k
	      sm=dzero
	      i=kp1-l
	      if (i.eq.k) go to 200
	      ip1=i+1
		  do 190 j=ip1,k
  190		  sm=sm+a(i,j)*dble(b(j,jb))
  200	      sm1=sm
  210	      b(i,jb)=(b(i,jb)-sm1)/a(i,i)
c
c     complete computation of solution vector.
c    ..
	  if (k.eq.n) go to 240
	      do 220 j=kp1,n
  220	      b(j,jb)=szero
	      do 230 i=1,k
  230	      call h12 (2,i,kp1,n,a(i,1),mda,g(i),b(1,jb),1,mdb,1)
c
c      re-order the solution vector to compensate for the
c      column interchanges.
c    ..
  240	      do 250 jj=1,ldiag
	      j=ldiag+1-jj
	      if (ip(j).eq.j) go to 250
	      l=ip(j)
	      tmp=b(l,jb)
	      b(l,jb)=b(j,jb)
	      b(j,jb)=tmp
  250	      continue
  260	  continue
c    ..
c     the solution vectors, x, are now
c     in the first  n  rows of the array b(,).
c
  270 krank=k
      return
      end
