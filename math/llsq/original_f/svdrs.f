c     subroutine svdrs (a,mda,mm,nn,b,mdb,nb,s)
c     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1974 mar 1
c     to appear in 'solving least squares problems', prentice-hall, 1974
c	   singular value decomposition also treating right side vector.
c
c     the array s occupies 3*n cells.
c     a occupies m*n cells
c     b occupies m*nb cells.
c
c     special singular value decomposition subroutine.
c     we have the m x n matrix a and the system a*x=b to solve.
c     either m .ge. n  or  m .lt. n is permitted.
c			the singular value decomposition
c     a = u*s*v**(t) is made in such a way that one gets
c	 (1) the matrix v in the first n rows and columns of a.
c	 (2) the diagonal matrix of ordered singular values in
c	     the first n cells of the array s(ip), ip .ge. 3*n.
c	 (3) the matrix product u**(t)*b=g gets placed back in b.
c	 (4) the user must complete the solution and do his own
c	     singular value analysis.
c     *******
c     give special
c     treatment to rows and columns which are entirely zero.  this
c     causes certain zero sing. vals. to appear as exact zeros rather
c     than as about eta times the largest sing. val.   it similarly
c     cleans up the associated columns of u and v.
c     method..
c      1. exchange cols of a to pack nonzero cols to the left.
c	  set n = no. of nonzero cols.
c	  use locations a(1,nn),a(1,nn-1),...,a(1,n+1) to record the
c	  col permutations.
c      2. exchange rows of a to pack nonzero rows to the top.
c	  quit packing if find n nonzero rows.	make same row exchanges
c	  in b.  set m so that all nonzero rows of the permuted a
c	  are in first m rows.	if m .le. n then all m rows are
c	  nonzero.  if m .gt. n then	  the first n rows are known
c	  to be nonzero,and rows n+1 thru m may be zero or nonzero.
c      3. apply original algorithm to the m by n problem.
c      4. move permutation record from a(,) to s(i),i=n+1,...,nn.
c      5. build v up from  n by n  to  nn by nn  by placing ones on
c	  the diagonal and zeros elsewhere.  this is only partly done
c	  explicitly.  it is completed during step 6.
c      6. exchange rows of v to compensate for col exchanges of step 2.
c      7. place zeros in  s(i),i=n+1,nn  to represent zero sing vals.
c
      subroutine svdrs (a,mda,mm,nn,b,mdb,nb,s)
      dimension a(mda,nn),b(mdb,nb),s(nn,3)
      zero=0.
      one=1.
c
c			      begin.. special for zero rows and cols.
c
c			      pack the nonzero cols to the left
c
      n=nn
      if (n.le.0.or.mm.le.0) return
      j=n
   10 continue
	  do 20 i=1,mm
	  if (a(i,j)) 50,20,50
   20	  continue
c
c	  col j  is zero. exchange it with col n.
c
      if (j.eq.n) go to 40
	  do 30 i=1,mm
   30	  a(i,j)=a(i,n)
   40 continue
      a(1,n)=j
      n=n-1
   50 continue
      j=j-1
      if (j.ge.1) go to 10
c			      if n=0 then a is entirely zero and svd
c			      computation can be skipped
      ns=0
      if (n.eq.0) go to 240
c			      pack nonzero rows to the top
c			      quit packing if find n nonzero rows
      i=1
      m=mm
   60 if (i.gt.n.or.i.ge.m) go to 150
      if (a(i,i)) 90,70,90
   70	  do 80 j=1,n
	  if (a(i,j)) 90,80,90
   80	  continue
      go to 100
   90 i=i+1
      go to 60
c			      row i is zero
c			      exchange rows i and m
  100 if(nb.le.0) go to 115
	  do 110 j=1,nb
	  t=b(i,j)
	  b(i,j)=b(m,j)
  110	  b(m,j)=t
  115	  do 120 j=1,n
  120	  a(i,j)=a(m,j)
      if (m.gt.n) go to 140
	  do 130 j=1,n
  130	  a(m,j)=zero
  140 continue
c			      exchange is finished
      m=m-1
      go to 60
c
  150 continue
c			      end.. special for zero rows and columns
c			      begin.. svd algorithm
c     method..
c     (1)     reduce the matrix to upper bidiagonal form with
c     householder transformations.
c	   h(n)...h(1)aq(1)...q(n-2) = (d**t,0)**t
c     where d is upper bidiagonal.
c
c     (2)     apply h(n)...h(1) to b.  here h(n)...h(1)*b replaces b
c     in storage.
c
c     (3)     the matrix product w= q(1)...q(n-2) overwrites the first
c     n rows of a in storage.
c
c     (4)     an svd for d is computed.  here k rotations ri and pi are
c     computed so that
c	   rk...r1*d*p1**(t)...pk**(t) = diag(s1,...,sm)
c     to working accuracy.  the si are nonnegative and nonincreasing.
c     here rk...r1*b overwrites b in storage while
c     a*p1**(t)...pk**(t)  overwrites a in storage.
c
c     (5)     it follows that,with the proper definitions,
c     u**(t)*b overwrites b, while v overwrites the first n row and
c     columns of a.
c
      l=min0(m,n)
c	      the following loop reduces a to upper bidiagonal and
c	      also applies the premultiplying transformations to b.
c
	  do 170 j=1,l
	  if (j.ge.m) go to 160
	  call h12 (1,j,j+1,m,a(1,j),1,t,a(1,j+1),1,mda,n-j)
	  call h12 (2,j,j+1,m,a(1,j),1,t,b,1,mdb,nb)
  160	  if (j.ge.n-1) go to 170
	  call h12 (1,j+1,j+2,n,a(j,1),mda,s(j,3),a(j+1,1),mda,1,m-j)
  170	  continue
c
c     copy the bidiagonal matrix into the array s() for qrbd.
c
      if (n.eq.1) go to 190
	  do 180 j=2,n
	  s(j,1)=a(j,j)
  180	  s(j,2)=a(j-1,j)
  190 s(1,1)=a(1,1)
c
      ns=n
      if (m.ge.n) go to 200
      ns=m+1
      s(ns,1)=zero
      s(ns,2)=a(m,m+1)
  200 continue
c
c     construct the explicit n by n product matrix, w=q1*q2*...*ql*i
c     in the array a().
c
	  do 230 k=1,n
	  i=n+1-k
	  if(i.gt.min0(m,n-2)) go to 210
	  call h12 (2,i+1,i+2,n,a(i,1),mda,s(i,3),a(1,i+1),1,mda,n-i)
  210	      do 220 j=1,n
  220	      a(i,j)=zero
  230	  a(i,i)=one
c
c	   compute the svd of the bidiagonal matrix
c
      call qrbd (ipass,s(1,1),s(1,2),ns,a,mda,n,b,mdb,nb)
c
      go to (240,310), ipass
  240 continue
      if (ns.ge.n) go to 260
      nsp1=ns+1
	  do 250 j=nsp1,n
  250	  s(j,1)=zero
  260 continue
      if (n.eq.nn) return
      np1=n+1
c				   move record of permutations
c				   and store zeros
	  do 280 j=np1,nn
	  s(j,1)=a(1,j)
	      do 270 i=1,n
  270	      a(i,j)=zero
  280	  continue
c			      permute rows and set zero singular values.
	  do 300 k=np1,nn
	  i=s(k,1)
	  s(k,1)=zero
	      do 290 j=1,nn
	      a(k,j)=a(i,j)
  290	      a(i,j)=zero
	  a(i,k)=one
  300	  continue
c			      end.. special for zero rows and columns
      return
  310 write (6,320)
      stop
  320 format (49h convergence failure in qr bidiagonal svd routine)
      end
