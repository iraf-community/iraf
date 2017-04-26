      subroutine bchslv ( w, nbands, nrow, b )
c  from  * a practical guide to splines *  by c. de boor
c  solves the linear system	c*x = b   of order  n r o w  for  x
c  provided  w	contains the cholesky factorization for the banded (sym-
c  metric) positive definite matrix  c	as constructed in the subroutine
c    b c h f a c  (quo vide).
c
c******  i n p u t  ******
c  nrow.....is the order of the matrix	c .
c  nbands.....indicates the bandwidth of  c .
c  w.....contains the cholesky factorization for  c , as output from
c	 subroutine bchfac  (quo vide).
c  b.....the vector of length  n r o w	containing the right side.
c
c******  o u t p u t  ******
c  b.....the vector of length  n r o w	containing the solution.
c
c******  m e t h o d  ******
c  with the factorization  c = l*d*l-transpose	available, where  l  is
c  unit lower triangular and  d  is diagonal, the triangular system
c  l*y = b  is solved for  y (forward substitution), y is stored in  b,
c  the vector  d**(-1)*y is computed and stored in  b, then the triang-
c  ular system	l-transpose*x = d**(-1)*y is solved for  x (backsubstit-
c  ution).
      integer nbands,nrow,   j,jmax,n,nbndm1
      real w(nbands,nrow),b(nrow)
      if (nrow .gt. 1)			go to 21
      b(1) = b(1)*w(1,1)
					return
c
c		  forward substitution. solve l*y = b for y, store in b.
   21 nbndm1 = nbands - 1
      do 30 n=1,nrow
	 jmax = min0(nbndm1,nrow-n)
	 if (jmax .lt. 1)		go to 30
	 do 25 j=1,jmax
   25	    b(j+n) = b(j+n) - w(j+1,n)*b(n)
   30	 continue
c
c     backsubstitution. solve l-transp.x = d**(-1)*y  for x, store in b.
      n = nrow
   31	 b(n) = b(n)*w(1,n)
	 jmax = min0(nbndm1,nrow-n)
	 if (jmax .lt. 1)		go to 40
	 do 35 j=1,jmax
   35	    b(n) = b(n) - w(j+1,n)*b(j+n)
   40	 n = n-1
      if (n.gt.0)			go to 31
					return
      end
