      subroutine bchfac ( w, nbands, nrow, diag )
c  from  * a practical guide to splines *  by c. de boor
constructs cholesky factorization
c		      c  =  l * d * l-transpose
c  with l unit lower triangular and d diagonal, for given matrix c of
c  order  n r o w , in case  c	is (symmetric) positive semidefinite
c  and	b a n d e d , having  n b a n d s  diagonals at and below the
c  main diagonal.
c
c******  i n p u t  ******
c  nrow.....is the order of the matrix	c .
c  nbands.....indicates its bandwidth, i.e.,
c	   c(i,j) = 0 for abs(i-j) .gt. nbands .
c  w.....workarray of size (nbands,nrow)  containing the  nbands  diago-
c	 nals in its rows, with the main diagonal in row  1 . precisely,
c	 w(i,j)  contains  c(i+j-1,j), i=1,...,nbands, j=1,...,nrow.
c	   for example, the interesting entries of a seven diagonal sym-
c	 metric matrix	c  of order  9	would be stored in  w  as
c
c			11 22 33 44 55 66 77 88 99
c			21 32 43 54 65 76 87 98
c			31 42 53 64 75 86 97
c			41 52 63 74 85 96
c
c	 all other entries of  w  not identified in this way with an en-
c	 try of  c  are never referenced .
c  diag.....is a work array of length  nrow .
c
c******  o u t p u t  ******
c  w.....contains the cholesky factorization  c = l*d*l-transp, with
c	 w(1,i) containing  1/d(i,i)
c	 and  w(i,j)  containing  l(i-1+j,j), i=2,...,nbands.
c
c******  m e t h o d  ******
c   gauss elimination, adapted to the symmetry and bandedness of  c , is
c   used .
c     near zero pivots are handled in a special way. the diagonal ele-
c  ment c(n,n) = w(1,n) is saved initially in  diag(n), all n. at the n-
c  th elimination step, the current pivot element, viz.  w(1,n), is com-
c  pared with its original value, diag(n). if, as the result of prior
c  elimination steps, this element has been reduced by about a word
c  length, (i.e., if w(1,n)+diag(n) .le. diag(n)), then the pivot is de-
c  clared to be zero, and the entire n-th row is declared to be linearly
c  dependent on the preceding rows. this has the effect of producing
c   x(n) = 0  when solving  c*x = b  for  x, regardless of  b. justific-
c  ation for this is as follows. in contemplated applications of this
c  program, the given equations are the normal equations for some least-
c  squares approximation problem, diag(n) = c(n,n) gives the norm-square
c  of the n-th basis function, and, at this point,  w(1,n)  contains the
c  norm-square of the error in the least-squares approximation to the n-
c  th basis function by linear combinations of the first n-1 . having
c  w(1,n)+diag(n) .le. diag(n) signifies that the n-th function is lin-
c  early dependent to machine accuracy on the first n-1 functions, there
c  fore can safely be left out from the basis of approximating functions
c     the solution of a linear system
c			c*x = b
c   is effected by the succession of the following  t w o  calls:
c     call bchfac ( w, nbands, nrow, diag )	  , to get factorization
c     call bchslv ( w, nbands, nrow, b, x )	       , to solve for x.
c
      integer nbands,nrow,   i,imax,j,jmax,n
      real w(nbands,nrow),diag(nrow),	ratio
      if (nrow .gt. 1)			go to 9
      if (w(1,1) .gt. 0.) w(1,1) = 1./w(1,1)
					return
c					 store diagonal of  c  in  diag.
    9 do 10 n=1,nrow
   10	 diag(n) = w(1,n)
c							 factorization .
      do 20 n=1,nrow
	 if (w(1,n)+diag(n) .gt. diag(n)) go to 15
	 do 14 j=1,nbands
   14	    w(j,n) = 0.
					go to 20
   15	 w(1,n) = 1./w(1,n)
	 imax = min0(nbands-1,nrow - n)
	 if (imax .lt. 1)		go to 20
	 jmax = imax
	 do 18 i=1,imax
	    ratio = w(i+1,n)*w(1,n)
	    do 17 j=1,jmax
   17	       w(j,n+i) = w(j,n+i) - w(j+i,n)*ratio
	    jmax = jmax - 1
   18	    w(i+1,n) = ratio
   20	 continue
					return
      end
