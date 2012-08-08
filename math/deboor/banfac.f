      subroutine banfac ( w, nroww, nrow, nbandl, nbandu, iflag )
c  from  * a practical guide to splines *  by c. de boor
c  returns in  w  the lu-factorization (without pivoting) of the banded
c  matrix  a  of order	nrow  with  (nbandl + 1 + nbandu) bands or diag-
c  onals in the work array  w .
c
c******  i n p u t  ******
c  w.....work array of size  (nroww,nrow)  containing the interesting
c	 part of a banded matrix  a , with the diagonals or bands of  a
c	 stored in the rows of	w , while columns of  a  correspond to
c	 columns of  w . this is the storage mode used in  linpack  and
c	 results in efficient innermost loops.
c	    explicitly,  a  has  nbandl  bands below the diagonal
c			     +	   1	 (main) diagonal
c			     +	 nbandu  bands above the diagonal
c	 and thus, with    middle = nbandu + 1,
c	   a(i+j,j)  is in  w(i+middle,j)  for i=-nbandu,...,nbandl
c					       j=1,...,nrow .
c	 for example, the interesting entries of a (1,2)-banded matrix
c	 of order  9  would appear in the first  1+1+2 = 4  rows of  w
c	 as follows.
c			   13 24 35 46 57 68 79
c			12 23 34 45 56 67 78 89
c		     11 22 33 44 55 66 77 88 99
c		     21 32 43 54 65 76 87 98
c
c	 all other entries of  w  not identified in this way with an en-
c	 try of  a  are never referenced .
c  nroww.....row dimension of the work array  w .
c	 must be  .ge.	nbandl + 1 + nbandu  .
c  nbandl.....number of bands of  a  below the main diagonal
c  nbandu.....number of bands of  a  above the main diagonal .
c
c******  o u t p u t  ******
c  iflag.....integer indicating success( = 1) or failure ( = 2) .
c     if  iflag = 1, then
c  w.....contains the lu-factorization of  a  into a unit lower triangu-
c	 lar matrix  l	and an upper triangular matrix	u (both banded)
c	 and stored in customary fashion over the corresponding entries
c	 of  a . this makes it possible to solve any particular linear
c	 system  a*x = b  for  x  by a
c	       call banslv ( w, nroww, nrow, nbandl, nbandu, b )
c	 with the solution x  contained in  b  on return .
c     if  iflag = 2, then
c	 one of  nrow-1, nbandl,nbandu failed to be nonnegative, or else
c	 one of the potential pivots was found to be zero indicating
c	 that  a  does not have an lu-factorization. this implies that
c	 a  is singular in case it is totally positive .
c
c******  m e t h o d  ******
c     gauss elimination  w i t h o u t	pivoting is used. the routine is
c  intended for use with matrices  a  which do not require row inter-
c  changes during factorization, especially for the  t o t a l l y
c  p o s i t i v e  matrices which occur in spline calculations.
c     the routine should not be used for an arbitrary banded matrix.
c
      integer iflag,nbandl,nbandu,nrow,nroww,	i,ipk,j,jmax,k,kmax
     *					      ,middle,midmk,nrowm1
      real w(nroww,nrow),   factor,pivot
c
      iflag = 1
      middle = nbandu + 1
c			  w(middle,.) contains the main diagonal of  a .
      nrowm1 = nrow - 1
      if (nrowm1)			999,900,1
    1 if (nbandl .gt. 0)		go to 10
c		 a is upper triangular. check that diagonal is nonzero .
      do 5 i=1,nrowm1
	 if (w(middle,i) .eq. 0.)	go to 999
    5	 continue
					go to 900
   10 if (nbandu .gt. 0)		go to 20
c	       a is lower triangular. check that diagonal is nonzero and
c		  divide each column by its diagonal .
      do 15 i=1,nrowm1
	 pivot = w(middle,i)
	 if(pivot .eq. 0.)		go to 999
	 jmax = min0(nbandl, nrow - i)
	 do 15 j=1,jmax
   15	    w(middle+j,i) = w(middle+j,i)/pivot
					return
c
c	 a  is not just a triangular matrix. construct lu factorization
   20 do 50 i=1,nrowm1
c				   w(middle,i)	is pivot for i-th step .
	 pivot = w(middle,i)
	 if (pivot .eq. 0.)		go to 999
c		  jmax	is the number of (nonzero) entries in column  i
c		      below the diagonal .
	 jmax = min0(nbandl,nrow - i)
c	       divide each entry in column  i  below diagonal by pivot .
	 do 32 j=1,jmax
   32	    w(middle+j,i) = w(middle+j,i)/pivot
c		  kmax	is the number of (nonzero) entries in row  i  to
c		      the right of the diagonal .
	 kmax = min0(nbandu,nrow - i)
c		   subtract  a(i,i+k)*(i-th column) from (i+k)-th column
c		   (below row  i ) .
	 do 40 k=1,kmax
	    ipk = i + k
	    midmk = middle - k
	    factor = w(midmk,ipk)
	    do 40 j=1,jmax
   40	       w(midmk+j,ipk) = w(midmk+j,ipk) - w(middle+j,i)*factor
   50	 continue
c					check the last diagonal entry .
  900 if (w(middle,nrow) .ne. 0.)	return
  999 iflag = 2
					return
      end
