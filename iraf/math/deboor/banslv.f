      subroutine banslv ( w, nroww, nrow, nbandl, nbandu, b )
c  from  * a practical guide to splines *  by c. de boor
c  companion routine to  banfac . it returns the solution  x  of the
c  linear system  a*x = b  in place of	b , given the lu-factorization
c  for	a  in the workarray  w .
c
c******  i n p u t  ******
c  w, nroww,nrow,nbandl,nbandu.....describe the lu-factorization of a
c	 banded matrix	a  of roder  nrow  as constructed in  banfac .
c	 for details, see  banfac .
c  b.....right side of the system to be solved .
c
c******  o u t p u t  ******
c  b.....contains the solution	x , of order  nrow .
c
c******  m e t h o d  ******
c     (with  a = l*u, as stored in  w,) the unit lower triangular system
c  l(u*x) = b  is solved for  y = u*x, and  y  stored in  b . then the
c  upper triangular system  u*x = y  is solved for  x  . the calcul-
c  ations are so arranged that the innermost loops stay within columns.
c
      integer nbandl,nbandu,nrow,nroww,   i,j,jmax,middle,nrowm1
      real w(nroww,nrow),b(nrow)
      middle = nbandu + 1
      if (nrow .eq. 1)			go to 49
      nrowm1 = nrow - 1
      if (nbandl .eq. 0)		go to 30
c				  forward pass
c	     for i=1,2,...,nrow-1, subtract  right side(i)*(i-th column
c	     of  l )  from right side  (below i-th row) .
      do 21 i=1,nrowm1
	 jmax = min0(nbandl, nrow-i)
	 do 21 j=1,jmax
   21	    b(i+j) = b(i+j) - b(i)*w(middle+j,i)
c				  backward pass
c	     for i=nrow,nrow-1,...,1, divide right side(i) by i-th diag-
c	     onal entry of  u, then subtract  right side(i)*(i-th column
c	     of  u)  from right side  (above i-th row).
   30 if (nbandu .gt. 0)		go to 40
c				 a  is lower triangular .
      do 31 i=1,nrow
   31	 b(i) = b(i)/w(1,i)
					return
   40 i = nrow
   41	 b(i) = b(i)/w(middle,i)
	 jmax = min0(nbandu,i-1)
	 do 45 j=1,jmax
   45	    b(i-j) = b(i-j) - b(i)*w(middle-j,i)
	 i = i - 1
	 if (i .gt. 1)			go to 41
   49 b(1) = b(1)/w(middle,1)
					return
      end
