      subroutine spli2d ( tau, gtau, t, n, k, m, work, q, bcoef, iflag )
c  from  * a practical guide to splines *  by c. de boor
calls bsplvb, banfac/slv
c  this is an extended version of  splint , for the use in tensor prod-
c  uct interpolation.
c
c   spli2d  produces the b-spline coeff.s  bcoef(j,.)  of the spline of
c   order  k  with knots  t (i), i=1,..., n + k , which takes on the
c   value  gtau (i,j)  at  tau (i), i=1,..., n , j=1,..., m .
c
c******  i n p u t  ******
c  tau.....array of length  n , containing data point abscissae.
c    a s s u m p t i o n . . .	tau  is strictly increasing
c  gtau(.,j)..corresponding array of length  n , containing data point
c	 ordinates, j=1,...,m
c  t.....knot sequence, of length  n+k
c  n.....number of data points and dimension of spline space  s(k,t)
c  k.....order of spline
c  m.....number of data sets
c
c******  w o r k   a r e a  ******
c  work  a vector of length  n
c
c******  o u t p u t  ******
c  q.....array of size	(2*k-1)*n , containing the triangular factoriz-

c	 ation of the coefficient matrix of the linear system for the b-
c	 coefficients of the spline interpolant.
c	    the b-coeffs for the interpolant of an additional data set
c	 (tau(i),htau(i)), i=1,...,n  with the same data abscissae can
c	 be obtained without going through all the calculations in this

c	 routine, simply by loading  htau  into  bcoef	and then execut-
c	 ing the    call banslv ( q, 2*k-1, n, k-1, k-1, bcoef )
c  bcoef.....the b-coefficients of the interpolant, of length  n
c  iflag.....an integer indicating success (= 1)  or failure (= 2)
c	 the linear system to be solved is (theoretically) invertible if
c	 and only if
c	       t(i) .lt. tau(i) .lt. tau(i+k),	  all i.
c	 violation of this condition is certain to lead to  iflag = 2 .

c
c******  m e t h o d  ******
c     the i-th equation of the linear system  a*bcoef = b  for the b-co-
c  effs of the interpolant enforces interpolation at  tau(i), i=1,...,n.
c  hence,  b(i) = gtau(i), all i, and  a  is a band matrix with  2k-1
c   bands (if it is invertible).
c     the matrix  a  is generated row by row and stored, diagonal by di-
c  agonal, in the  r o w s  of the array  q , with the main diagonal go-
c  ing into row  k .  see comments in the program below.
c     the banded system is then solved by a call to  banfac (which con-

c  structs the triangular factorization for  a	and stores it again in
c   q ), followed by a call to	banslv (which then obtains the solution

c   bcoef  by substitution).
c     banfac  does no pivoting, since the total positivity of the matrix
c  a  makes this unnecessary.
c
      integer iflag,k,m,n,   i,ilp1mx,j,jj,km1,kpkm2,left,lenq,np1
      real bcoef(m,n),gtau(n,m),q(1),t(1),tau(n),work(n),   taui
c     dimension q(2*k-1,n), t(n+k)
current fortran standard makes it impossible to specify precisely the
c  dimension of  q  and  t  without the introduction of otherwise super-
c  fluous additional arguments.
      np1 = n + 1
      km1 = k - 1
      kpkm2 = 2*km1
      left = k
c		 zero out all entries of q
      lenq = n*(k+km1)
      do 5 i=1,lenq
    5	 q(i) = 0.
c
c  ***	 loop over i to construct the  n  interpolation equations
      do 30 i=1,n
	 taui = tau(i)
	 ilp1mx = min0(i+k,np1)
c	 *** find  left  in the closed interval (i,i+k-1) such that
c		 t(left) .le. tau(i) .lt. t(left+1)
c	 matrix is singular if this is not possible
	 left = max0(left,i)
	 if (taui .lt. t(left)) 	go to 998
   15	    if (taui .lt. t(left+1))	go to 16
	    left = left + 1
	    if (left .lt. ilp1mx)	go to 15
	 left = left - 1
	 if (taui .gt. t(left+1))	go to 998
c	 *** the i-th equation enforces interpolation at taui, hence
c	 a(i,j) = b(j,k,t)(taui), all j. only the  k  entries with  j =

c	 left-k+1,...,left actually might be nonzero. these  k	numbers

c	 are returned, in  work  (used for temp.storage here), by the
c	 following
   16	 call bsplvb ( t, k, 1, taui, left, work )
c	 we therefore want  work(j) = b(left -k+j)(taui) to go into
c	 a(i,left-k+j), i.e., into  q(i-(left+j)+2*k,(left+j)-k) since
c	 a(i+j,j)  is to go into  q(i+k,j), all i,j,  if we consider  q

c	 as a two-dim. array , with  2*k-1  rows (see comments in
c	 banfac). in the present program, we treat  q  as an equivalent

c	 one-dimensional array (because of fortran restrictions on
c	 dimension statements) . we therefore want  work(j) to go into
c	 entry
c	     i -(left+j) + 2*k + ((left+j) - k-1)*(2*k-1)
c		    =  i-left+1 + (left -k)*(2*k-1) + (2*k-2)*j
c	 of  q .
	 jj = i-left+1 + (left-k)*(k+km1)
	 do 30 j=1,k
	    jj = jj+kpkm2
   30	    q(jj) = work(j)
c
c     ***obtain factorization of  a  , stored again in	q.
      call banfac ( q, k+km1, n, km1, km1, iflag )
					go to (40,999), iflag
c     *** solve  a*bcoef = gtau  by backsubstitution
   40 do 50 j=1,m
	 do 41 i=1,n
   41	    work(i) = gtau(i,j)
	 call banslv ( q, k+km1, n, km1, km1, work )
	 do 50 i=1,n
   50	    bcoef(j,i) = work(i)
					return
  998 iflag = 2
  999 print 699
  699 format(41h linear system in  splint  not invertible)
					return
      end
