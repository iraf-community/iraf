      subroutine bsplpp ( t, bcoef, n, k, scrtch, break, coef, l )
c  from  * a practical guide to splines *  by c. de boor
calls  bsplvb
c
converts the b-representation  t, bcoef, n, k  of some spline into its
c  pp-representation  break, coef, l, k .
c
c******  i n p u t  ******
c  t.....knot sequence, of length  n+k
c  bcoef.....b-spline coefficient sequence, of length  n
c  n.....length of  bcoef  and	dimension of spline space  spline(k,t)
c  k.....order of the spline
c
c  w a r n i n g  . . .  the restriction   k .le. kmax (= 20)	is impo-
c	 sed by the arbitrary dimension statement for  biatx  below, but
c	 is  n o w h e r e   c h e c k e d   for.
c
c******  w o r k   a r e a  ******
c  scrtch......of size	(k,k) , needed to contain bcoeffs of a piece of
c	 the spline and its  k-1  derivatives
c
c******  o u t p u t  ******
c  break.....breakpoint sequence, of length  l+1, contains (in increas-
c	 ing order) the distinct points in the sequence  t(k),...,t(n+1)
c  coef.....array of size (k,n), with  coef(i,j) = (i-1)st derivative of
c	 spline at break(j) from the right
c  l.....number of polynomial pieces which make up the spline in the in-
c	 terval  (t(k), t(n+1))
c
c******  m e t h o d  ******
c     for each breakpoint interval, the  k  relevant b-coeffs of the
c  spline are found and then differenced repeatedly to get the b-coeffs
c  of all the derivatives of the spline on that interval. the spline and
c  its first  k-1  derivatives are then evaluated at the left end point
c  of that interval, using  bsplvb  repeatedly to obtain the values of
c  all b-splines of the appropriate order at that point.
c
c     parameter kmax = 20
      integer k,l,n,   i,j,jp1,kmj,left,lsofar
      real bcoef(n),break(1),coef(k,1),t(1),   scrtch(k,k)
     *					      ,biatx(20),diff,fkmj,sum
c    *					      ,biatx(kmax),diff,fkmj,sum
c     dimension break(l+1),coef(k,l),t(n+k)
current fortran standard makes it impossible to specify the length of
c  break , coef  and  t  precisely without the introduction of otherwise
c  superfluous additional arguments.
      lsofar = 0
      break(1) = t(k)
      do 50 left=k,n
c				 find the next nontrivial knot interval.
	 if (t(left+1) .eq. t(left))	go to 50
	 lsofar = lsofar + 1
	 break(lsofar+1) = t(left+1)
	 if (k .gt. 1)			go to 9
	 coef(1,lsofar) = bcoef(left)
					go to 50
c	 store the k b-spline coeff.s relevant to current knot interval
c			      in  scrtch(.,1) .
    9	 do 10 i=1,k
   10	    scrtch(i,1) = bcoef(left-k+i)
c
c	 for j=1,...,k-1, compute the  k-j  b-spline coeff.s relevant to
c	 current knot interval for the j-th derivative by differencing
c	 those for the (j-1)st derivative, and store in scrtch(.,j+1) .
	 do 20 jp1=2,k
	    j = jp1 - 1
	    kmj = k - j
	    fkmj = float(kmj)
	    do 20 i=1,kmj
	       diff = t(left+i) - t(left+i - kmj)
	       if (diff .gt. 0.)  scrtch(i,jp1) =
     *			     ((scrtch(i+1,j)-scrtch(i,j))/diff)*fkmj
   20	       continue
c
c	 for  j = 0, ..., k-1, find the values at  t(left)  of the  j+1
c	 b-splines of order  j+1  whose support contains the current
c	 knot interval from those of order  j  (in  biatx ), then comb-
c	 ine with the b-spline coeff.s (in scrtch(.,k-j) ) found earlier
c	 to compute the (k-j-1)st derivative at  t(left)  of the given
c	 spline.
c	    note. if the repeated calls to  bsplvb  are thought to gene-
c	 rate too much overhead, then replace the first call by
c	    biatx(1) = 1.
c	 and the subsequent call by the statement
c	    j = jp1 - 1
c	 followed by a direct copy of the lines
c	    deltar(j) = t(left+j) - x
c		   ......
c	    biatx(j+1) = saved
c	 from  bsplvb . deltal(kmax)  and  deltar(kmax)  would have to
c	 appear in a dimension statement, of course.
c
	 call bsplvb ( t, 1, 1, t(left), left, biatx )
	 coef(k,lsofar) = scrtch(1,k)
	 do 30 jp1=2,k
	    call bsplvb ( t, jp1, 2, t(left), left, biatx )
	    kmj = k+1 - jp1
	    sum = 0.
	    do 28 i=1,jp1
   28	       sum = biatx(i)*scrtch(i,kmj) + sum
   30	    coef(kmj,lsofar) = sum
   50	 continue
      l = lsofar
					return
      end
