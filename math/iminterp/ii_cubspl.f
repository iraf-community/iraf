      subroutine iicbsp (tau, c, n, ibcbeg, ibcend)
c  from  * a practical guide to splines *  by c. de boor
c     ************************	input  ***************************
c     n = number of data points. assumed to be .ge. 2.
c     (tau(i), c(1,i), i=1,...,n) = abscissae and ordinates of the
c	 data points. tau is assumed to be strictly increasing.
c     ibcbeg, ibcend = boundary condition indicators, and
c     c(2,1), c(2,n) = boundary condition information. specifically,
c	 ibcbeg = 0  means no boundary condition at tau(1) is given.
c	    in this case, the not-a-knot condition is used, i.e. the
c	    jump in the third derivative across tau(2) is forced to
c	    zero, thus the first and the second cubic polynomial pieces
c	    are made to coincide.)
c	 ibcbeg = 1  means that the slope at tau(1) is made to equal
c	    c(2,1), supplied by input.
c	 ibcbeg = 2  means that the second derivative at tau(1) is
c	    made to equal c(2,1), supplied by input.
c	 ibcend = 0, 1, or 2 has analogous meaning concerning the
c	    boundary condition at tau(n), with the additional infor-
c	    mation taken from c(2,n).
c     ***********************  output  **************************
c     c(j,i), j=1,...,4; i=1,...,l (= n-1) = the polynomial coefficients
c	 of the cubic interpolating spline with interior knots (or
c	 joints) tau(2), ..., tau(n-1). precisely, in the interval
c	 interval (tau(i), tau(i+1)), the spline f is given by
c	    f(x) = c(1,i)+h*(c(2,i)+h*(c(3,i)+h*c(4,i)/3.)/2.)
c	 where h = x - tau(i). the function program *ppvalu* may be
c	 used to evaluate f or its derivatives from tau,c, l = n-1,
c	 and k=4.
      integer ibcbeg,ibcend,n,	 i,j,l,m
      real c(4,n),tau(n),   divdf1,divdf3,dtau,g
c****** a tridiagonal linear system for the unknown slopes s(i) of
c  f  at tau(i), i=1,...,n, is generated and then solved by gauss elim-
c  ination, with s(i) ending up in c(2,i), all i.
c     c(3,.) and c(4,.) are used initially for temporary storage.
      l = n-1
compute first differences of tau sequence and store in c(3,.). also,
compute first divided difference of data and store in c(4,.).
      do 10 m=2,n
	 c(3,m) = tau(m) - tau(m-1)
   10	 c(4,m) = (c(1,m) - c(1,m-1))/c(3,m)
construct first equation from the boundary condition, of the form
c	      c(4,1)*s(1) + c(3,1)*s(2) = c(2,1)
      if (ibcbeg-1)			11,15,16
   11 if (n .gt. 2)			go to 12
c     no condition at left end and n = 2.
      c(4,1) = 1.
      c(3,1) = 1.
      c(2,1) = 2.*c(4,2)
					go to 25
c     not-a-knot condition at left end and n .gt. 2.
   12 c(4,1) = c(3,3)
      c(3,1) = c(3,2) + c(3,3)
      c(2,1) =((c(3,2)+2.*c(3,1))*c(4,2)*c(3,3)+c(3,2)**2*c(4,3))/c(3,1)
					go to 19
c     slope prescribed at left end.
   15 c(4,1) = 1.
      c(3,1) = 0.
					go to 18
c     second derivative prescribed at left end.
   16 c(4,1) = 2.
      c(3,1) = 1.
      c(2,1) = 3.*c(4,2) - c(3,2)/2.*c(2,1)
   18 if(n .eq. 2)			go to 25
c  if there are interior knots, generate the corresp. equations and car-
c  ry out the forward pass of gauss elimination, after which the m-th
c  equation reads    c(4,m)*s(m) + c(3,m)*s(m+1) = c(2,m).
   19 do 20 m=2,l
	 g = -c(3,m+1)/c(4,m-1)
	 c(2,m) = g*c(2,m-1) + 3.*(c(3,m)*c(4,m+1)+c(3,m+1)*c(4,m))
   20	 c(4,m) = g*c(3,m-1) + 2.*(c(3,m) + c(3,m+1))
construct last equation from the second boundary condition, of the form
c	    (-g*c(4,n-1))*s(n-1) + c(4,n)*s(n) = c(2,n)
c     if slope is prescribed at right end, one can go directly to back-
c     substitution, since c array happens to be set up just right for it
c     at this point.
      if (ibcend-1)			21,30,24
   21 if (n .eq. 3 .and. ibcbeg .eq. 0) go to 22
c     not-a-knot and n .ge. 3, and either n.gt.3 or  also not-a-knot at
c     left end point.
      g = c(3,n-1) + c(3,n)
      c(2,n) = ((c(3,n)+2.*g)*c(4,n)*c(3,n-1)
     *		  + c(3,n)**2*(c(1,n-1)-c(1,n-2))/c(3,n-1))/g
      g = -g/c(4,n-1)
      c(4,n) = c(3,n-1)
					go to 29
c     either (n=3 and not-a-knot also at left) or (n=2 and not not-a-
c     knot at left end point).
   22 c(2,n) = 2.*c(4,n)
      c(4,n) = 1.
					go to 28
c     second derivative prescribed at right endpoint.
   24 c(2,n) = 3.*c(4,n) + c(3,n)/2.*c(2,n)
      c(4,n) = 2.
					go to 28
   25 if (ibcend-1)			26,30,24
   26 if (ibcbeg .gt. 0)		go to 22
c     not-a-knot at right endpoint and at left endpoint and n = 2.
      c(2,n) = c(4,n)
					go to 30
   28 g = -1./c(4,n-1)
complete forward pass of gauss elimination.
   29 c(4,n) = g*c(3,n-1) + c(4,n)
      c(2,n) = (g*c(2,n-1) + c(2,n))/c(4,n)
carry out back substitution
   30 j = l
   40	 c(2,j) = (c(2,j) - c(3,j)*c(2,j+1))/c(4,j)
	 j = j - 1
	 if (j .gt. 0)			go to 40
c****** generate cubic coefficients in each interval, i.e., the deriv.s
c  at its left endpoint, from value and slope at its endpoints.
      do 50 i=2,n
	 dtau = c(3,i)
	 divdf1 = (c(1,i) - c(1,i-1))/dtau
	 divdf3 = c(2,i-1) + c(2,i) - 2.*divdf1
	 c(3,i-1) = 2.*(divdf1 - c(2,i-1) - divdf3)/dtau
   50	 c(4,i-1) = (divdf3/dtau)*(6./dtau)
					return
      end
