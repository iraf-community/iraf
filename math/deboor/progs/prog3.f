chapter ix. example comparing the b-representation of a cubic  f  with
c  its values at knot averages.
c  from  * a practical guide to splines *  by c. de boor
c
      integer i,id,j,jj,n,nm4
      real bcoef(23),d(4),d0(4),dtip1,dtip2,f(23),t(27),tave(23),x
c	     the taylor coefficients at  0  for the polynomial	f  are
      data d0 /-162.,99.,-18.,1./
c
c		  set up knot sequence in the array  t .
      n = 13
      do 5 i=1,4
	 t(i) = 0.
    5	 t(n+i) = 10.
      nm4 = n-4
      do 6 i=1,nm4
    6	 t(i+4) = float(i)
c
      do 50 i=1,n
c	 use nested multiplication to get taylor coefficients  d  at
c		t(i+2)	from those at  0 .
	 do 20 j=1,4
   20	    d(j) = d0(j)
	 do 21 j=1,3
	    id = 4
	    do 21 jj=j,3
	       id = id-1
   21	       d(id) = d(id) + d(id+1)*t(i+2)
c
c		 compute b-spline coefficients by formula (9).
	 dtip1 = t(i+2) - t(i+1)
	 dtip2 = t(i+3) - t(i+2)
	 bcoef(i) = d(1) + (d(2)*(dtip2-dtip1)-d(3)*dtip1*dtip2)/3.
c
c		   evaluate  f	at corresp. knot average.
	 tave(i) = (t(i+1) + t(i+2) + t(i+3))/3.
	 x = tave(i)
   50	 f(i) = d0(1) + x*(d0(2) + x*(d0(3) + x*d0(4)))
c
      print 650, (i,tave(i), f(i), bcoef(i),i=1,n)
  650 format(45h  i   tave(i)	   f at tave(i)      bcoef(i)//
     *	     (i3,f10.5,2f16.5))
					stop
      end
