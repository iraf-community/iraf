chapter x. example 1. plotting some b-splines
c  from  * a practical guide to splines *  by c. de boor
calls  bsplvb, interv
      integer i,j,k,left,leftmk,mflag,n,npoint
      real dx,t(10),values(7),x,xl
c  dimension, order and knot sequence for spline space are specified...
      data n,k /7,3/, t /3*0.,2*1.,3.,4.,3*6./
c  b-spline values are initialized to 0., number of evaluation points...
      data values /7*0./, npoint /31/
c  set leftmost evaluation point  xl , and spacing  dx	to be used...
      xl = t(k)
      dx = (t(n+1)-t(k))/float(npoint-1)
c
      print 600,(i,i=1,5)
  600 format(4h1  x,8x,5(1hb,i1,3h(x),7x))
c
      do 10 i=1,npoint
	 x = xl + float(i-1)*dx
c		      locate  x  with respect to knot array  t .
	 call interv ( t, n, x, left, mflag )
	 leftmk = left - k
c	    get b(i,k)(x)  in  values(i) , i=1,...,n .	k  of these,
c	 viz.  b(left-k+1,k)(x), ..., b(left,k)(x),  are supplied by
c	 bsplvb . all others are known to be zero a priori.
	 call bsplvb ( t, k, 1, x, left, values(leftmk+1) )
c
	 print 610, x, (values(j),j=3,7)
  610	 format(f7.3,5f12.7)
c
c	    zero out the values just computed in preparation for next
c	 evalulation point .
	 do 10 j=1,k
   10	    values(leftmk+j) = 0.
					stop
      end
