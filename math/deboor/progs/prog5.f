chapter x. example 2. plotting the pol,s which make up a b-spline
c  from  * a practical guide to splines *  by c. de boor
calls  bsplvb
c
      integer ia,left
      real biatx(4),t(11),values(4),x
c		  knot sequence set here....
      data t / 4*0.,1.,3.,4.,4*6. /
      do 20 ia=1,40
	 x = float(ia)*.2 - 1.
	 do 10 left=4,7
	    call bsplvb ( t, 4, 1, x, left, biatx )
c
c	    according to  bsplvb  listing,  biatx(.) now contains value
c	    at	x  of polynomial which agrees on the interval  (t(left),
c	    t(left+1) )  with the b-spline  b(left-4 + . ,4,t) . hence,
c	    biatx(8-left)  now contains value of that polynomial for
c	    b(left-4 +(8-left) ,4,t) = b(4,4,t) . since this b-spline
c	    has support  (t(4),t(8)), it makes sense to run  left = 4,
c	    ...,7, storing the resulting values in  values(1),...,
c	    values(4)  for later printing.
c
   10	    values(left-3) = biatx(8-left)
   20	 print 620, x, values
  620 format(f10.1,4f20.8)
					stop
      end
