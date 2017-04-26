chapter x.  example 4. construction of a b-spline via  bvalue
c  from  * a practical guide to splines *  by c. de boor
calls  bvalue(interv)
      integer ia
      real bcoef(1),t(5),value,x
      real bvalue
c		   set knot sequence  t  and  b-coeffs for  b(1,4,t)
      data t / 0.,1.,3.,4.,6. / , bcoef / 1. /
c	evaluate  b(1,4,t)  on a fine mesh.  on (0,6), the values should
c		coincide with those obtained in example 3 .
      do 20 ia=1,40
	 x = float(ia)*.2 - 1.
	 value = bvalue ( t, bcoef, 1, 4, x, 0 )
   20	 print 620, x, value
  620 format(f10.1,f20.8)
					stop
      end
