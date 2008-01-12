chapter x. example 3. construction and evaluation of the pp-representat-
c		      ion of a b-spline.
c  from  * a practical guide to splines *  by c. de boor
calls bsplpp(bsplvb),ppvalu(interv)
c
      integer ia,l
      real bcoef(7),break(5),coef(4,4),scrtch(4,4),t(11),value,x
      real ppvalu
c		 set knot sequence  t  and b-coeffs for  b(4,4,t)  ....
      data t / 4*0.,1.,3.,4.,4*6. /, bcoef / 3*0.,1.,3*0. /
c			   construct pp-representation	....
      call bsplpp ( t, bcoef, 7, 4, scrtch, break, coef, l )
c
c     as a check, evaluate  b(4,4,t)  from its pp-repr. on a fine mesh.
c	      the values should agree with (some of) those generated in
c			 example 2 .
      do 20 ia=1,40
	 x = float(ia)*.2 - 1.
	 value = ppvalu ( break, coef, l, 4, x, 0 )
   20	 print 620, x, value
  620 format(f10.1,f20.8)
					stop
      end
