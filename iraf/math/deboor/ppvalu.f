      real function ppvalu (break, coef, l, k, x, jderiv )
c  from  * a practical guide to splines *  by c. de boor
calls  interv
calculates value at  x	of  jderiv-th derivative of pp fct from pp-repr
c
c******  i n p u t  ******
c  break, coef, l, k.....forms the pp-representation of the function  f
c	 to be evaluated. specifically, the j-th derivative of	f  is
c	 given by
c
c     (d**j)f(x) = coef(j+1,i) + h*(coef(j+2,i) + h*( ... (coef(k-1,i) +
c			      + h*coef(k,i)/(k-j-1))/(k-j-2) ... )/2)/1
c
c	 with  h = x - break(i),  and
c
c	i  =  max( 1 , max( j ,  break(j) .le. x , 1 .le. j .le. l ) ).
c
c  x.....the point at which to evaluate.
c  jderiv.....integer giving the order of the derivative to be evaluat-
c	 ed.  a s s u m e d  to be zero or positive.
c
c******  o u t p u t  ******
c  ppvalu.....the value of the (jderiv)-th derivative of  f  at  x.
c
c******  m e t h o d  ******
c     the interval index  i , appropriate for  x , is found through a
c  call to  interv . the formula above for the	jderiv-th derivative
c  of  f  is then evaluated (by nested multiplication).
c
      integer jderiv,k,l,   i,m,ndummy
      real break(l),coef(k,l),x,   fmmjdr,h
      ppvalu = 0.
      fmmjdr = k - jderiv
c	       derivatives of order  k	or higher are identically zero.
      if (fmmjdr .le. 0.)		go to 99
c
c	       find index  i  of largest breakpoint to the left of  x .
      call interv ( break, l, x, i, ndummy )
c
c      evaluate  jderiv-th derivative of  i-th polynomial piece at  x .
      h = x - break(i)
      m = k
    9	 ppvalu = (ppvalu/fmmjdr)*h + coef(m,i)
	 m = m - 1
	 fmmjdr = fmmjdr - 1.
	 if (fmmjdr .gt. 0.)		go to 9
   99					return
      end
