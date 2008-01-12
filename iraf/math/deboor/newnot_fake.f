      subroutine newnot ( break, coef, l, k, brknew, lnew, coefg )
c  from  * a practical guide to splines *  by c. de boor
c  this is a fake version of  n e w n o t  , of use in example 3 of
c  chapter xiv .
c  returns  lnew+1  knots in  brknew  which are equidistributed on (a,b)
c  = (break(1),break(l+1)) .
c
      integer k,l,lnew,   i
      real break(1),brknew(1),coef(k,l),coefg(2,l),   step
c******  i n p u t ******
c  break, coef, l, k.....contains the pp-representation of a certain
c	 function  f  of order	k . specifically,
c	 d**(k-1)f(x) = coef(k,i)  for	break(i).le. x .lt.break(i+1)
c  lnew.....number of intervals into which the interval (a,b) is to be
c	 sectioned by the new breakpoint sequence  brknew .
c
c******  o u t p u t  ******
c  brknew.....array of length  lnew+1  containing the new breakpoint se-
c	 quence
c  coefg.....the coefficient part of the pp-repr.  break, coefg, l, 2
c	 for the monotone p.linear function  g	wrto which  brknew  will
c	 be equidistributed.
c
      brknew(1) = break(1)
      brknew(lnew+1) = break(l+1)
      step = (break(l+1) - break(1))/float(lnew)
      do 93 i=2,lnew
   93	 brknew(i) = break(1) + float(i-1)*step
					return
      end
