chapter xiv, example 1. cubic smoothing spline
c  from  * a practical guide to splines *  by c. de boor
calls  bsplpp(bsplvb), ppvalu(interv), smooth(setupq,chol1d)
c     values from a cubic b-spline are rounded to  n d i g i t	places
c  after the decimal point, then smoothed via  s m o o t h  for
c  various values of the control parameter  s .
      integer i,is,j,l,lsm,ndigit,npoint,ns
      real a(61,4),bcoef(7),break(5),coef(4,4),coefsm(4,60),dely,dy(61)
     *	  ,s(20),scrtch(427),sfp,t(11),tenton,x(61),y(61)
      real ppvalu,smooth
      equivalence (scrtch,coefsm)
      data t /4*0.,1.,3.,4.,4*6./
      data bcoef /3*0.,1.,3*0./
      call bsplpp(t,bcoef,7,4,scrtch,break,coef,l)
      npoint = 61
      read 500,ndigit,ns,(s(i),i=1,ns)
  500 format(2i3/(e10.4))
      print 600,ndigit
  600 format(24h exact values rounded to,i2,21h digits after decimal
     *	     ,7h point./)
      tenton = 10.**ndigit
      dely = .5/tenton
      do 10 i=1,npoint
	 x(i) = .1*float(i-1)
	 y(i) = ppvalu(break,coef,l,4,x(i),0)
	 y(i) = float(int(y(i)*tenton + .5))/tenton
   10	 dy(i) = dely
      do 15 i=1,npoint,5
	 do 15 j=1,4
   15	    a(i,j) = ppvalu(break,coef,l,4,x(i),j-1)
      print 615,(i,(a(i,j),j=1,4),i=1,npoint,5)
  615 format(52h value and derivatives of noisefree function at some
     *	    ,7h points/(i4,4e15.7))
      do 20 is=1,ns
	 sfp = smooth ( x, y, dy, npoint, s(is), scrtch, a )
	 lsm = npoint - 1
	 do 16 i=1,lsm
	    do 16 j=1,4
   16	       coefsm(j,i) = a(i,j)
	 do 18 i=1,npoint,5
	    do 18 j=1,4
   18	       a(i,j) = ppvalu(x,coefsm,lsm,4,x(i),j-1)
   20	 print 620, s(is), sfp, (i,(a(i,j),j=1,4),i=1,npoint,5)
  620 format(15h prescribed s =,e10.3,23h, s(smoothing spline) =,e10.3/
     *	     54h value and derivatives of smoothing spline at corresp.
     *	    ,7h points/(i4,4e15.7))
					stop
      end
