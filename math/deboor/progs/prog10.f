chapter xii, example 4. quasi-interpolant with good knots.
c  from  * a practical guide to splines *  by c. de boor
calls bsplpp(bsplvb)
c
      integer i,irate,istep,j,l,m,mmk,n,nlow,nhigh,nm1
      real algerp,aloger,bcoef(22),break(20),c(4,20),decay,dg,ddg,x
     *	  ,dtip1,dtip2,dx,errmax,g,h,pnatx,scrtch(4,4),step,t(26),taui
c     istep and step = float(istep) specify point density for error det-
c     ermination.
      data step, istep /20., 20/
c	 g  is the function to be approximated,  dg  is its first, and
c	 ddg  its second derivative .
      g(x) = sqrt(x+1.)
      dg(x) = .5/g(x)
      ddg(x) = -.5*dg(x)/(x+1.)
      decay = 0.
c      read in the exponent  irate  for the knot distribution and the
c      lower and upper limit for the number  n	.
      read 500,irate,nlow,nhigh
  500 format(3i3)
      print 600
  600 format(28h  n   max.error   decay exp./)
c			       loop over  n = dim( spline(4,t) ) - 2 .
c	       n  is chosen as the parameter in order to afford compar-
c	       ison with examples 2 and 3  in which cubic spline interp-
c	       olation at  n  data points was used .
      do 40 n=nlow,nhigh,2
	 nm1 = n-1
	 h = 1./float(nm1)
	 m = n+2
	 mmk = m-4
	 do 5 i=1,4
	    t(i) = -1.
    5	    t(m+i) = 1.
c		  interior knots are equidistributed with respect to the
c		  function  (x + 1)**(1/irate) .
	 do 6 i=1,mmk
    6	    t(i+4) = 2.*(float(i)*h)**irate - 1.
c					    construct quasi-interpolant.
c						 bcoef(1) = g(-1.) = 0.
	 bcoef(1) = 0.
	 dtip2 = t(5) - t(4)
	 taui = t(5)
c			    special choice of  tau(2)  to avoid infinite
c			    derivatives of  g  at left endpoint .
	 bcoef(2) = g(taui) - 2.*dtip2*dg(taui)/3.
     *		   + dtip2**2*ddg(taui)/6.
	 do 15 i=3,m
	    taui = t(i+2)
	    dtip1 = dtip2
	    dtip2 = t(i+3) - t(i+2)
c				       formula xii(30) of text is used .
   15	    bcoef(i) = g(taui) + (dtip2-dtip1)*dg(taui)/3.
     *		     - dtip1*dtip2*ddg(taui)/6.
c					  convert to pp-representation .
	 call bsplpp(t,bcoef,m,4,scrtch,break,c,l)
c			     estimate max.interpolation error on (-1,1).
	 errmax = 0.
c					      loop over cubic pieces ...
	 do 30 i=1,l
	    dx = (break(i+1)-break(i))/step
c			error is calculated at	istep  points per piece.
	    do 30 j=1,istep
	       h = float(j)*dx
	       pnatx = c(1,i)+h*(c(2,i)+h*(c(3,i)+h*c(4,i)/3.)/2.)
   30	       errmax = amax1(errmax,abs(g(break(i)+h)-pnatx))
c					      calculate decay exponent .
	 aloger = alog(errmax)
	 if (n .gt. nlow)  decay =
     *	     (aloger - algerp)/alog(float(n)/float(n-2))
	 algerp = aloger
c
   40	 print 640,n,errmax,decay
  640 format(i3,e12.4,f11.2)
					stop
      end
