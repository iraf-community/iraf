chapter xii, example 3. cubic spline interpolation with good knots
c  from  * a practical guide to splines *  by c. de boor
calls cubspl
      integer i,irate,istep,j,n,nhigh,nlow,nm1
      real algerp,aloger,c(4,20),decay,dx,errmax,g,h,pnatx,step,tau(20)
     *	  ,x
      data step, istep /20., 20/
      g(x) = sqrt(x+1.)
      decay = 0.
      read 500,irate,nlow,nhigh
  500 format(3i3)
      print 600
  600 format(28h  n   max.error   decay exp./)
      do 40 n=nlow,nhigh,2
	 nm1 = n-1
	 h = 1./float(nm1)
	 do 10 i=1,n
	    tau(i) = 2.*(float(i-1)*h)**irate - 1.
   10	    c(1,i) = g(tau(i))
c	 construct cubic spline interpolant.
	 call cubspl ( tau, c, n, 0, 0 )
c	 estimate max.interpolation error on (-1,1).
	 errmax = 0.
	 do 30 i=1,nm1
	    dx = (tau(i+1)-tau(i))/step
	    do 30 j=1,istep
	       h = float(j)*dx
	       pnatx = c(1,i)+h*(c(2,i)+h*(c(3,i)+h*c(4,i)/3.)/2.)
	       x = tau(i) + h
   30	       errmax = amax1(errmax,abs(g(x)-pnatx))
	 aloger = alog(errmax)
	 if (n .gt. nlow)  decay =
     *	     (aloger - algerp)/alog(float(n)/float(n-2))
	 algerp = aloger
   40	 print 640,n,errmax,decay
  640 format(i3,e12.4,f11.2)
					stop
      end
