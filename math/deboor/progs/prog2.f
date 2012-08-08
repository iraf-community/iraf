chapter iv. runge example, with cubic hermite interpolation
c  from  * a practical guide to splines *  by c. de boor
      integer i,istep,j,n,nm1
      real aloger,algerp,c(4,20),decay,divdf1,divdf3,dtau,dx,errmax,g,h
     *	  ,pnatx,step,tau(20),x
      data step, istep /20., 20/
      g(x) = 1./(1.+(5.*x)**2)
      print 600
  600 format(28h  n   max.error   decay exp.//)
      decay = 0.
      do 40 n=2,20,2
c	 choose interpolation points  tau(1), ..., tau(n) , equally
c	 spaced in (-1,1), and set  c(1,i) = g(tau(i)), c(2,i) =
c	 gprime(tau(i)) = -50.*tau(i)*g(tau(i))**2, i=1,...,n.
	 nm1 = n-1
	 h = 2./float(nm1)
	 do 10 i=1,n
	    tau(i) = float(i-1)*h - 1.
	    c(1,i) = g(tau(i))
   10	    c(2,i) = -50.*tau(i)*c(1,i)**2
c	 calculate the coefficients of the polynomial pieces
c
	 do 20 i=1,nm1
	    dtau = tau(i+1) - tau(i)
	    divdf1 = (c(1,i+1) - c(1,i))/dtau
	    divdf3 = c(2,i) + c(2,i+1) - 2.*divdf1
	    c(3,i) = (divdf1 - c(2,i) - divdf3)/dtau
   20	    c(4,i) = (divdf3/dtau)/dtau
c
c	 estimate max.interpolation error on (-1,1).
	 errmax = 0.
	 do 30 i=2,n
	    dx = (tau(i)-tau(i-1))/step
	    do 30 j=1,istep
	       h = float(j)*dx
c	       evaluate (i-1)st cubic piece
c
	       pnatx = c(1,i-1)+h*(c(2,i-1)+h*(c(3,i-1)+h*c(4,i-1)))
c
   30	       errmax = amax1(errmax,abs(g(tau(i-1)+h)-pnatx))
	 aloger = alog(errmax)
	 if (n .gt. 2)	decay =
     *	     (aloger - algerp)/alog(float(n)/float(n-2))
	 algerp = aloger
   40	 print 640,n,errmax,decay
  640 format(i3,e12.4,f11.2)
					stop
      end
