chapter ii. runge example
c  from  * a practical guide to splines *  by c. de boor
      integer i,istep,j,k,n,nmk,nm1
      real aloger,algerp,d(20),decay,dx,errmax,g,h,pnatx,step,tau(20),x
      data step, istep /20., 20/
      g(x) = 1./(1.+(5.*x)**2)
      print 600
  600 format(28h  n   max.error   decay exp.//)
      decay = 0.
      do 40 n=2,20,2
c	 choose interpolation points  tau(1), ..., tau(n) , equally
c	 spaced in (-1,1), and set  d(i) = g(tau(i)), i=1,...,n.
	 nm1 = n-1
	 h = 2./float(nm1)
	 do 10 i=1,n
	    tau(i) = float(i-1)*h - 1.
   10	    d(i) = g(tau(i))
c	 calculate the divided differences for the newton form.
c
	 do 20 k=1,nm1
	    nmk = n-k
	    do 20 i=1,nmk
   20	       d(i) = (d(i+1)-d(i))/(tau(i+k)-tau(i))
c
c	 estimate max.interpolation error on (-1,1).
	 errmax = 0.
	 do 30 i=2,n
	    dx = (tau(i)-tau(i-1))/step
	    do 30 j=1,istep
	       x = tau(i-1) + float(j)*dx
c	       evaluate interp.pol. by nested multiplication
c
	       pnatx = d(1)
	       do 29 k=2,n
   29		  pnatx = d(k) + (x-tau(k))*pnatx
c
   30	       errmax = amax1(errmax,abs(g(x)-pnatx))
	 aloger = alog(errmax)
	 if (n .gt. 2)	decay =
     *	     (aloger - algerp)/alog(float(n)/float(n-2))
	 algerp = aloger
   40	 print 640,n,errmax,decay
  640 format(i3,e12.4,f11.2)
					stop
      end
