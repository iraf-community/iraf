chapter xii, example 2. cubic spline interpolation with good knots
c  from  * a practical guide to splines *  by c. de boor
calls  cubspl, newnot
c     parameter nmax=20
      integer i,istep,iter,itermx,j,n,nhigh,nlow,nm1
c     real algerp,aloger,decay,dx,errmax,c(4,nmax),g,h,pnatx
c    *	  ,scrtch(2,nmax),step,tau(nmax),taunew(nmax)
      real algerp,aloger,decay,dx,errmax,c(4,20),g,h,pnatx
     *	  ,scrtch(2,20),step,tau(20),taunew(20),x
c     istep and step = float(istep) specify point density for error det-
c     ermination.
      data step, istep /20., 20/
c				the function  g  is to be interpolated .
      g(x) = sqrt(x+1.)
      decay = 0.
c      read in the number of iterations to be carried out and the lower
c      and upper limit for the number  n  of data points to be used.
      read 500,itermx,nlow,nhigh
  500 format(3i3)
      print 600, itermx
  600 format(i4,22h cycles through newnot//
     *	    28h  n   max.error	 decay exp./)
c				  loop over  n = number of data points .
      do 40 n=nlow,nhigh,2
c					 knots are initially equispaced.
	 nm1 = n-1
	 h = 2./float(nm1)
	 do 10 i=1,n
   10	    tau(i) = float(i-1)*h - 1.
	 iter = 1
c		construct cubic spline interpolant. then, itermx  times,
c		 determine new knots from it and find a new interpolant.
   11	    do 15 i=1,n
   15	       c(1,i) = g(tau(i))
	    call cubspl ( tau, c, n, 0, 0 )
	    if (iter .gt. itermx)	go to 19
	    iter = iter+1
	    call newnot(tau,c,nm1,4,taunew,nm1,scrtch)
	    do 18 i=1,n
   18	       tau(i) = taunew(i)
					go to 11
   19	 continue
c			     estimate max.interpolation error on (-1,1).
	 errmax = 0.
c			    loop over polynomial pieces of interpolant .
	 do 30 i=1,nm1
	    dx = (tau(i+1)-tau(i))/step
c		 interpolation error is calculated at  istep  points per
c		     polynomial piece .
	    do 30 j=1,istep
	       h = float(j)*dx
	       pnatx = c(1,i)+h*(c(2,i)+h*(c(3,i)+h*c(4,i)/3.)/2.)
   30	    errmax = amax1(errmax,abs(g(tau(i)+h)-pnatx))
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
