chapter xiii, example 2. cubic spline interpolant at knot averages
c			 with good knots.
c  from  * a practical guide to splines *  by c. de boor
calls splint(banfac/slv,bsplvb),newnot,bsplpp(bsplvb*)
c     parameter nmax=20,nmaxp6=26,nmaxp2=22,nmaxt7=140
      integer i,istep,iter,itermx,n,nhigh,nmk,nlow
c     real algerp,aloger,bcoef(nmaxp2),break(nmax),decay,dx,errmax,
c    *	  c(4,nmax),g,gtau(nmax),h,pnatx,scrtch(nmaxt7),step,t(nmaxp6),
c    *	  tau(nmax),tnew(nmax)
      real algerp,aloger,bcoef(22),break(20),decay,dx,errmax,
     *	  c(4,20),g,gtau(20),h,pnatx,scrtch(140),step,t(26),
     *	  tau(20),tnew(20),x
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
    4	 nmk = n-4
	 h = 2./float(nmk+1)
	 do 5 i=1,4
	    t(i) = -1.
    5	    t(n+i) = 1.
	 if (nmk .lt. 1)		go to 10
	 do 6 i=1,nmk
    6	    t(i+4) = float(i)*h - 1.
   10	 iter = 1
c		construct cubic spline interpolant. then, itermx  times,
c		 determine new knots from it and find a new interpolant.
   11	    do 12 i=1,n
	       tau(i) = (t(i+1)+t(i+2)+t(i+3))/3.
   12	       gtau(i) = g(tau(i))
	    call splint ( tau, gtau, t, n, 4, scrtch, bcoef, iflag )
	    call bsplpp ( t, bcoef, n, 4, scrtch, break, c, l )
	    if (iter .gt. itermx)	go to 19
	    iter = iter + 1
	    call newnot ( break, c, l, 4, tnew, l, scrtch )
   15	    do 18 i=2,l
   18	       t(3+i) = tnew(i)
					go to 11
c		       estimate max.interpolation error on  (-1,1) .
   19	 errmax = 0.
c			    loop over polynomial pieces of interpolant .
	 do 30 i=1,l
	    dx = (break(i+1)-break(i))/step
c		 interpolation error is calculated at  istep  points per
c		     polynomial piece .
	    do 30 j=1,istep
	       h = float(j)*dx
	       pnatx = c(1,i)+h*(c(2,i)+h*(c(3,i)+h*c(4,i)/3.)/2.)
   30	    errmax = amax1(errmax,abs(g(break(i)+h)-pnatx))
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
