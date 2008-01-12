chapter xiii, example 1. a large norm amplifies noise.
c  from  * a practical guide to splines *  by c. de boor
calls splint(banfac/slv,bsplvb),bsplpp(bsplvb*),ppvalu(interv),round
c     an initially uniform data point distribution of  n  points is
c  changed  i t e r m x  times by moving the  j c l o s e -th data point
c  toward its left neighbor, cutting the distance between the two by a
c  factor of  r a t e  each time . to demonstrate the corresponding
c  increase in the norm of cubic spline interpolation at these data
c  points, the data are taken from a cubic polynomial (which would be
c  interpolated exactly) but with noise of size  s i z e  added. the re-
c  sulting noise or error in the interpolant (compared to the cubic)
c  gives the norm or noise amplification factor and is printed out
c  together with the diminishing distance  h  between the two data
c  points.
c     parameter nmax=200,nmaxt4=800,nmaxt7=1400,nmaxp4=204
      integer i,iflag,istep,iter,itermx,j,jclose,l,n,nm1
c     real amax,bcoef(nmax),break(nmax),coef(nmaxt4),dx,fltnm1,fx
c    * ,gtau(nmax),h,rate,scrtch(nmaxt7),size,step,t(nmaxp4),tau(nmax),x
      real amax,bcoef(200),break(200),coef(800),dx,fltnm1,fx
     * ,gtau(200),h,rate,scrtch(1400),size,step,t(204),tau(200),x
      real ppvalu,round,g
      common /rount/ size
      data step, istep / 20., 20 /
c					   function to be interpolated .
      g(x) = 1.+x*(1.+x*(1.+x))
      read 500,n,itermx,jclose,size,rate
  500 format(3i3/e10.3/e10.3)
      print 600,size
  600 format(16h size of noise =,e8.2//
     *	    25h    h	       max.error)
c					start with uniform data points .
      nm1 = n - 1
      fltnm1 = float(nm1)
      do 10 i=1,n
   10	 tau(i) = float(i-1)/fltnm1
c		     set up knot sequence for not-a-knot end condition .
      do 11 i=1,4
	 t(i) = tau(1)
   11	 t(n+i) = tau(n)
      do 12 i=5,n
   12	 t(i) = tau(i-2)
c
      do 100 iter=1,itermx
	 do 21 i=1,n
   21	    gtau(i) = round(g(tau(i)))
	 call splint ( tau, gtau, t, n, 4, scrtch, bcoef, iflag )
					go to (24,23),iflag
   23	 print 623
  623	 format(27h something wrong in splint.)
					stop
   24	 call bsplpp ( t, bcoef, n, 4, scrtch, break, coef, l )
c				     calculate max.interpolation error .
	 amax = 0.
	 do 30 i=4,n
	    dx = (break(i-2) - break(i-3))/step
	    do 25 j=2,istep
	       x = break(i-2) - dx*float(j-1)
	       fx = ppvalu(break,coef,l,4,x,0)
   25	       amax = amax1(amax,abs(fx-g(x)))
   30	    continue
	 h = tau(jclose) - tau(jclose-1)
	 print 630,h,amax
  630	 format(e9.2,e15.3)
c		  move tau(jclose) toward its left neighbor so as to cut
c		  their distance by a factor of  rate .
	 tau(jclose) = (tau(jclose) + (rate-1.)*tau(jclose-1))/rate
  100	 continue
					stop
      end
