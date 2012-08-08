chapter xvi, example 3. two parametrizations of some data
c  from  * a practical guide to splines *  by c. de boor
calls  splint(bsplvb,banfac/slv),bsplpp(bsplvb*),banslv*,ppvalu(interv)
c     parameter k=4,kpkm1=7, n=8,npk=12, npiece=6,npoint=21
c     integer i,icount,iflag,kp1,l
c     real bcoef(n),break(npiece),ds,q(n,kpkm1),s(n),scrtch(k,k)
c    *	  ,ss,t(npk),x(n),xcoef(k,npiece),xx(npoint),y(n)
c    *	  ,ycoef(k,npiece),yy(npoint)
      integer i,icount,iflag,k,kpkm1,kp1,l,n,npoint
      real bcoef(8),break(6),ds,q(8,7),s(8),scrtch(4,4)
     *	  ,ss,t(12),x(8),xcoef(4,6),xx(21),y(8)
     *	  ,ycoef(4,6),yy(21)
      real ppvalu
      data k,kpkm1,n,npoint /4,7,8,21/
      data x /0.,.1,.2,.3,.301,.4,.5,.6/
c  *** compute y-component and set 'natural' parametrization
      do 1 i=1,n
	 y(i) = (x(i)-.3)**2
    1	 s(i) = x(i)
      print 601
  601 format(26h 'natural' parametrization/6x,1hx,11x,1hy)
      icount = 1
c  *** convert data abscissae to knots. note that second and second
c     last data abscissae are not knots.
    5 do 6 i=1,k
	 t(i) = s(1)
    6	 t(n+i) = s(n)
      kp1 = k+1
      do 7 i=kp1,n
    7	 t(i) = s(i+2-k)
c  *** interpolate to x-component
      call splint(s,x,t,n,k,q,bcoef,iflag)
      call bsplpp(t,bcoef,n,k,scrtch,break,xcoef,l)
c  *** interpolate to y-component. since data abscissae and knots are
c     the same for both components, we only need to use backsubstitution
      do 10 i=1,n
   10	 bcoef(i) = y(i)
      call banslv(q,kpkm1,n,k-1,k-1,bcoef)
      call bsplpp(t,bcoef,n,k,scrtch,break,ycoef,l)
c  *** evaluate curve at some points near the potential trouble spot,
c     the fourth and fifth data points.
      ss = s(3)
      ds = (s(6)-s(3))/float(npoint-1)
      do 20 i=1,npoint
	 xx(i) = ppvalu(break,xcoef,l,k,ss,0)
	 yy(i) = ppvalu(break,ycoef,l,k,ss,0)
   20	 ss = ss + ds
      print 620,(xx(i),yy(i),i=1,npoint)
  620 format(2f12.7)
      if (icount .ge. 2)		stop
c  *** now repeat the whole process with uniform parametrization
      icount = icount + 1
      do 30 i=1,n
   30	 s(i) = float(i)
      print 630
  630 format(/26h 'uniform' parametrization/6x,1hx,11x,1hy)
					go to 5
      end
