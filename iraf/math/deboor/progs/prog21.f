chapter xvii, example 3. bivariate spline interpolation
c  followed by conversion to pp-repr and evaluation
c  from  * a practical guide to splines *  by c. de boor
calls  spli2d(bsplvb,banfac/slv),interv,bspp2d(bsplvb*),ppvalu(interv*)
c     parameter nx=7,kx=3,ny=6,ky=4
c     integer i,iflag,j,jj,lefty,lx,ly,mflag
c     real bcoef(nx,ny),breakx(6),breaky(4),coef(kx,5,ky,3),taux(nx)
c    *	  ,tauy(ny),tx(10),ty(10)
c    *	       ,work1(nx,ny),work2(nx),work3(42)
c    *	       ,work4(kx,kx,ny),work5(ny,kx,5),work6(ky,ky,21)
      integer i,iflag,j,jj,kp1,kx,ky,lefty,lx,ly,mflag,nx,ny
      real bcoef(7,6),breakx(6),breaky(4),coef(3,5,4,3),taux(7)
     *	  ,tauy(6),tx(10),ty(10)
     *	       ,work1(7,6),work2(7),work3(42)
     *	       ,work4(3,3,6),work5(6,3,5),work6(4,4,21)
      real ppvalu,g,x,y
      data nx,kx,ny,ky / 7,3,6,4 /
c     note that , with the above parameters, lx=5, ly = 3
      equivalence (work4,work6)
      g(x,y) = amax1(x-3.5,0.)**2 + amax1(y-3.,0.)**3
c *** set up data points and knots
c     in x, interpolate between knots by parabolic splines, using
c     not-a-knot end condition
      do 10 i=1,nx
   10	 taux(i) = float(i)
      do 11 i=1,kx
	 tx(i) = taux(1)
   11	 tx(nx+i) = taux(nx)
      kp1 = kx+1
      do 12 i=kp1,nx
   12	 tx(i) = (taux(i-kx+1) + taux(i-kx+2))/2.
c     in y, interpolate at knots by cubic splines, using not-a-knot
c     end condition
      do 20 i=1,ny
   20	 tauy(i) = float(i)
      do 21 i=1,ky
	 ty(i) = tauy(1)
   21	 ty(ny+i) = tauy(ny)
      kp1 = ky+1
      do 22 i=kp1,ny
   22	 ty(i) = tauy(i-ky+2)
c  *** generate and print out function values
      print 620,(tauy(i),i=1,ny)
  620 format(11h given data//6f12.1)
      do 32 i=1,nx
	 do 31 j=1,ny
   31	    bcoef(i,j) = g(taux(i),tauy(j))
   32	 print 632,taux(i),(bcoef(i,j),j=1,ny)
  632 format(f5.1,6e12.5)
c
c  *** construct b-coefficients of interpolant
c
      call spli2d(taux,bcoef,tx,nx,kx,ny,work2,work3,work1,iflag)
      call spli2d(tauy,work1,ty,ny,ky,nx,work2,work3,bcoef,iflag)
c
c  *** convert to pp-representation
c
      call bspp2d(tx,bcoef,nx,kx,ny,work4,breakx,work5,lx)
      call bspp2d(ty,work5,ny,ky,lx*kx,work6,breaky,coef,ly)
c
c  *** evaluate interpolation error at mesh points and print out
      print 640,(tauy(j),j=1,ny)
  640 format(//20h interpolation error//6f12.1)
      do 45 j=1,ny
	 call interv(breaky,ly,tauy(j),lefty,mflag)
	 do 45 i=1,nx
	    do 41 jj=1,ky
   41	     work2(jj)=ppvalu(breakx,coef(1,1,jj,lefty),lx,kx,taux(i),0)
   45	    work1(i,j) = g(taux(i),tauy(j)) -
     *	    ppvalu(breaky(lefty),work2,1,ky,tauy(j),0)
      do 46 i=1,nx
   46	 print 632,taux(i),(work1(i,j),j=1,ny)
					stop
      end
