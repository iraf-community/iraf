      subroutine chol1d ( p, v, qty, npoint, ncol, u, qu )
c  from  * a practical guide to splines *  by c. de boor
c  from  * a practical guide to splines *  by c. de boor
c to be called in  s m o o t h
constructs the upper three diags. in v(i,j), i=2,,npoint-1, j=1,3, of
c  the matrix  6*(1-p)*q-transp.*(d**2)*q + p*r, then computes its
c  l*l-transp. decomposition and stores it also in v, then applies
c  forward and backsubstitution to the right side q-transp.*y in  qty
c  to obtain the solution in  u .
      integer ncol,npoint,   i,npm1,npm2
      real p,qty(npoint),qu(npoint),u(npoint),v(npoint,7),   prev,ratio
     *	  ,six1mp,twop
      npm1 = npoint - 1
c     construct 6*(1-p)*q-transp.*(d**2)*q  +  p*r
      six1mp = 6.*(1.-p)
      twop = 2.*p
      do 2 i=2,npm1
	 v(i,1) = six1mp*v(i,5) + twop*(v(i-1,4)+v(i,4))
	 v(i,2) = six1mp*v(i,6) + p*v(i,4)
    2	 v(i,3) = six1mp*v(i,7)
      npm2 = npoint - 2
      if (npm2 .ge. 2)			go to 10
      u(1) = 0.
      u(2) = qty(2)/v(2,1)
      u(3) = 0.
					go to 41
c  factorization
   10 do 20 i=2,npm2
	 ratio = v(i,2)/v(i,1)
	 v(i+1,1) = v(i+1,1) - ratio*v(i,2)
	 v(i+1,2) = v(i+1,2) - ratio*v(i,3)
	 v(i,2) = ratio
	 ratio = v(i,3)/v(i,1)
	 v(i+2,1) = v(i+2,1) - ratio*v(i,3)
   20	 v(i,3) = ratio
c
c  forward substitution
      u(1) = 0.
      v(1,3) = 0.
      u(2) = qty(2)
      do 30 i=2,npm2
   30	 u(i+1) = qty(i+1) - v(i,2)*u(i) - v(i-1,3)*u(i-1)
c  back substitution
      u(npoint) = 0.
      u(npm1) = u(npm1)/v(npm1,1)
      i = npm2
   40	 u(i) = u(i)/v(i,1)-u(i+1)*v(i,2)-u(i+2)*v(i,3)
	 i = i - 1
	 if (i .gt. 1)			go to 40
c  construct q*u
   41 prev = 0.
      do 50 i=2,npoint
	 qu(i) = (u(i) - u(i-1))/v(i-1,4)
	 qu(i-1) = qu(i) - prev
   50	 prev = qu(i)
      qu(npoint) = -qu(npoint)
					return
      end
