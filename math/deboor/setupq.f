      subroutine setupq ( x, dx, y, npoint, v, qty )
c  from  * a practical guide to splines *  by c. de boor
c  to be called in  s m o o t h
c  put	delx = x(.+1) - x(.)  into  v(.,4),
c  put	the three bands of  q-transp*d	into  v(.,1-3), and
c  put the three bands of  (d*q)-transp*(d*q)  at and above the diagonal
c     into  v(.,5-7) .
c     here,  q is  the tridiagonal matrix of order (npoint-2,npoint)
c  with general row  1/delx(i) , -1/delx(i) - 1/delx(i+1) , 1/delx(i+1)
c  and	 d  is the diagonal matrix  with general row  dx(i) .
      integer npoint,	i,npm1
      real dx(npoint),qty(npoint),v(npoint,7),x(npoint),y(npoint),
     *							  diff,   prev
      npm1 = npoint - 1
      v(1,4) = x(2) - x(1)
      do 11 i=2,npm1
	 v(i,4) = x(i+1) - x(i)
	 v(i,1) = dx(i-1)/v(i-1,4)
	 v(i,2) = - dx(i)/v(i,4) - dx(i)/v(i-1,4)
   11	 v(i,3) = dx(i+1)/v(i,4)
      v(npoint,1) = 0.
      do 12 i=2,npm1
   12	 v(i,5) = v(i,1)**2 + v(i,2)**2 + v(i,3)**2
      if (npm1 .lt. 3)			go to 14
      do 13 i=3,npm1
   13	 v(i-1,6) = v(i-1,2)*v(i,1) + v(i-1,3)*v(i,2)
   14 v(npm1,6) = 0.
      if (npm1 .lt. 4)			go to 16
      do 15 i=4,npm1
   15	 v(i-2,7) = v(i-2,3)*v(i,1)
   16 v(npm1-1,7) = 0.
      v(npm1,7) = 0.
construct  q-transp. * y  in  qty.
      prev = (y(2) - y(1))/v(1,4)
      do 21 i=2,npm1
	 diff = (y(i+1)-y(i))/v(i,4)
	 qty(i) = diff - prev
   21	 prev = diff
					return
      end
