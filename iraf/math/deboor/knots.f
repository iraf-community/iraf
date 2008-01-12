      subroutine knots ( break, l, kpm, t, n )
c  from  * a practical guide to splines *  by c. de boor
c  to be called in  c o l l o c .
c  constructs from the given breakpoint sequence  b r e a k  the knot
c  sequence  t	so that
c  spline(k+m,t) = pp(k+m,break) with  m-1  continuous derivatives .
c  this means that
c  t(1),...,t(n+kpm)  =  break(1) kpm times, then break(2),...,
c	 break(l) each	k  times, then, finally, break(l+1) kpm times.
c
c******  i n p u t  ******
c  break(1),...,break(l+1)  breakpoint sequence
c  l  number of intervals or pieces
c  kpm	 = k + m, order of the pp function or spline
c
c******  o u t p u t  ******
c  t(1),...,t(n+kpm)  the knot sequence.
c  n	 = l*k + m  =  dimension of  spline(k+m,t).
c
      integer l,kpm,n,	 iside,j,jj,jjj,k,ll,m
      real break(1),t(1),   xside
      common /side/ m,iside,xside(10)
      k = kpm - m
      n = l*k + m
      jj = n + kpm
      jjj = l + 1
      do 11 ll=1,kpm
	 t(jj) = break(jjj)
   11	 jj = jj - 1
      do 12 j=1,l
      jjj = jjj - 1
	 do 12 ll=1,k
	    t(jj) = break(jjj)
   12	    jj = jj - 1
      do 13 ll=1,kpm
   13	 t(ll) = break(1)
					return
      end
