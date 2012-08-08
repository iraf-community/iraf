c     prog6
c     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 jun 15
c     to appear in 'solving least squares problems', prentice-hall, 1974
c	   demonstrate the use of the subroutine   ldp	for least
c     distance programming by solving the constrained line data fitting
c     problem of chapter 23.
c
      dimension e(4,2), f(4), g(3,2), h(3), g2(3,2), h2(3), x(2), z(2),
     1w(4)
      dimension wldp(21), s(6), t(4)
      integer index(3)
c
      write (6,110)
      mde=4
      mdgh=3
c
      me=4
      mg=3
      n=2
c		 define the least squares and constraint matrices.
      t(1)=0.25
      t(2)=0.5
      t(3)=0.5
      t(4)=0.8
c
      w(1)=0.5
      w(2)=0.6
      w(3)=0.7
      w(4)=1.2
c
	  do 10 i=1,me
	  e(i,1)=t(i)
	  e(i,2)=1.
   10	  f(i)=w(i)
c
      g(1,1)=1.
      g(1,2)=0.
      g(2,1)=0.
      g(2,2)=1.
      g(3,1)=-1.
      g(3,2)=-1.
c
      h(1)=0.
      h(2)=0.
      h(3)=-1.
c
c     compute the singular value decomposition of the matrix, e.
c
      call svdrs (e,mde,me,n,f,1,1,s)
c
      write (6,120) ((e(i,j),j=1,n),i=1,n)
      write (6,130) f,(s(j),j=1,n)
c
c     generally rank determination and levenberg-marquardt
c     stabilization could be inserted here.
c
c	 define the constraint matrix for the z coordinate system.
	  do 30 i=1,mg
	      do 30 j=1,n
	      sm=0.
		  do 20 l=1,n
   20		  sm=sm+g(i,l)*e(l,j)
   30	      g2(i,j)=sm/s(j)
c	  define constraint rt side for the z coordinate system.
	  do 50 i=1,mg
	  sm=0.
	      do 40 j=1,n
   40	      sm=sm+g2(i,j)*f(j)
   50	  h2(i)=h(i)-sm
c
      write (6,140) ((g2(i,j),j=1,n),i=1,mg)
      write (6,150) h2
c
c			 solve the constrained problem in z-coordinates.
c
      call ldp (g2,mdgh,mg,n,h2,z,znorm,wldp,index,mode)
c
      write (6,200) mode,znorm
      write (6,160) z
c
c		     transform back from z-coordinates to x-coordinates.
	  do 60 j=1,n
   60	  z(j)=(z(j)+f(j))/s(j)
	  do 80 i=1,n
	  sm=0.
	      do 70 j=1,n
   70	      sm=sm+e(i,j)*z(j)
   80	  x(i)=sm
      res=znorm**2
      np1=n+1
	  do 90 i=np1,me
   90	  res=res+f(i)**2
      res=sqrt(res)
c				     compute the residuals.
	  do 100 i=1,me
  100	  f(i)=w(i)-x(1)*t(i)-x(2)
      write (6,170) (x(j),j=1,n)
      write (6,180) (i,f(i),i=1,me)
      write (6,190) res
      stop
c
  110 format (46h0prog6..  example of constrained curve fitting,26h usin
     1g the subroutine ldp.,/43h0related intermediate quantities are giv
     2en.)
  120 format (10h0v =	   ,2f10.5/(10x,2f10.5))
  130 format (10h0f tilda =,4f10.5/10h0s =	,2f10.5)
  140 format (10h0g tilda =,2f10.5/(10x,2f10.5))
  150 format (10h0h tilda =,3f10.5)
  160 format (10h0z =	   ,2f10.5)
  170 format (52h0the coeficients of the fitted line f(t)=x(1)*t+x(2),12
     1h are x(1) = ,f10.5,14h	and x(2) = ,f10.5)
  180 format (30h0the consecutive residuals are/1x,4(i10,f10.5))
  190 format (23h0the residuals norm is ,f10.5)
  200 format (18h0mode (from ldp) =,i3,2x,7hznorm =,f10.5)
c
      end
