      subroutine ldp (g,mdg,m,n,h,x,xnorm,w,index,ier)
c     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1974 mar 1
c     to appear in 'solving least squares problems', prentice-hall, 1974
c
c	     **********  least distance programming  **********
c
      integer index(m)
      dimension g(mdg,n), h(m), x(n), w(1)
      zero=0.
      one=1.
      if (n.le.0) go to 120
	  do 10 j=1,n
   10	  x(j)=zero
      xnorm=zero
      if (m.le.0) go to 110
c
c     the declared dimension of w() must be at least (n+1)*(m+2)+2*m.
c
c      first (n+1)*m locs of w()   =  matrix e for problem nnls.
c	next	 n+1 locs of w()   =  vector f for problem nnls.
c	next	 n+1 locs of w()   =  vector z for problem nnls.
c	next	   m locs of w()   =  vector y for problem nnls.
c	next	   m locs of w()   =  vector wdual for problem nnls.
c     copy g**t into first n rows and m columns of e.
c     copy h**t into row n+1 of e.
c
      iw=0
	  do 30 j=1,m
	      do 20 i=1,n
	      iw=iw+1
   20	      w(iw)=g(j,i)
	  iw=iw+1
   30	  w(iw)=h(j)
      if=iw+1
c				 store n zeros followed by a one into f.
	  do 40 i=1,n
	  iw=iw+1
   40	  w(iw)=zero
      w(iw+1)=one
c
      np1=n+1
      iz=iw+2
      iy=iz+np1
      iwdual=iy+m
c
      call nnls (w,np1,np1,m,w(if),w(iy),rnorm,w(iwdual),w(iz),index,
     *		 ier)
c		       use the following return if unsuccessful in nnls.
      if (ier.ne.0) return
      if (rnorm) 130,130,50
   50 fac=one
      iw=iy-1
	  do 60 i=1,m
	  iw=iw+1
c				here we are using the solution vector y.
   60	  fac=fac-h(i)*w(iw)
c
      if (diff(one+fac,one)) 130,130,70
   70 fac=one/fac
	  do 90 j=1,n
	  iw=iy-1
	      do 80 i=1,m
	      iw=iw+1
c				here we are using the solution vector y.
   80	      x(j)=x(j)+g(i,j)*w(iw)
   90	  x(j)=x(j)*fac
	  do 100 j=1,n
  100	  xnorm=xnorm+x(j)**2
      xnorm=sqrt(xnorm)
c			      successful return.
  110 ier=0
      return
c			      error return.	  n .le. 0.
  120 ier=2
      return
c			      returning with constraints not compatible.
  130 ier=4
      return
      end
