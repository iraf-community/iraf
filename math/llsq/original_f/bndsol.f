      subroutine bndsol (mode,g,mdg,nb,ip,ir,x,n,rnorm)
c     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 jun 12
c     to appear in 'solving least squares problems', prentice-hall, 1974
c	   sequential solution of a banded least squares problem..
c	   solution phase.   for the accumulation phase use bndacc.
c
c     mode = 1	   solve r*x=y	 where r and y are in the g( ) array
c		   and x will be stored in the x( ) array.
c	     2	   solve (r**t)*x=y   where r is in g( ),
c		   y is initially in x( ), and x replaces y in x( ),
c	     3	   solve r*x=y	 where r is in g( ).
c		   y is initially in x( ), and x replaces y in x( ).
c
c     the second subscript of g( ) must be dimensioned at least
c     nb+1 in the calling program.
      dimension g(mdg,1),x(n)
      zero=0.
c
      rnorm=zero
      go to (10,90,50), mode
c				    ********************* mode = 1
c				    algc step 26
   10	   do 20 j=1,n
   20	   x(j)=g(j,nb+1)
      rsq=zero
      np1=n+1
      irm1=ir-1
      if (np1.gt.irm1) go to 40
	   do 30 j=np1,irm1
   30	   rsq=rsq+g(j,nb+1)**2
      rnorm=sqrt(rsq)
   40 continue
c				    ********************* mode = 3
c				    alg. step 27
   50	   do 80 ii=1,n
	   i=n+1-ii
c				    alg. step 28
	   s=zero
	   l=max0(0,i-ip)
c				    alg. step 29
	   if (i.eq.n) go to 70
c				    alg. step 30
	   ie=min0(n+1-i,nb)
		do 60 j=2,ie
		jg=j+l
		ix=i-1+j
   60		s=s+g(i,jg)*x(ix)
c				    alg. step 31
   70	   if (g(i,l+1)) 80,130,80
   80	   x(i)=(x(i)-s)/g(i,l+1)
c				    alg. step 32
      return
c				    ********************* mode = 2
   90	   do 120 j=1,n
	   s=zero
	   if (j.eq.1) go to 110
	   i1=max0(1,j-nb+1)
	   i2=j-1
		do 100 i=i1,i2
		l=j-i+1+max0(0,i-ip)
  100		s=s+x(i)*g(i,l)
  110	   l=max0(0,j-ip)
	   if (g(j,l+1)) 120,130,120
  120	   x(j)=(x(j)-s)/g(j,l+1)
      return
c
  130 write (6,140) mode,i,j,l
      stop
  140 format (30h0zero diagonal term in bndsol.,12h mode,i,j,l=,4i6)
      end
