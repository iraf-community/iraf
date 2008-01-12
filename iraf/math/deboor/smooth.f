      real function smooth ( x, y, dy, npoint, s, v, a )
c  from  * a practical guide to splines *  by c. de boor
calls  setupq, chol1d
c
c  constructs the cubic smoothing spline  f  to given data  (x(i),y(i)),
c  i=1,...,npoint, which has as small a second derivative as possible
c  while
c  s(f) = sum( ((y(i)-f(x(i)))/dy(i))**2 , i=1,...,npoint ) .le. s .
c
c******  i n p u t  ******
c  x(1),...,x(npoint)	data abscissae,  a s s u m e d	to be strictly
c	 increasing .
c  y(1),...,y(npoint)	  corresponding data ordinates .
c  dy(1),...,dy(npoint)     estimate of uncertainty in data,  a s s u m-
c	 e d  to be positive .
c  npoint.....number of data points,  a s s u m e d  .gt. 1
c  s.....upper bound on the discrete weighted mean square distance of
c	 the approximation  f  from the data .
c
c******  w o r k  a r r a y s  *****
c  v.....of size (npoint,7)
c  a.....of size (npoint,4)
c
c*****	o u t p u t  *****
c  a(.,1).....contains the sequence of smoothed ordinates .
c  a(i,j) = d**(j-1)f(x(i)), j=2,3,4, i=1,...,npoint-1 ,  i.e., the
c	 first three derivatives of the smoothing spline  f  at the
c	 left end of each of the data intervals .
c     w a r n i n g . . .   a  would have to be transposed before it
c	 could be used in  ppvalu .
c
c******  m e t h o d  ******
c     the matrices  q-transp*d	and  q-transp*d**2*q  are constructed in
c   s e t u p q  from  x  and  dy , as is the vector  qty = q-transp*y .
c  then, for given  p , the vector  u  is determined in  c h o l 1 d  as
c  the solution of the linear system
c		(6(1-p)q-transp*d**2*q + p*r)u	= qty  .
c  from  u , the smoothing spline  f  (for this choice of smoothing par-
c  ameter  p ) is obtained in the sense that
c			 f(x(.))  =  y - 6(1-p)d**2*q*u        and
c		   (d**2)f(x(.))  =  6*p*u			.
c     the smoothing parameter  p  is found (if possible) so that
c		 sf(p)	=  s ,
c  with  sf(p) = s(f) , where  f  is the smoothing spline as it depends
c  on  p .  if	s = 0, then p = 1 . if	sf(0) .le. s , then p = 0 .
c  otherwise, the secant method is used to locate an appropriate  p  in
c  the open interval  (0,1) . specifically,
c		 p(0) = 0,  p(1) = (s - sf(0))/dsf
c  with  dsf = -24*u-transp*r*u  a good approximation to  d(sf(0)) = dsf
c  + 60*(d*q*u)-transp*(d*q*u) , and  u  as obtained for  p = 0 .
c  after that, for n=1,2,...  until sf(p(n)) .le. 1.01*s, do....
c     determine  p(n+1)  as the point at which the secant to  sf  at the
c     points  p(n)  and  p(n-1)  takes on the value  s .
c     if  p(n+1) .ge. 1 , choose instead  p(n+1)  as the point at which
c     the parabola  sf(p(n))*((1-.)/(1-p(n)))**2  takes on the value  s.
c     note that, in exact arithmetic, always  p(n+1) .lt. p(n) , hence
c     sf(p(n+1)) .lt. sf(p(n)) . therefore, also stop the iteration,
c     with final  p = 1 , in case  sf(p(n+1)) .ge. sf(p(n)) .
c
      integer npoint,	i,npm1
      real a(npoint,4),dy(npoint),s,v(npoint,7),x(npoint),y(npoint)
     *	   ,change,p,prevsf,prevp,sfp,sixp,six1mp,utru
      call setupq(x,dy,y,npoint,v,a(1,4))
      if (s .gt. 0.)			go to 20
   10 p = 1.
      call chol1d(p,v,a(1,4),npoint,1,a(1,3),a(1,1))
      sfp = 0.
					go to 60
   20 p = 0.
      call chol1d(p,v,a(1,4),npoint,1,a(1,3),a(1,1))
      sfp = 0.
      do 21 i=1,npoint
   21	 sfp = sfp + (a(i,1)*dy(i))**2
      sfp = sfp*36.
      if (sfp .le. s)			go to 60
      prevp = 0.
      prevsf = sfp
      utru = 0.
      do 25 i=2,npoint
   25	 utru = utru + v(i-1,4)*(a(i-1,3)*(a(i-1,3)+a(i,3))+a(i,3)**2)
      p = (sfp-s)/(24.*utru)
c  secant iteration for the determination of p starts here.
   30 call chol1d(p,v,a(1,4),npoint,1,a(1,3),a(1,1))
      sfp = 0.
      do 35 i=1,npoint
   35	 sfp = sfp+ (a(i,1)*dy(i))**2
      sfp = sfp*36.*(1.-p)**2
      if (sfp .le. 1.01*s)		go to 60
      if (sfp .ge. prevsf)		go to 10
      change = (p-prevp)/(sfp-prevsf)*(sfp-s)
      prevp = p
      p = p - change
      prevsf = sfp
      if (p .lt. 1.)			go to 30
      p = 1. - sqrt(s/prevsf)*(1.-prevp)
					go to 30
correct value of p has been found.
compute pol.coefficients from  q*u (in a(.,1)).
   60 smooth = sfp
      six1mp = 6.*(1.-p)
      do 61 i=1,npoint
   61	 a(i,1) = y(i) - six1mp*dy(i)**2*a(i,1)
      sixp = 6.*p
      do 62 i=1,npoint
   62	 a(i,3) = a(i,3)*sixp
      npm1 = npoint - 1
      do 63 i=1,npm1
	 a(i,4) = (a(i+1,3)-a(i,3))/v(i,4)
   63	 a(i,2) = (a(i+1,1)-a(i,1))/v(i,4)
     *			    - (a(i,3)+a(i,4)/3.*v(i,4))/2.*v(i,4)
					return
      end
