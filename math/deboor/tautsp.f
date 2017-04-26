      subroutine tautsp ( tau, gtau, ntau, gamma, s,
     *			  break, coef, l, k, iflag )
c  from  * a practical guide to splines *  by c. de boor
constructs cubic spline interpolant to given data
c	  tau(i), gtau(i), i=1,...,ntau.
c  if  gamma .gt. 0., additional knots are introduced where needed to
c  make the interpolant more flexible locally. this avoids extraneous
c  inflection points typical of cubic spline interpolation at knots to
c  rapidly changing data.
c
c  parameters
c	     input
c  tau	    sequence of data points. must be strictly increasing.
c  gtau     corresponding sequence of function values.
c  ntau     number of data points. must be at least  4 .
c  gamma    indicates whether additional flexibility is desired.
c	   = 0., no additional knots
c	   in (0.,3.), under certain conditions on the given data at
c		 points i-1, i, i+1, and i+2, a knot is added in the
c		 i-th interval, i=2,...,ntau-2. see description of meth-
c		 od below. the interpolant gets rounded with increasing
c		 gamma. a value of  2.5  for gamma is typical.
c	   in (3.,6.), same , except that knots might also be added in
c		 intervals in which an inflection point would be permit-
c		 ted.  a value of  5.5	for gamma is typical.
c	     output
c  break, coef, l, k  give the pp-representation of the interpolant.
c	   specifically, for break(i) .le. x .le. break(i+1), the
c	 interpolant has the form
c  f(x) = coef(1,i) +dx(coef(2,i) +(dx/2)(coef(3,i) +(dx/3)coef(4,i)))
c	 with  dx = x - break(i) and i=1,...,l .
c  iflag   = 1, ok
c	   = 2, input was incorrect. a printout specifying the mistake
c	     was made.
c	     workspace
c  s	 is required, of size (ntau,6). the individual columns of this
c	 array contain the following quantities mentioned in the write-
c	 up and below.
c     s(.,1) = dtau = tau(.+1) - tau
c     s(.,2) = diag = diagonal in linear system
c     s(.,3) = u = upper diagonal in linear system
c     s(.,4) = r = right side for linear system (initially)
c	     = fsecnd = solution of linear system , namely the second
c			derivatives of interpolant at  tau
c     s(.,5) = z = indicator of additional knots
c     s(.,6) = 1/hsecnd(1,x) with x = z or = 1-z. see below.
c
c  ------  m e t h o d	------
c  on the i-th interval, (tau(i), tau(i+1)), the interpolant is of the
c  form
c  (*)	f(u(x)) = a + b*u + c*h(u,z) + d*h(1-u,1-z) ,
c  with  u = u(x) = (x - tau(i))/dtau(i). here,
c	z = z(i) = addg(i+1)/(addg(i) + addg(i+1))
c  (= .5, in case the denominator vanishes). with
c	addg(j) = abs(ddg(j)), ddg(j) = dg(j+1) - dg(j),
c	dg(j) = divdif(j) = (gtau(j+1) - gtau(j))/dtau(j)
c  and
c	h(u,z) = alpha*u**3 + (1 - alpha)*(max(((u-zeta)/(1-zeta)),0)**3
c  with
c	alpha(z) = (1-gamma/3)/zeta
c	zeta(z) = 1 - gamma*min((1 - z), 1/3)
c  thus, for 1/3 .le. z .le. 2/3,  f  is just a cubic polynomial on
c  the interval i. otherwise, it has one additional knot, at
c	  tau(i) + zeta*dtau(i) .
c  as  z  approaches  1, h(.,z) has an increasingly sharp bend	near 1,
c  thus allowing  f  to turn rapidly near the additional knot.
c     in terms of f(j) = gtau(j) and
c	fsecnd(j) = 2.derivative of  f	at  tau(j),
c  the coefficients for (*) are given as
c	a = f(i) - d
c	b = (f(i+1) - f(i)) - (c - d)
c	c = fsecnd(i+1)*dtau(i)**2/hsecnd(1,z)
c	d = fsecnd(i)*dtau(i)**2/hsecnd(1,1-z)
c  hence can be computed once fsecnd(i),i=1,...,ntau, is fixed.
c   f  is automatically continuous and has a continuous second derivat-
c  ive (except when z = 0 or 1 for some i). we determine fscnd(.) from
c  the requirement that also the first derivative of  f  be contin-
c  uous. in addition, we require that the third derivative be continuous
c  across  tau(2) and across  tau(ntau-1) . this leads to a strictly
c  diagonally dominant tridiagonal linear system for the fsecnd(i)
c  which we solve by gauss elimination without pivoting.
c
      integer iflag,k,l,ntau,	i,method,ntaum1
      real break(1),coef(4,1),gamma,gtau(ntau),s(ntau,6),tau(ntau)
     *	  ,alpha,c,d,del,denom,divdif,entry,entry3,factor,factr2,gam
     *	  ,onemg3,onemzt,ratio,sixth,temp,x,z,zeta,zt2
      real alph
      alph(x) = amin1(1.,onemg3/x)
c
c  there must be at least  4  interpolation points.
      if (ntau .ge. 4)			go to 5
c     print 600,ntau
c 600 format(8h ntau = ,i4,20h. should be .ge. 4 .)
					go to 999
c
construct delta tau and first and second (divided) differences of data
c
    5 ntaum1 = ntau - 1
      do 6 i=1,ntaum1
	 s(i,1) = tau(i+1) - tau(i)
	 if (s(i,1) .gt. 0.)		go to 6
c	 print 610,i,tau(i),tau(i+1)
c 610	 format(7h point ,i3,13h and the next,2e15.6,15h are disordered)
					go to 999
    6	 s(i+1,4) = (gtau(i+1)-gtau(i))/s(i,1)
      do 7 i=2,ntaum1
    7	 s(i,4) = s(i+1,4) - s(i,4)
c
construct system of equations for second derivatives at  tau. at each
c  interior data point, there is one continuity equation, at the first
c  and the last interior data point there is an additional one for a
c  total of  ntau  equations in  ntau  unknowns.
c
      i = 2
      s(2,2) = s(1,1)/3.
      sixth = 1./6.
      method = 2
      gam = gamma
      if (gam .le. 0.)	 method = 1
      if ( gam .le. 3.) 		go to 9
      method = 3
      gam = gam - 3.
    9 onemg3 = 1. - gam/3.
c		  ------ loop over i ------
   10 continue
c	   construct z(i) and zeta(i)
      z = .5
					go to (19,11,12),method
   11 if (s(i,4)*s(i+1,4) .lt. 0.)	go to 19
   12 temp = abs(s(i+1,4))
      denom = abs(s(i,4)) + temp
      if (denom .eq. 0.)		go to 19
      z = temp/denom
      if (abs(z - .5) .le. sixth)  z = .5
   19 s(i,5) = z
c   ******set up part of the i-th equation which depends on
c	  the i-th interval
      if (z - .5)			21,22,23
   21 zeta = gam*z
      onemzt = 1. - zeta
      zt2 = zeta**2
      alpha = alph(onemzt)
      factor = zeta/(alpha*(zt2-1.) + 1.)
      s(i,6) = zeta*factor/6.
      s(i,2) = s(i,2) + s(i,1)*((1.-alpha*onemzt)*factor/2. - s(i,6))
c     if z = 0 and the previous z = 1, then d(i) = 0. since then
c     also u(i-1) = l(i+1) = 0, its value does not matter. reset
c     d(i) = 1 to insure nonzero pivot in elimination.
      if (s(i,2) .le. 0.) s(i,2) = 1.
      s(i,3) = s(i,1)/6.
					go to 25
   22 s(i,2) = s(i,2) + s(i,1)/3.
      s(i,3) = s(i,1)/6.
					go to 25
   23 onemzt = gam*(1. - z)
      zeta = 1. - onemzt
      alpha = alph(zeta)
      factor = onemzt/(1. - alpha*zeta*(1.+onemzt))
      s(i,6) = onemzt*factor/6.
      s(i,2) = s(i,2) + s(i,1)/3.
      s(i,3) = s(i,6)*s(i,1)
   25 if (i .gt. 2)			go to 30
      s(1,5) = .5
c  ******the first two equations enforce continuity of the first and of
c	 the third derivative across tau(2).
      s(1,2) = s(1,1)/6.
      s(1,3) = s(2,2)
      entry3 = s(2,3)
      if (z - .5)			26,27,28
   26 factr2 = zeta*(alpha*(zt2-1.) + 1.)/(alpha*(zeta*zt2-1.)+1.)
      ratio = factr2*s(2,1)/s(1,2)
      s(2,2) = factr2*s(2,1) + s(1,1)
      s(2,3) = -factr2*s(1,1)
					go to 29
   27 ratio = s(2,1)/s(1,2)
      s(2,2) = s(2,1) + s(1,1)
      s(2,3) = -s(1,1)
					go to 29
   28 ratio = s(2,1)/s(1,2)
      s(2,2) = s(2,1) + s(1,1)
      s(2,3) = -s(1,1)*6.*alpha*s(2,6)
c	at this point, the first two equations read
c	       diag(1)*x1 + u(1)*x2 + entry3*x3 = r(2)
c	-ratio*diag(1)*x1 + diag(2)*x2 + u(2)*x3 = 0.
c	eliminate first unknown from second equation
   29 s(2,2) = ratio*s(1,3) + s(2,2)
      s(2,3) = ratio*entry3 + s(2,3)
      s(1,4) = s(2,4)
      s(2,4) = ratio*s(1,4)
					go to 35
   30 continue
c  ******the i-th equation enforces continuity of the first derivative
c	 across tau(i). it has been set up in statements 35 up to 40
c	 and 21 up to 25 and reads now
c	  -ratio*diag(i-1)*xi-1 + diag(i)*xi + u(i)*xi+1 = r(i) .
c	 eliminate (i-1)st unknown from this equation
      s(i,2) = ratio*s(i-1,3) + s(i,2)
      s(i,4) = ratio*s(i-1,4) + s(i,4)
c
c  ******set up the part of the next equation which depends on the
c	 i-th interval.
   35 if (z - .5)			36,37,38
   36 ratio = -s(i,6)*s(i,1)/s(i,2)
      s(i+1,2) = s(i,1)/3.
					go to 40
   37 ratio = -(s(i,1)/6.)/s(i,2)
      s(i+1,2) = s(i,1)/3.
					go to 40
   38 ratio = -(s(i,1)/6.)/s(i,2)
      s(i+1,2) = s(i,1)*((1.-zeta*alpha)*factor/2. - s(i,6))
c	  ------  end of i loop ------
   40 i = i+1
      if (i .lt. ntaum1)		go to 10
      s(i,5) = .5
c
c	 ------  last two equations  ------
c  the last two equations enforce continuity of third derivative and
c  of first derivative across  tau(ntau-1).
      entry = ratio*s(i-1,3) + s(i,2) + s(i,1)/3.
      s(i+1,2) = s(i,1)/6.
      s(i+1,4) = ratio*s(i-1,4) + s(i,4)
      if (z - .5)			41,42,43
   41 ratio = s(i,1)*6.*s(i-1,6)*alpha/s(i-1,2)
      s(i,2) = ratio*s(i-1,3) + s(i,1) + s(i-1,1)
      s(i,3) = -s(i-1,1)
					go to 45
   42 ratio = s(i,1)/s(i-1,2)
      s(i,2) = ratio*s(i-1,3) + s(i,1) + s(i-1,1)
      s(i,3) = -s(i-1,1)
					go to 45
   43 factr2 = onemzt*(alpha*(onemzt**2-1.)+1.)/
     *		     (alpha*(onemzt**3-1.)+1.)
      ratio = factr2*s(i,1)/s(i-1,2)
      s(i,2) = ratio*s(i-1,3) + factr2*s(i-1,1) + s(i,1)
      s(i,3) = -factr2*s(i-1,1)
c     at this point, the last two equations read
c	      diag(i)*xi + u(i)*xi+1 = r(i)
c      -ratio*diag(i)*xi + diag(i+1)*xi+1 = r(i+1)
c     eliminate xi from last equation
   45 s(i,4) = ratio*s(i-1,4)
      ratio = -entry/s(i,2)
      s(i+1,2) = ratio*s(i,3) + s(i+1,2)
      s(i+1,4) = ratio*s(i,4) + s(i+1,4)
c
c	 ------ back substitution ------
c
      s(ntau,4) = s(ntau,4)/s(ntau,2)
   50	 s(i,4) = (s(i,4) - s(i,3)*s(i+1,4))/s(i,2)
	 i = i - 1
	 if (i .gt. 1)			go to 50
      s(1,4) = (s(1,4)-s(1,3)*s(2,4)-entry3*s(3,4))/s(1,2)
c
c	 ------ construct polynomial pieces ------
c
      break(1) = tau(1)
      l = 1
      do 70 i=1,ntaum1
	 coef(1,l) = gtau(i)
	 coef(3,l) = s(i,4)
	 divdif = (gtau(i+1)-gtau(i))/s(i,1)
	 z = s(i,5)
	 if (z - .5)			61,62,63
   61	 if (z .eq. 0.) 		go to 65
	 zeta = gam*z
	 onemzt = 1. - zeta
	 c = s(i+1,4)/6.
	 d = s(i,4)*s(i,6)
	 l = l + 1
	 del = zeta*s(i,1)
	 break(l) = tau(i) + del
	 zt2 = zeta**2
	 alpha = alph(onemzt)
	 factor = onemzt**2*alpha
	 coef(1,l) = gtau(i) + divdif*del
     *		   + s(i,1)**2*(d*onemzt*(factor-1.)+c*zeta*(zt2-1.))
	 coef(2,l) = divdif + s(i,1)*(d*(1.-3.*factor)+c*(3.*zt2-1.))
	 coef(3,l) = 6.*(d*alpha*onemzt + c*zeta)
	 coef(4,l) = 6.*(c - d*alpha)/s(i,1)
	 coef(4,l-1) = coef(4,l) - 6.*d*(1.-alpha)/(del*zt2)
	 coef(2,l-1) = coef(2,l) - del*(coef(3,l) -(del/2.)*coef(4,l-1))
					go to 68
   62	 coef(2,l) = divdif - s(i,1)*(2.*s(i,4) + s(i+1,4))/6.
	 coef(4,l) = (s(i+1,4)-s(i,4))/s(i,1)
					go to 68
   63	 onemzt = gam*(1. - z)
	 if (onemzt .eq. 0.)		go to 65
	 zeta = 1. - onemzt
	 alpha = alph(zeta)
	 c = s(i+1,4)*s(i,6)
	 d = s(i,4)/6.
	 del = zeta*s(i,1)
	 break(l+1) = tau(i) + del
	 coef(2,l) = divdif - s(i,1)*(2.*d + c)
	 coef(4,l) = 6.*(c*alpha - d)/s(i,1)
	 l = l + 1
	 coef(4,l) = coef(4,l-1) + 6.*(1.-alpha)*c/(s(i,1)*onemzt**3)
	 coef(3,l) = coef(3,l-1) + del*coef(4,l-1)
	 coef(2,l) = coef(2,l-1)+del*(coef(3,l-1)+(del/2.)*coef(4,l-1))
	 coef(1,l) = coef(1,l-1)+del*(coef(2,l-1)+(del/2.)*(coef(3,l-1)
     *			+(del/3.)*coef(4,l-1)))
					go to 68
   65	 coef(2,l) = divdif
	 coef(3,l) = 0.
	 coef(4,l) = 0.
   68	 l = l + 1
   70	 break(l) = tau(i+1)
      l = l - 1
      k = 4
      iflag = 1
					return
  999 iflag = 2
					return
      end
