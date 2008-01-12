      subroutine g1 (a,b,cos,sin,sig)
c     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 jun 12
c     to appear in 'solving least squares problems', prentice-hall, 1974
c
c
c     compute orthogonal rotation matrix..
c     compute.. matrix	 (c, s) so that (c, s)(a) = (sqrt(a**2+b**2))
c			 (-s,c) 	(-s,c)(b)   (	0	   )
c     compute sig = sqrt(a**2+b**2)
c	 sig is computed last to allow for the possibility that
c	 sig may be in the same location as a or b .
c
      zero=0.
      one=1.
      if (abs(a).le.abs(b)) go to 10
      xr=b/a
      yr=sqrt(one+xr**2)
      cos=sign(one/yr,a)
      sin=cos*xr
      sig=abs(a)*yr
      return
   10 if (b) 20,30,20
   20 xr=a/b
      yr=sqrt(one+xr**2)
      sin=sign(one/yr,b)
      cos=sin*xr
      sig=abs(b)*yr
      return
   30 sig=zero
      cos=zero
      sin=one
      return
      end
