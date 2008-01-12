c
c-----------------------------------------------------------------------
c subroutine: iftasm
c compute idft for real, antisymmetric, n-point sequence x(m) using
c n/2-point fft
c antisymmetric sequence means x(m)=-x(n-m), m=1,...,n/2-1
c note: index m is sequence index--not fortran index
c-----------------------------------------------------------------------
c
      subroutine iftasm(x, n, y)
      dimension x(1), y(1)
c
c x = imaginary array which on input contains the n/2+1 real points of
c     the transform of the input--i.e. the zero valued real parts
c     are not given as input
c     on output x contains the n/2 points of the time sequence
c     (antisymmetrical)
c n = true size of input
c y = scratch array of size n/2+2
c
c
c for n = 2, assume x(1)=0, x(2)=0
c
      if (n.gt.2) go to 10
      x(1) = 0
      x(2) = 0
      return
  10  twopi = 8.*atan(1.0)
c
c first compute x1=x(1) term directly
c use recursion on the sine cosine terms
c
      no2 = n/2
      no4 = n/4
      tpn = twopi/float(n)
c
c scramble original dft (x(k)) to give y(k)
c use recursion relation to give sin(tpn*i) multiplier
c
      cosi = cos(tpn)
      sini = sin(tpn)
      cosd = cosi
      sind = sini
      nind = no4 + 1
      do 20 i=2,nind
        ind = 2*i
        ind1 = no2 + 2 - i
        ak = (x(i)-x(ind1))/2.
        bk = -(x(i)+x(ind1))
        y(ind) = ak
        y(ind-1) = bk*sini
        temp = cosi*cosd - sini*sind
        sini = cosi*sind + sini*cosd
        cosi = temp
  20  continue
      y(1) = 0.
      y(2) = 0.
c
c take n/2 point idft of y
c
      call fsst(y, no2)
c
c form x sequence from y sequence
c
      x(2) = y(1)/2.
      x(1) = 0.
      if (n.eq.4) go to 40
      do 30 i=2,no4
        ind = 2*i
        ind1 = no2 + 2 - i
        x(ind-1) = (y(i)-y(ind1))/2.
        t1 = (y(i)+y(ind1))/2.
        x(ind) = t1 + x(ind-2)
  30  continue
  40  x(no2) = -y(no4+1)/2.
      return
      end
