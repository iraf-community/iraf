c
c-----------------------------------------------------------------------
c subroutine: iftsym
c compute idft for real, symmetric, n-point sequence x(m) using
c n/2-point fft
c symmetric sequence means x(m)=x(n-m), m=1,...,n/2-1
c note: index m is sequence index--not fortran index
c-----------------------------------------------------------------------
c
      subroutine iftsym(x, n, y)
      dimension x(1), y(1)
c
c x = real array which on input contains the n/2+1 real points of the
c     transform of the input--i.e. the zero valued imaginary parts
c     are not given as input
c     on output x contains the n/2+1 points of the time sequence
c     (symmetrical)
c n = true size of input
c y = scratch array of size n/2+2
c
c
c for n = 2, compute idft directly
c
      if (n.gt.2) go to 10
      t = (x(1)+x(2))/2.
      x(2) = (x(1)-x(2))/2.
      x(1) = t
      return
  10  twopi = 8.*atan(1.0)
c
c first compute x1=x(1) term directly
c use recursion on the sine cosine terms
c
      no2 = n/2
      no4 = n/4
      tpn = twopi/float(n)
      cosd = cos(tpn)
      sind = sin(tpn)
      cosi = 2.
      sini = 0.
      x1 = x(1) - x(no2+1)
      do 20 i=2,no2
        temp = cosi*cosd - sini*sind
        sini = cosi*sind + sini*cosd
        cosi = temp
        x1 = x1 + x(i)*cosi
  20  continue
      x1 = x1/float(n)
c
c scramble original dft (x(k)) to give y(k)
c use recursion relation to generate sin(tpn*i) multiplier
c
      cosi = cos(tpn)
      sini = sin(tpn)
      cosd = cosi
      sind = sini
      y(1) = (x(1)+x(no2+1))/2.
      y(2) = 0.
      nind = no4 + 1
      do 30 i=2,nind
        ind = 2*i
        nind1 = no2 + 2 - i
        ak = (x(i)+x(nind1))/2.
        bk = (x(i)-x(nind1))
        y(ind-1) = ak
        y(ind) = bk*sini
        temp = cosi*cosd - sini*sind
        sini = cosi*sind + sini*cosd
        cosi = temp
  30  continue
c
c take n/2 point idft of y
c
      call fsst(y, no2)
c
c form x sequence from y sequence
c
      x(1) = y(1)
      x(2) = x1
      if (n.eq.4) go to 50
      do 40 i=2,no4
        ind = 2*i
        ind1 = no2 + 2 - i
        x(ind-1) = (y(i)+y(ind1))/2.
        t1 = (y(i)-y(ind1))/2.
        x(ind) = t1 + x(ind-2)
  40  continue
  50  x(no2+1) = y(no4+1)
      return
      end
