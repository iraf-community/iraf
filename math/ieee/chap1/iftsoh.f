c
c-----------------------------------------------------------------------
c subroutine: iftsoh
c compute idft for real, symmetric, odd harmonic, n-point sequence
c using n/4-point fft
c symmetric sequence means x(m)=x(n-m), m=1,...,n/2-1
c odd harmonic means x(2*k)=0, all k, where x(k) is the dft of x(m)
c x(m) has the property x(m)=-x(n/2-m), m=0,1,...,n/4-1,  x(n/4)=0
c note: index m is sequence index--not fortran index
c-----------------------------------------------------------------------
c
      subroutine iftsoh(x, n, y)
      dimension x(1), y(1)
c
c  x = real array which on input contains the n/4 real points of
c      the odd harmonics of the transform of the original time sequence
c      i.e. the zero valued imaginary parts are not given nor are the
c      zero valued even harmonics
c      on output x contains the first n/4 points of the original input
c      sequence (symmetrical)
c  n = true size of input
c  y = scratch array of size n/4+2
c
c
c handle n = 2 and n = 4 cases separately
c
      if (n.gt.4) go to 10
c
c for n=2, 4 assume x(1)=x0, x(2)=-x0, compute idft directly
c
      x(1) = x(1)/2.
      return
c
c code for values of n which are multiples of 8
c
  10  twopi = 8.*atan(1.0)
      no2 = n/2
      no4 = n/4
      no8 = n/8
      tpn = twopi/float(n)
c
c first compute x1=x(1) term directly
c use recursion on the sine cosine terms
c
      cosd = cos(tpn*2.)
      sind = sin(tpn*2.)
      cosi = 2.*cos(tpn)
      sini = 2.*sin(tpn)
      x1 = 0.
      do 20 i=1,no4
        x1 = x1 + x(i)*cosi
        temp = cosi*cosd - sini*sind
        sini = cosi*sind + sini*cosd
        cosi = temp
  20  continue
      x1 = x1/float(n)
c
c scramble original dft (x(k)) to give y(k)
c use recursion relation to give sin multipliers
c
      cosi = cos(tpn)
      sini = sin(tpn)
      do 30 i=1,no8
        ind = 2*i
        ind1 = no4 + 1 - i
        ak = (x(i)+x(ind1))/2.
        bk = (x(i)-x(ind1))
        y(ind-1) = ak
        y(ind) = bk*sini
        temp = cosi*cosd - sini*sind
        sini = cosi*sind + sini*cosd
        cosi = temp
  30  continue
c
c the sequence y(k) is the odd harmonics dft output
c use subroutine iftohm to obtain y(m), the inverse transform
c
      call iftohm(y, no2)
c
c form x(m) sequence from y(m) sequence
c use x1 initial condition on the recursion
c
      x(1) = y(1)
      x(2) = x1
      if (no8.eq.1) return
      do 40 i=2,no8
        ind = 2*i
        ind1 = no4 + 2 - i
        t1 = (y(i)+y(ind1))/2.
        x(ind-1) = (y(i)-y(ind1))/2.
        x(ind) = t1 + x(ind-2)
  40  continue
      return
      end
