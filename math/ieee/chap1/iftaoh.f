c
c-----------------------------------------------------------------------
c subroutine: iftaoh
c compute idft for real, antisymmetric, odd harmonic, n-point sequence
c using n/4-point fft
c antisymmetric sequence means x(m)=-x(n-m), m=1,...,n/2-1
c odd harmonic means x(2*k)=0, all k, where x(k) is the dft of x(m)
c x(m)has the property x(m)=x(n/2-m), m=0,1,...,n/4-1,  x(0)=0
c note: index m is sequence index--not fortran index
c-----------------------------------------------------------------------
c
      subroutine iftaoh(x, n, y)
      dimension x(1), y(1)
c
c  x = real array which on input contains the n/4 imaginary points
c      of the odd harmonics of the transform of the original time
c      sequence--i.e. the zero valued real parts are not input nor
c      are the zero-valued even harmonics
c      on output x contains the first (n/4+1) points of the original
c      time sequence (antisymmetrical)
c  n = true size of input
c  y = scratch array of size n/4+2
c
c
c handle n = 2 and n = 4 cases separately
c
      if (n.gt.4) go to 20
      if (n.eq.4) go to 10
c
c for n=2  assume x(1)=0, x(2)=0, compute idft directly
c
      x(1) = 0.
      return
c
c for n=4, assume x(1)=x(3)=0, x(2)=-x(4)=x0, compute idft directly
c
  10  x(2) = -x(1)/2.
      x(1) = 0.
      return
c
c code for values of n which are multiples of 8
c
  20  twopi = 8.*atan(1.0)
      no2 = n/2
      no4 = n/4
      no8 = n/8
      tpn = twopi/float(n)
c
c scramble original dft (x(k)) to give y(k)
c use recursion to give sin multipliers
c
      cosi = cos(tpn)
      sini = sin(tpn)
      cosd = cos(tpn*2.)
      sind = sin(tpn*2.)
      do 30 i=1,no8
        ind = 2*i
        ind1 = no4 + 1 - i
        ak = (x(i)-x(ind1))/2.
        bk = -(x(i)+x(ind1))
        y(ind) = ak
        y(ind-1) = bk*sini
        temp = cosi*cosd - sini*sind
        sini = cosi*sind + sini*cosd
        cosi = temp
  30  continue
c
c the sequence y(k) is an odd harmonic sequence
c use subroutine iftohm to give y(m)
c
      call iftohm(y, no2)
c
c form x sequence from y sequence
c
      x(2) = y(1)/2.
      x(1) = 0.
      if (n.eq.8) return
      do 40 i=2,no8
        ind = 2*i
        ind1 = no4 + 2 - i
        x(ind-1) = (y(i)+y(ind1))/2.
        t1 = (y(i)-y(ind1))/2.
        x(ind) = t1 + x(ind-2)
  40  continue
      x(no4+1) = y(no8+1)
      return
      end
