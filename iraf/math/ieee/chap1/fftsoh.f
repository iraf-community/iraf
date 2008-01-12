c
c-----------------------------------------------------------------------
c subroutine: fftsoh
c compute dft for real, symmetric, odd harmonic, n-point sequence
c using n/4-point fft
c symmetric sequence means x(m)=x(n-m), m=1,...,n/2-1
c odd harmonic means x(2*k)=0, all k, where x(k) is the dft of x(m)
c x(m) has the property x(m)=-x(n/2-m), m=0,1,...,n/4-1,  x(n/4)=0
c note: index m is sequence index--not fortran index
c-----------------------------------------------------------------------
c
      subroutine fftsoh(x, n, y)
      dimension x(1), y(1)
c
c  x = real array which on input contains the n/4 points of the
c      input sequence (symmetrical)
c      on output x contains  the n/4 real points of the odd harmonics
c      of the transform of the input--i.e. the zero valued imaginary
c      parts are not given nor are the zero-valued even harmonics
c  n = true size of input
c  y = scratch array of size n/4+2
c
c
c handle n = 2 and n = 4 cases separately
c
      if (n.gt.4) go to 20
      if (n.eq.4) go to 10
c
c for n=2, assume x(1)=x0, x(2)=-x0, compute dft directly
c
      x(1) = 2.*x(1)
      return
c
c n = 4 case, compute dft directly
c
  10  x(1) = 2.*x(1)
      return
  20  twopi = 8.*atan(1.0)
c
c form new sequence, y(m)=x(2*m)+(x(2*m+1)-x(2*m-1))
c
      no2 = n/2
      no4 = n/4
      no8 = n/8
      if (no8.eq.1) go to 40
      do 30 i=2,no8
        ind = 2*i
        t1 = x(ind) - x(ind-2)
        y(i) = x(ind-1) + t1
        ind1 = n/4 + 2 - i
        y(ind1) = -x(ind-1) + t1
  30  continue
  40  y(1) = x(1)
      y(no8+1) = -2.*x(no4)
c
c the sequence y (n/4 points) has only odd harmonics
c call subroutine fftohm to exploit odd harmonics
c
      call fftohm(y, no2)
c
c form original dft from complex odd harmonics of y(k)
c by unscrambling y(k)
c
      tpn = twopi/float(n)
      cosi = 2.*cos(tpn)
      sini = 2.*sin(tpn)
      cosd = cos(tpn*2.)
      sind = sin(tpn*2.)
      do 50 i=1,no8
        ind = 2*i
        bk = y(ind)/sini
        temp = cosi*cosd - sini*sind
        sini = cosi*sind + sini*cosd
        cosi = temp
        ak = y(ind-1)
        x(i) = ak + bk
        ind1 = n/4 + 1 - i
        x(ind1) = ak - bk
  50  continue
      return
      end
