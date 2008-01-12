c
c-----------------------------------------------------------------------
c subroutine: fftasm
c compute dft for real, antisymmetric, n-point sequence x(m) using
c n/2-point fft
c antisymmetric sequence means x(m)=-x(n-m), m=1,...,n/2-1
c note: index m is sequence index--not fortran index
c-----------------------------------------------------------------------
c
      subroutine fftasm(x, n, y)
      dimension x(1), y(1)
c
c x = real array which on input contains the n/2 points of the
c     input sequence (asymmetrical)
c     on output x contains the n/2+1 imaginary points of the transform
c     of the input--i.e. the zero valued real parts are not returned
c n = true size of input
c y = scratch array of size n/2+2
c
c
c for n = 2, assume x(1)=0, x(2)=0, compute dft directly
c
      if (n.eq.2) go to 30
      twopi = 8.*atan(1.0)
c
c form new sequence, y(m)=x(2*m)+(x(2*m+1)-x(2*m-1))
c
      no2 = n/2
      no4 = n/4
      do 10 i=2,no4
        ind = 2*i
        t1 = x(ind) - x(ind-2)
        y(i) = x(ind-1) + t1
        ind1 = no2 + 2 - i
        y(ind1) = -x(ind-1) + t1
  10  continue
      y(1) = 2.*x(2)
      y(no4+1) = -2.*x(no2)
c
c take n/2 point (real) fft of y
c
      call fast(y, no2)
c
c form original dft by unscrambling y(k)
c use recursion relation to generate sin(tpn*i) multiplier
c
      tpn = twopi/float(n)
      cosi = 2.*cos(tpn)
      sini = 2.*sin(tpn)
      cosd = cosi/2.
      sind = sini/2.
      nind = no4 + 1
      do 20 i=2,nind
        ind = 2*i
        bk = y(ind-1)/sini
        ak = y(ind)
        x(i) = ak - bk
        ind1 = no2 + 2 - i
        x(ind1) = -ak - bk
        temp = cosi*cosd - sini*sind
        sini = cosi*sind + sini*cosd
        cosi = temp
  20  continue
  30  x(1) = 0.
      x(no2+1) = 0.
      return
      end
