c
c-----------------------------------------------------------------------
c subroutine: fftsym
c compute dft for real, symmetric, n-point sequence x(m) using
c n/2-point fft
c symmetric sequence means x(m)=x(n-m), m=1,...,n/2-1
c note: index m is sequence index--not fortran index
c-----------------------------------------------------------------------
c
      subroutine fftsym(x, n, y)
      dimension x(1), y(1)
c
c x = real array which on input contains the n/2+1 points of the
c     input sequence (symmetrical)
c     on output x contains the n/2+1 real points of the transform of
c     the input--i.e. the zero valued imaginary parts are not returned
c n = true size of input
c y = scratch array of size n/2+2
c
c
c for n = 2, compute dft directly
c
      if (n.gt.2) go to 10
      t = x(1) + x(2)
      x(2) = x(1) - x(2)
      x(1) = t
      return
  10  twopi = 8.*atan(1.0)
c
c first compute b0 term, where b0=sum of odd values of x(m)
c
      no2 = n/2
      no4 = n/4
      nind = no2 + 1
      b0 = 0.
      do 20 i=2,nind,2
        b0 = b0 + x(i)
  20  continue
      b0 = b0*2.
c
c for n = 4 skip recursion loop
c
      if (n.eq.4) go to 40
c
c form new sequence, y(m)=x(2*m)+(x(2*m+1)-x(2*m-1))
c
      do 30 i=2,no4
        ind = 2*i
        t1 = x(ind) - x(ind-2)
        y(i) = x(ind-1) + t1
        ind1 = no2 + 2 - i
        y(ind1) = x(ind-1) - t1
  30  continue
  40  y(1) = x(1)
      y(no4+1) = x(no2+1)
c
c take n/2 point (real) fft of y
c
      call fast(y, no2)
c
c form original dft by unscrambling y(k)
c use recursion to give sin(tpn*i) multiplier
c
      tpn = twopi/float(n)
      cosi = 2.*cos(tpn)
      sini = 2.*sin(tpn)
      cosd = cosi/2.
      sind = sini/2.
      nind = no4 + 1
      do 50 i=2,nind
        ind = 2*i
        bk = y(ind)/sini
        ak = y(ind-1)
        x(i) = ak + bk
        nind1 = n/2 + 2 - i
        x(nind1) = ak - bk
        temp = cosi*cosd - sini*sind
        sini = cosi*sind + sini*cosd
        cosi = temp
  50  continue
      x(1) = b0 + y(1)
      x(no2+1) = y(1) - b0
      return
      end
