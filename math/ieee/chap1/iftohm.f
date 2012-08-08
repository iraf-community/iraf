c
c-----------------------------------------------------------------------
c subroutine: iftohm
c compute idft for real, n-point, odd harmonic sequences using an
c n/2 point fft
c odd harmonic means x(2*k)=0, all k where x(k) is the dft of x(m)
c note: index m is sequence index--not fortran index
c-----------------------------------------------------------------------
c
      subroutine iftohm(x, n)
      dimension x(1)
c
c x = real array which on input contains the n/4 complex values of the
c     odd harmonics of the input--stored in the sequence re(x(1)),
c     im(x(1)),re(x(2)),im(x(2)),...
c     on output x contains the first n/2 points of the input
c ****note: x must be dimensioned to size n/2+2 for fft routine
c n = true size of x sequence
c
c first compute real(x(1)) and real(x(n/2-1)) separately
c also simultaneously multiply original sequence by sin(twopi*(m-1)/n)
c sin and cos are computed recursively
c
c
c for n = 2, assume x(1)=x0, x(2)=-x0, compute idft directly
c
      if (n.gt.2) go to 10
      x(1) = 0.5*x(1)
      x(2) = -x(1)
      return
  10  twopi = 8.*atan(1.0)
      tpn = twopi/float(n)
      no2 = n/2
      no4 = n/4
      nind = no2
c
c solve for x(0)=x0 directly
c
      x0 = 0.
      do 20 i=1,no2,2
        x0 = x0 + 2.*x(i)
  20  continue
      x0 = x0/float(n)
c
c form y(k)=j*(x(2k+1)-x(2k-1))
c overwrite x array with y sequence
c
      xpr = x(1)
      xpi = x(2)
      x(1) = -2.*x(2)
      x(2) = 0.
      if (no4.eq.1) go to 40
      do 30 i=3,nind,2
        ti = x(i) - xpr
        tr = -x(i+1) + xpi
        xpr = x(i)
        xpi = x(i+1)
        x(i) = tr
        x(i+1) = ti
  30  continue
  40  x(no2+1) = 2.*xpi
      x(no2+2) = 0.
c
c take n/2 point (real) ifft of preprocessed sequence x
c
      call fsst(x, no2)
c
c solve for x(m) by dividing by 4*sin(twopi*m/n) for m=1,2,...,n/2-1
c for m=0 substitute precomputed value x0
c
      cosi = 4.
      sini = 0.
      cosd = cos(tpn)
      sind = sin(tpn)
      do 50 i=2,no2
        temp = cosi*cosd - sini*sind
        sini = cosi*sind + sini*cosd
        cosi = temp
        x(i) = x(i)/sini
  50  continue
      x(1) = x0
      return
      end
