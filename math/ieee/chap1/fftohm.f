c
c-----------------------------------------------------------------------
c subroutine: fftohm
c compute dft for real, n-point, odd harmonic sequences using an
c n/2 point fft
c odd harmonic means x(2*k)=0, all k where x(k) is the dft of x(m)
c note: index m is sequence index--not fortran index
c-----------------------------------------------------------------------
c
      subroutine fftohm(x, n)
      dimension x(1)
c
c x = real array which on input contains the first n/2 points of the
c     input
c     on output x contains the n/4 complex values of the odd
c     harmonics of the input--stored in the sequence re(x(1)),im(x(1)),
c     re(x(2)),im(x(2)),...
c ****note: x must be dimensioned to size n/2+2 for fft routine
c n = true size of x sequence
c
c first compute real(x(1)) and real(x(n/2-1)) separately
c also simultaneously multiply original sequence by sin(twopi*(m-1)/n)
c sin and cos are computed recursively
c
c
c for n = 2, assume x(1)=x0, x(2)=-x0, compute dft directly
c
      if (n.gt.2) go to 10
      x(1) = 2.*x(1)
      x(2) = 0.
      return
  10  twopi = 8.*atan(1.0)
      tpn = twopi/float(n)
c
c compute x1=real(x(1)) and x2=imaginary(x(n/2-1))
c x(n) = x(n)*4.*sin(twopi*(i-1)/n)
c
      t1 = 0.
c
c cosd and sind are multipliers for recursion for sin and cos
c cosi and sini are initial conditions for recursion for sin and cos
c
      cosd = cos(tpn*2.)
      sind = sin(tpn*2.)
      cosi = 1.
      sini = 0.
      no2 = n/2
      do 20 i=1,no2,2
        t = x(i)*cosi
        x(i) = x(i)*4.*sini
        temp = cosi*cosd - sini*sind
        sini = cosi*sind + sini*cosd
        cosi = temp
        t1 = t1 + t
  20  continue
c
c reset initial conditions (cosi,sini) for new recursion
c
      cosi = cos(tpn)
      sini = sin(tpn)
      t2 = 0.
      do 30 i=2,no2,2
        t = x(i)*cosi
        x(i) = x(i)*4.*sini
        temp = cosi*cosd - sini*sind
        sini = cosi*sind + sini*cosd
        cosi = temp
        t2 = t2 + t
  30  continue
      x1 = 2.*(t1+t2)
      x2 = 2.*(t1-t2)
c
c take n/2 point (real) fft of preprocessed sequence x
c
      call fast(x, no2)
c
c for n = 4--skip recursion and initial conditions
c
      if (n.eq.4) go to 50
c
c initial conditions for recursion
c
      x(2) = -x(1)/2.
      x(1) = x1
c
c for n = 8, skip recursion
c
      if (n.eq.8) go to 50
c
c unscramble y(k) using recursion formula
c
      nind = no2 - 2
      do 40 i=3,nind,2
        t = x(i)
        x(i) = x(i-2) + x(i+1)
        x(i+1) = x(i-1) - t
  40  continue
  50  x(no2) = x(no2+1)/2.
      x(no2-1) = x2
      return
      end
