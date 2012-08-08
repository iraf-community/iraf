c
c-----------------------------------------------------------------------
c subroutine: fourea
c performs cooley-tukey fast fourier transform
c-----------------------------------------------------------------------
c
      subroutine fourea(data, n, isi)
c
c the cooley-tukey fast fourier transform in ansi fortran
c
c data is a one-dimensional complex array whose length, n, is a
c power of two.  isi is +1 for an inverse transform and -1 for a
c forward transform.  transform values are returned in the input
c array, replacing the input.
c transform(j)=sum(data(i)*w**((i-1)*(j-1))), where i and j run
c from 1 to n and w = exp (isi*2*pi*sqrt(-1)/n).  program also
c computes inverse transform, for which the defining expression
c is invtr(j)=(1/n)*sum(data(i)*w**((i-1)*(j-1))).
c running time is proportional to n*log2(n), rather than to the
c classical n**2.
c after program by brenner, june 1967. this is a very short version
c of the fft and is intended mainly for demonstration. programs
c are available in this collection which run faster and are not
c restricted to powers of 2 or to one-dimensional arrays.
c see -- ieee trans audio (june 1967), special issue on fft.
c
      complex data(1)
      complex temp, w
      ioutd = i1mach(2)
c
c check for power of two up to 15
c
      nn = 1
      do 10 i=1,15
        m = i
        nn = nn*2
        if (nn.eq.n) go to 20
  10  continue
      write (ioutd,9999)
9999  format (30h n not a power of 2 for fourea)
      stop
  20  continue
c
      pi = 4.*atan(1.)
      fn = n
c
c this section puts data in bit-reversed order
c
      j = 1
      do 80 i=1,n
c
c at this point, i and j are a bit reversed pair (except for the
c displacement of +1)
c
        if (i-j) 30, 40, 40
c
c exchange data(i) with data(j) if i.lt.j.
c
  30    temp = data(j)
        data(j) = data(i)
        data(i) = temp
c
c implement j=j+1, bit-reversed counter
c
  40    m = n/2
  50    if (j-m) 70, 70, 60
  60    j = j - m
        m = (m+1)/2
        go to 50
  70    j = j + m
  80  continue
c
c now compute the butterflies
c
      mmax = 1
  90  if (mmax-n) 100, 130, 130
 100  istep = 2*mmax
      do 120 m=1,mmax
        theta = pi*float(isi*(m-1))/float(mmax)
        w = cmplx(cos(theta),sin(theta))
        do 110 i=m,n,istep
          j = i + mmax
          temp = w*data(j)
          data(j) = data(i) - temp
          data(i) = data(i) + temp
 110    continue
 120  continue
      mmax = istep
      go to 90
 130  if (isi) 160, 140, 140
c
c for inv trans -- isi=1 -- multiply output by 1/n
c
 140  do 150 i=1,n
        data(i) = data(i)/fn
 150  continue
 160  return
      end
