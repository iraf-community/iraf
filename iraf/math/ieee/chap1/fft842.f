c
c-----------------------------------------------------------------------
c subroutine:  fft842
c fast fourier transform for n=2**m
c complex input
c-----------------------------------------------------------------------
c
      subroutine fft842(in, n, x, y)
c
c this program replaces the vector z=x+iy by its  finite
c discrete, complex fourier transform if in=0.  the inverse transform
c is calculated for in=1.  it performs as many base
c 8 iterations as possible and then finishes with a base 4 iteration
c or a base 2 iteration if needed.
c
c the subroutine is called as subroutine fft842 (in,n,x,y).
c the integer n (a power of 2), the n real location array x, and
c the n real location array y must be supplied to the subroutine.
c
      dimension x(2), y(2), l(15)
      common /con2/ pi2, p7
      equivalence (l15,l(1)), (l14,l(2)), (l13,l(3)), (l12,l(4)),
     *    (l11,l(5)), (l10,l(6)), (l9,l(7)), (l8,l(8)), (l7,l(9)),
     *    (l6,l(10)), (l5,l(11)), (l4,l(12)), (l3,l(13)), (l2,l(14)),
     *    (l1,l(15))
c
c
c iw is a machine dependent write device number
c
      iw = i1mach(2)
c
      pi2 = 8.*atan(1.)
      p7 = 1./sqrt(2.)
      do 10 i=1,15
        m = i
        nt = 2**i
        if (n.eq.nt) go to 20
  10  continue
      write (iw,9999)
9999  format (35h n is not a power of two for fft842)
      stop
  20  n2pow = m
      nthpo = n
      fn = nthpo
      if (in.eq.1) go to 40
      do 30 i=1,nthpo
        y(i) = -y(i)
  30  continue
  40  n8pow = n2pow/3
      if (n8pow.eq.0) go to 60
c
c radix 8 passes,if any.
c
      do 50 ipass=1,n8pow
        nxtlt = 2**(n2pow-3*ipass)
        lengt = 8*nxtlt
        call r8tx(nxtlt, nthpo, lengt, x(1), x(nxtlt+1), x(2*nxtlt+1),
     *      x(3*nxtlt+1), x(4*nxtlt+1), x(5*nxtlt+1), x(6*nxtlt+1),
     *      x(7*nxtlt+1), y(1), y(nxtlt+1), y(2*nxtlt+1), y(3*nxtlt+1),
     *      y(4*nxtlt+1), y(5*nxtlt+1), y(6*nxtlt+1), y(7*nxtlt+1))
  50  continue
c
c is there a four factor left
c
  60  if (n2pow-3*n8pow-1) 90, 70, 80
c
c go through the base 2 iteration
c
c
  70  call r2tx(nthpo, x(1), x(2), y(1), y(2))
      go to 90
c
c go through the base 4 iteration
c
  80  call r4tx(nthpo, x(1), x(2), x(3), x(4), y(1), y(2), y(3), y(4))
c
  90  do 110 j=1,15
        l(j) = 1
        if (j-n2pow) 100, 100, 110
 100    l(j) = 2**(n2pow+1-j)
 110  continue
      ij = 1
      do 130 j1=1,l1
      do 130 j2=j1,l2,l1
      do 130 j3=j2,l3,l2
      do 130 j4=j3,l4,l3
      do 130 j5=j4,l5,l4
      do 130 j6=j5,l6,l5
      do 130 j7=j6,l7,l6
      do 130 j8=j7,l8,l7
      do 130 j9=j8,l9,l8
      do 130 j10=j9,l10,l9
      do 130 j11=j10,l11,l10
      do 130 j12=j11,l12,l11
      do 130 j13=j12,l13,l12
      do 130 j14=j13,l14,l13
      do 130 ji=j14,l15,l14
        if (ij-ji) 120, 130, 130
 120    r = x(ij)
        x(ij) = x(ji)
        x(ji) = r
        fi = y(ij)
        y(ij) = y(ji)
        y(ji) = fi
 130    ij = ij + 1
      if (in.eq.1) go to 150
      do 140 i=1,nthpo
        y(i) = -y(i)
 140  continue
      go to 170
 150  do 160 i=1,nthpo
        x(i) = x(i)/fn
        y(i) = y(i)/fn
 160  continue
 170  return
      end
