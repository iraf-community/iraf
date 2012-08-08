c
c-----------------------------------------------------------------------
c subroutine:  ffs
c fast fourier synthesis subroutine
c radix 8-4-2
c-----------------------------------------------------------------------
c
      subroutine ffs(b, nfft)
c
c this subroutine synthesizes the real vector b(k), where
c k=1,2,...,n. the initial fourier coefficients are placed in
c the b array of size n+2.  the dc term is in b(1) with
c b(2) equal to 0.
c the jth harmonic is stored as b(2*j+1) + i b(2*j+2).
c the n/2 harmonic is in b(n+1) with b(n+2) equal to 0.
c the subroutine is called as ffs(b,n) where n=2**m and
c b is the n term real array discussed above.
c
      dimension b(2)
      common /con1/ pii, p7, p7two, c22, s22, pi2
c
c iw is a machine dependent write device number
c
      iw = i1mach(2)
c
      pii = 4.*atan(1.)
      pi8 = pii/8.
      p7 = 1./sqrt(2.)
      p7two = 2.*p7
      c22 = cos(pi8)
      s22 = sin(pi8)
      pi2 = 2.*pii
      n = 1
      do 10 i=1,15
        m = i
        n = n*2
        if (n.eq.nfft) go to 20
  10  continue
      write (iw,9999)
9999  format (30h nfft not a power of 2 for ffs)
      stop
  20  continue
      b(2) = b(nfft+1)
      do 30 i=1,nfft
        b(i) = b(i)/float(nfft)
  30  continue
      do 40 i=4,nfft,2
        b(i) = -b(i)
  40  continue
      n8pow = m/3
c
c reorder the input fourier coefficients
c
      call ord2(m, b)
      call ord1(m, b)
c
      if (n8pow.eq.0) go to 60
c
c perform the radix 8 iterations
c
      nn = n
      do 50 it=1,n8pow
        int = n/nn
        call r8syn(int, nn, b, b(int+1), b(2*int+1), b(3*int+1),
     *      b(4*int+1), b(5*int+1), b(6*int+1), b(7*int+1), b(1),
     *      b(int+1), b(2*int+1), b(3*int+1), b(4*int+1), b(5*int+1),
     *      b(6*int+1), b(7*int+1))
        nn = nn/8
  50  continue
c
c do a radix 2 or radix 4 iteration if one is required
c
  60  if (m-n8pow*3-1) 90, 80, 70
  70  int = n/4
      call r4syn(int, b(1), b(int+1), b(2*int+1), b(3*int+1))
      go to 90
  80  int = n/2
      call r2tr(int, b(1), b(int+1))
  90  return
      end
