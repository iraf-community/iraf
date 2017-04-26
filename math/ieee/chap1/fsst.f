c
c-----------------------------------------------------------------------
c subroutine:  fsst
c fourier synthesis subroutine
c-----------------------------------------------------------------------
c
      subroutine fsst(b, n)
c
c this subroutine synthesizes the real vector b(k), for
c k=1,2,...,n, from the fourier coefficients stored in the
c b array of size n+2.  the dc term is in b(1) with b(2) equal
c to  0.  the jth harmonic is stored as b(2*j+1) + i b(2*j+2).
c the n/2 harmonic is in b(n+1) with b(n+2) equal to 0.
c the subroutine is called as fsst(b,n) where n=2**m and
c b is the real array discussed above.
c
      dimension b(2)
      common /const/ pii, p7, p7two, c22, s22, pi2
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
      do 10 i=1,15
        m = i
        nt = 2**i
        if (n.eq.nt) go to 20
  10  continue
      write (iw,9999)
9999  format (33h n is not a power of two for fsst)
      stop
  20  b(2) = b(n+1)
      do 30 i=4,n,2
        b(i) = -b(i)
  30  continue
c
c scale the input by n
c
      do 40 i=1,n
        b(i) = b(i)/float(n)
  40  continue
      n4pow = m/2
c
c scramble the inputs
c
      call ford2(m, b)
      call ford1(m, b)
c
      if (n4pow.eq.0) go to 60
      nn = 4*n
      do 50 it=1,n4pow
        nn = nn/4
        int = n/nn
        call fr4syn(int, nn, b(1), b(int+1), b(2*int+1), b(3*int+1),
     *      b(1), b(int+1), b(2*int+1), b(3*int+1))
  50  continue
c
c do a radix 2 iteration if one is required
c
  60  if (m-n4pow*2) 80, 80, 70
  70  int = n/2
      call fr2tr(int, b(1), b(int+1))
  80  return
      end
