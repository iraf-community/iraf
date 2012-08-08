c
c-----------------------------------------------------------------------
c subroutine:  fast
c replaces the real vector b(k), for k=1,2,...,n,
c with its finite discrete fourier transform
c-----------------------------------------------------------------------
c
      subroutine fast(b, n)
c
c the dc term is returned in location b(1) with b(2) set to 0.
c thereafter the jth harmonic is returned as a complex
c number stored as  b(2*j+1) + i b(2*j+2).
c the n/2 harmonic is returned in b(n+1) with b(n+2) set to 0.
c hence, b must be dimensioned to size n+2.
c the subroutine is called as  fast(b,n) where n=2**m and
c b is the real array described above.
c
      dimension b(2)
      common /cons/ pii, p7, p7two, c22, s22, pi2
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
9999  format (33h n is not a power of two for fast)
      stop
  20  n4pow = m/2
c
c do a radix 2 iteration first if one is required.
c
      if (m-n4pow*2) 40, 40, 30
  30  nn = 2
      int = n/nn
      call fr2tr(int, b(1), b(int+1))
      go to 50
  40  nn = 1
c
c perform radix 4 iterations.
c
  50  if (n4pow.eq.0) go to 70
      do 60 it=1,n4pow
        nn = nn*4
        int = n/nn
        call fr4tr(int, nn, b(1), b(int+1), b(2*int+1), b(3*int+1),
     *      b(1), b(int+1), b(2*int+1), b(3*int+1))
  60  continue
c
c perform in-place reordering.
c
  70  call ford1(m, b)
      call ford2(m, b)
      t = b(2)
      b(2) = 0.
      b(n+1) = t
      b(n+2) = 0.
      do 80 it=4,n,2
        b(it) = -b(it)
  80  continue
      return
      end
