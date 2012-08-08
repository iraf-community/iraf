c
c-----------------------------------------------------------------------
c subroutine:  ffa
c fast fourier analysis subroutine
c-----------------------------------------------------------------------
c
      subroutine ffa(b, nfft)
c
c this subroutine replaces the real vector b(k),  (k=1,2,...,n),
c with its finite discrete fourier transform.  the dc term is
c returned in location b(1) with b(2) set to 0.  thereafter, the
c jth harmonic is returned as a complex number stored as
c b(2*j+1) + i b(2*j+2).  note that the n/2 harmonic is returned
c in b(n+1) with b(n+2) set to 0.  hence, b must be dimensioned
c to size n+2.
c subroutine is called as ffa (b,n) where n=2**m and b is an
c n term real array.  a real-valued, radix 8  algorithm is used
c with in-place reordering and the trig functions are computed as
c needed.
c
      dimension b(2)
      common /con/ pii, p7, p7two, c22, s22, pi2
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
9999  format (30h nfft not a power of 2 for ffa)
      stop
  20  continue
      n8pow = m/3
c
c do a radix 2 or radix 4 iteration first if one is required
c
      if (m-n8pow*3-1) 50, 40, 30
  30  nn = 4
      int = n/nn
      call r4tr(int, b(1), b(int+1), b(2*int+1), b(3*int+1))
      go to 60
  40  nn = 2
      int = n/nn
      call r2tr(int, b(1), b(int+1))
      go to 60
  50  nn = 1
c
c perform radix 8 iterations
c
  60  if (n8pow) 90, 90, 70
  70  do 80 it=1,n8pow
        nn = nn*8
        int = n/nn
        call r8tr(int, nn, b(1), b(int+1), b(2*int+1), b(3*int+1),
     *      b(4*int+1), b(5*int+1), b(6*int+1), b(7*int+1), b(1),
     *      b(int+1), b(2*int+1), b(3*int+1), b(4*int+1), b(5*int+1),
     *      b(6*int+1), b(7*int+1))
  80  continue
c
c perform in-place reordering
c
  90  call ord1(m, b)
      call ord2(m, b)
      t = b(2)
      b(2) = 0.
      b(nfft+1) = t
      b(nfft+2) = 0.
      do 100 i=4,nfft,2
        b(i) = -b(i)
 100  continue
      return
      end
