c
c-----------------------------------------------------------------------
c main program: fastmain  -  fast fourier transforms
c authors:      g. d. bergland and m. t. dolan
c               bell laboratories, murray hill, new jersey 07974
c
c input:        the program calls on a random number
c               generator for input and checks dft and
c               idft with a 32-point sequence
c-----------------------------------------------------------------------
c
      dimension x(32), y(32), b(34)
c
c generate random numbers and store array in b so
c the same sequence can be used in all tests.
c note that b is dimensioned to size n+2.
c
c iw is a machine dependent write device number
c
      iw = i1mach(2)
c
      do 10 i=1,32
        x(i) = uni(0)
        b(i) = x(i)
  10  continue
      m = 5
      n = 2**m
      np1 = n + 1
      np2 = n + 2
      knt = 1
c
c test fast-fsst then ffa-ffs
c
      write (iw,9999)
  20  write (iw,9998) (b(i),i=1,n)
      if (knt.eq.1) call fast(b, n)
      if (knt.eq.2) call ffa(b, n)
      write (iw,9997) (b(i),i=1,np1,2)
      write (iw,9996) (b(i),i=2,np2,2)
      if (knt.eq.1) call fsst(b, n)
      if (knt.eq.2) call ffs(b, n)
      write (iw,9995) (b(i),i=1,n)
      knt = knt + 1
      if (knt.eq.3) go to 40
c
      write (iw,9994)
      do 30 i=1,n
        b(i) = x(i)
  30  continue
      go to 20
c
c test fft842 with real input then complex
c
  40  write (iw,9993)
      do 50 i=1,n
        b(i) = x(i)
        y(i) = 0.
  50  continue
  60  write (iw,9992) (b(i),i=1,n)
      write (iw,9991) (y(i),i=1,n)
      call fft842(0, n, b, y)
      write (iw,9997) (b(i),i=1,n)
      write (iw,9996) (y(i),i=1,n)
      call fft842(1, n, b, y)
      write (iw,9990) (b(i),i=1,n)
      write (iw,9989) (y(i),i=1,n)
      knt = knt + 1
      if (knt.eq.5) go to 80
c
      write (iw,9988)
      do 70 i=1,n
        b(i) = x(i)
        y(i) = uni(0)
  70  continue
      go to 60
c
9999  format (19h1test fast and fsst)
9998  format (20h0real input sequence/(4e17.8))
9997  format (29h0real components of transform/(4e17.8))
9996  format (29h0imag components of transform/(4e17.8))
9995  format (23h0real inverse transform/(4e17.8))
9994  format (17h1test ffa and ffs)
9993  format (37h1test fft842 with real input sequence/(4e17.8))
9992  format (34h0real components of input sequence/(4e17.8))
9991  format (34h0imag components of input sequence/(4e17.8))
9990  format (37h0real components of inverse transform/(4e17.8))
9989  format (37h0imag components of inverse transform/(4e17.8))
9988  format (40h1test fft842 with complex input sequence)
  80  stop
      end
