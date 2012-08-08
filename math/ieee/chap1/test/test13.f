c
c-----------------------------------------------------------------------
c main program: test program for fft subroutines
c author:      l r rabiner
c      bell laboratories, murray hill, new jersey, 07974
c input:       randomly chosen sequences to test fft subroutines
c      for sequences with special properties
c      n is the fft length (n must be a power of 2)
c          2<= n <= 4096
c-----------------------------------------------------------------------
c
      dimension x(4098), y(4098)
c
c  define i/0 device codes
c  input: input to this program is user-interactive
c        that is - a question is written on the user
c        terminal (iout1) ad the user types in the answer.
c
c output: all output is written on the standard
c         output unit (iout2).
c
      ind = i1mach(1)
      iout1 = i1mach(4)
      iout2 = i1mach(2)
c
c read in analysis size for fft
c
  10  write (iout1,9999)
9999  format (30h fft size(2.le.n.le.4096)(i4)=)
      read (ind,9998) n
9998  format (i4)
      if (n.eq.0) stop
      do 20 i=1,12
        itest = 2**i
        if (n.eq.itest) go to 30
  20  continue
      write (iout1,9997)
9997  format (45h n is not a power of 2 in the range 2 to 4096)
      go to 10
  30  write (iout2,9996) n
9996  format (11h testing n=, i5, 17h random sequences)
      write (iout2,9992)
      np2 = n + 2
      no2 = n/2
      no2p1 = no2 + 1
      no4 = n/4
      no4p1 = no4 + 1
c
c create symmetrical sequence of size n
c
      do 40 i=2,no2
        x(i) = uni(0) - 0.5
        ind1 = np2 - i
        x(ind1) = x(i)
  40  continue
      x(1) = uni(0) - 0.5
      x(no2p1) = uni(0) - 0.5
      do 50 i=1,no2p1
        y(i) = x(i)
  50  continue
      write (iout2,9995)
9995  format (28h original symmetric sequence)
      write (iout2,9993) (x(i),i=1,n)
      write (iout2,9992)
c
c compute true fft of n point sequence
c
      call fast(x, n)
      write (iout2,9994) n
9994  format (1h , i4, 32h point fft of symmetric sequence)
      write (iout2,9993) (x(i),i=1,np2)
9993  format (1h , 5e13.5)
      write (iout2,9992)
9992  format (1h /1h )
c
c use subroutine fftsym to obtain dft from no2 point fft
c
      do 60 i=1,no2p1
        x(i) = y(i)
  60  continue
      call fftsym(x, n, y)
      write (iout2,9991)
9991  format (17h output of fftsym)
      write (iout2,9993) (x(i),i=1,no2p1)
      write (iout2,9992)
c
c use subroutine iftsym to obtain original sequence from no2 point dft
c
      call iftsym(x, n, y)
      write (iout2,9990)
9990  format (17h output of iftsym)
      write (iout2,9993) (x(i),i=1,no2p1)
      write (iout2,9992)
c
c create antisymmetric n point sequence
c
      do 70 i=2,no2
        x(i) = uni(0) - 0.5
        ind1 = np2 - i
        x(ind1) = -x(i)
  70  continue
      x(1) = 0.
      x(no2p1) = 0.
      do 80 i=1,no2p1
        y(i) = x(i)
  80  continue
      write (iout2,9989)
9989  format (32h original antisymmetric sequence)
      write (iout2,9993) (x(i),i=1,n)
      write (iout2,9992)
c
c obtain n point dft of antisymmetric sequence
c
      call fast(x, n)
      write (iout2,9988) n
9988  format (1h , i4, 36h point fft of antisymmetric sequence)
      write (iout2,9993) (x(i),i=1,np2)
      write (iout2,9992)
c
c use subroutine fftasm to obtain dft from no2 point fft
c
      do 90 i=1,no2
        x(i) = y(i)
  90  continue
      call fftasm(x, n, y)
      write (iout2,9987)
9987  format (17h output of fftasm)
      write (iout2,9993) (x(i),i=1,no2p1)
      write (iout2,9992)
c
c use subroutine iftasm to obtain original sequence from no2 point dft
c
      call iftasm(x, n, y)
      write (iout2,9986)
9986  format (17h output of iftasm)
      write (iout2,9993) (x(i),i=1,no2)
      write (iout2,9992)
c
c create sequence with only odd harmonics--begin in frequency domain
c
      do 100 i=1,np2,2
        x(i) = 0.
        x(i+1) = 0.
        if (mod(i,4).eq.1) go to 100
        x(i) = uni(0) - 0.5
        x(i+1) = uni(0) - 0.5
        if (n.eq.2) x(i+1) = 0.
 100  continue
      write (iout2,9985) n
9985  format (1h , i4, 35h point fft of odd harmonic sequence)
      write (iout2,9993) (x(i),i=1,np2)
      write (iout2,9992)
c
c transform back to time sequence
c
      call fsst(x, n)
      write (iout2,9984)
9984  format (31h original odd harmonic sequence)
      write (iout2,9993) (x(i),i=1,n)
      write (iout2,9992)
c
c use subroutine fftohm to obtain dft from no2 point fft
c
      call fftohm(x, n)
      write (iout2,9983)
9983  format (17h output of fftohm)
      write (iout2,9993) (x(i),i=1,no2)
      write (iout2,9992)
c
c use subroutine iftohm to obtain original sequence from no2 point dft
c
      call iftohm(x, n)
      write (iout2,9982)
9982  format (17h output of iftohm)
      write (iout2,9993) (x(i),i=1,no2)
      write (iout2,9992)
c
c create sequence with only real valued odd harmonics
c
      do 110 i=1,np2,2
        x(i) = 0.
        x(i+1) = 0.
        if (mod(i,4).eq.1) go to 110
        x(i) = uni(0) - 0.5
 110  continue
      write (iout2,9981) n
9981  format (1h , i4, 45h point fft of odd harmonic, symmetric sequenc,
     *    1he)
      write (iout2,9993) (x(i),i=1,np2)
      write (iout2,9992)
c
c transform back to time sequence
c
      call fsst(x, n)
      write (iout2,9980)
9980  format (42h original odd harmonic, symmetric sequence)
      write (iout2,9993) (x(i),i=1,n)
      write (iout2,9992)
c
c use subroutine fftsoh to obtain dft from no4 point fft
c
      call fftsoh(x, n, y)
      write (iout2,9979)
9979  format (17h output of fftsoh)
      write (iout2,9993) (x(i),i=1,no4)
      write (iout2,9992)
c
c use subroutine iftsoh to obtain original sequence from no4 point dft
c
      call iftsoh(x, n, y)
      write (iout2,9978)
9978  format (17h output of iftsoh)
      write (iout2,9993) (x(i),i=1,no4)
      write (iout2,9992)
c
c create sequence with only imaginary valued odd harmonics--begin
c in frequency domain
c
      do 120 i=1,np2,2
        x(i) = 0.
        x(i+1) = 0.
        if (mod(i,4).eq.1) go to 120
        x(i+1) = uni(0) - 0.5
 120  continue
      write (iout2,9977) n
9977  format (1h , i4, 41h point fft of odd harmonic, antisymmetric,
     *    9h sequence)
      write (iout2,9993) (x(i),i=1,np2)
      write (iout2,9992)
c
c transform back to time sequence
c
      call fsst(x, n)
      write (iout2,9976)
9976  format (46h original odd harmonic, antisymmetric sequence)
      write (iout2,9993) (x(i),i=1,n)
      write (iout2,9992)
c
c use subroutine fftaoh to obtain dft from no4 point fft
c
      call fftaoh(x, n, y)
      write (iout2,9975)
9975  format (17h output of fftaoh)
      write (iout2,9993) (x(i),i=1,no4)
      write (iout2,9992)
c
c use subroutine iftaoh to obtain original sequence from n/4 point dft
c
      call iftaoh(x, n, y)
      write (iout2,9974)
9974  format (17h output of iftaoh)
      write (iout2,9993) (x(i),i=1,no4p1)
      write (iout2,9992)
c
c begin a new page
c
      write (iout2,9973)
9973  format (1h1)
      go to 10
      end
