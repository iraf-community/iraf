c
c----------------------------------------------------------------------
c  function:  i1mach
c  this routine is from the port mathematical subroutine library
c  it is described in the bell laboratories computing science
c  technical report #47 by p.a. fox, a.d. hall and n.l. schryer
c---------------------------------------------------------------------
c
      integer function i1mach(i)
c
c  i/o unit numbers.
c
c    i1mach( 1) = the standard input unit.
c
c    i1mach( 2) = the standard output unit.
c
c    i1mach( 3) = the standard punch unit.
c
c    i1mach( 4) = the standard error message unit.
c
c  words.
c
c    i1mach( 5) = the number of bits per integer storage unit.
c
c    i1mach( 6) = the number of characters per integer storage unit.
c
c  integers.
c
c    assume integers are represented in the s-digit, base-a form
c
c               sign ( x(s-1)*a**(s-1) + ... + x(1)*a + x(0) )
c
c               where 0 .le. x(i) .lt. a for i=0,...,s-1.
c
c    i1mach( 7) = a, the base.
c
c    i1mach( 8) = s, the number of base-a digits.
c
c    i1mach( 9) = a**s - 1, the largest magnitude.
c
c  floating-point numbers.
c
c    assume floating-point numbers are represented in the t-digit,
c    base-b form
c
c               sign (b**e)*( (x(1)/b) + ... + (x(t)/b**t) )
c
c               where 0 .le. x(i) .lt. b for i=1,...,t,
c               0 .lt. x(1), and emin .le. e .le. emax.
c
c    i1mach(10) = b, the base.
c
c  single-precision
c
c    i1mach(11) = t, the number of base-b digits.
c
c    i1mach(12) = emin, the smallest exponent e.
c
c    i1mach(13) = emax, the largest exponent e.
c
c  double-precision
c
c    i1mach(14) = t, the number of base-b digits.
c
c    i1mach(15) = emin, the smallest exponent e.
c
c    i1mach(16) = emax, the largest exponent e.
c
c  to alter this function for a particular environment,
c  the desired set of data statements should be activated by
c  removing the c from column 1.  also, the values of
c  i1mach(1) - i1mach(4) should be checked for consistency
c  with the local operating system.
c
      integer imach(16),output
c
      equivalence (imach(4),output)
c
c     machine constants for the burroughs 1700 system.
c
c     data imach( 1) /    7 /
c     data imach( 2) /    2 /
c     data imach( 3) /    2 /
c     data imach( 4) /    2 /
c     data imach( 5) /   36 /
c     data imach( 6) /    4 /
c     data imach( 7) /    2 /
c     data imach( 8) /   33 /
c     data imach( 9) / z1ffffffff /
c     data imach(10) /    2 /
c     data imach(11) /   24 /
c     data imach(12) / -256 /
c     data imach(13) /  255 /
c     data imach(14) /   60 /
c     data imach(15) / -256 /
c     data imach(16) /  255 /
c
c     machine constants for the burroughs 5700 system.
c
c     data imach( 1) /   5 /
c     data imach( 2) /   6 /
c     data imach( 3) /   7 /
c     data imach( 4) /   6 /
c     data imach( 5) /  48 /
c     data imach( 6) /   6 /
c     data imach( 7) /   2 /
c     data imach( 8) /  39 /
c     data imach( 9) / o0007777777777777 /
c     data imach(10) /   8 /
c     data imach(11) /  13 /
c     data imach(12) / -50 /
c     data imach(13) /  76 /
c     data imach(14) /  26 /
c     data imach(15) / -50 /
c     data imach(16) /  76 /
c
c     machine constants for the burroughs 6700/7700 systems.
c
c     data imach( 1) /   5 /
c     data imach( 2) /   6 /
c     data imach( 3) /   7 /
c     data imach( 4) /   6 /
c     data imach( 5) /  48 /
c     data imach( 6) /   6 /
c     data imach( 7) /   2 /
c     data imach( 8) /  39 /
c     data imach( 9) / o0007777777777777 /
c     data imach(10) /   8 /
c     data imach(11) /  13 /
c     data imach(12) / -50 /
c     data imach(13) /  76 /
c     data imach(14) /  26 /
c     data imach(15) / -32754 /
c     data imach(16) /  32780 /
c
c     machine constants for the cdc 6000/7000 series.
c
c     data imach( 1) /    5 /
c     data imach( 2) /    6 /
c     data imach( 3) /    7 /
c     data imach( 4) /    6 /
c     data imach( 5) /   60 /
c     data imach( 6) /   10 /
c     data imach( 7) /    2 /
c     data imach( 8) /   48 /
c     data imach( 9) / 00007777777777777777b /
c     data imach(10) /    2 /
c     data imach(11) /   48 /
c     data imach(12) / -974 /
c     data imach(13) / 1070 /
c     data imach(14) /   96 /
c     data imach(15) / -927 /
c     data imach(16) / 1070 /
c
c     machine constants for the cray 1
c
c     data imach( 1) /   100 /
c     data imach( 2) /   101 /
c     data imach( 3) /   102 /
c     data imach( 4) /   101 /
c     data imach( 5) /    64 /
c     data imach( 6) /     8 /
c     data imach( 7) /     2 /
c     data imach( 8) /    63 /
c     data imach( 9) /  777777777777777777777b /
c     data imach(10) /     2 /
c     data imach(11) /    47 /
c     data imach(12) / -8192 /
c     data imach(13) /  8190 /
c     data imach(14) /    95 /
c     data imach(15) / -8192 /
c     data imach(16) /  8190 /
c
c     machine constants for the data general eclipse s/200
c
c     data imach( 1) /   11 /
c     data imach( 2) /   12 /
c     data imach( 3) /    8 /
c     data imach( 4) /   10 /
c     data imach( 5) /   16 /
c     data imach( 6) /    2 /
c     data imach( 7) /    2 /
c     data imach( 8) /   15 /
c     data imach( 9) /32767 /
c     data imach(10) /   16 /
c     data imach(11) /    6 /
c     data imach(12) /  -64 /
c     data imach(13) /   63 /
c     data imach(14) /   14 /
c     data imach(15) /  -64 /
c     data imach(16) /   63 /
c
c     machine constants for the harris slash 6 and slash 7
c
c     data imach( 1) /       5 /
c     data imach( 2) /       6 /
c     data imach( 3) /       0 /
c     data imach( 4) /       6 /
c     data imach( 5) /      24 /
c     data imach( 6) /       3 /
c     data imach( 7) /       2 /
c     data imach( 8) /      23 /
c     data imach( 9) / 8388607 /
c     data imach(10) /       2 /
c     data imach(11) /      23 /
c     data imach(12) /    -127 /
c     data imach(13) /     127 /
c     data imach(14) /      38 /
c     data imach(15) /    -127 /
c     data imach(16) /     127 /
c
c     machine constants for the honeywell 600/6000 series.
c
c     data imach( 1) /    5 /
c     data imach( 2) /    6 /
c     data imach( 3) /   43 /
c     data imach( 4) /    6 /
c     data imach( 5) /   36 /
c     data imach( 6) /    6 /
c     data imach( 7) /    2 /
c     data imach( 8) /   35 /
c     data imach( 9) / o377777777777 /
c     data imach(10) /    2 /
c     data imach(11) /   27 /
c     data imach(12) / -127 /
c     data imach(13) /  127 /
c     data imach(14) /   63 /
c     data imach(15) / -127 /
c     data imach(16) /  127 /
c
c     machine constants for the ibm 360/370 series,
c     the xerox sigma 5/7/9 and the sel systems 85/86.
c
c     data imach( 1) /   5 /
c     data imach( 2) /   6 /
c     data imach( 3) /   7 /
c     data imach( 4) /   6 /
c     data imach( 5) /  32 /
c     data imach( 6) /   4 /
c     data imach( 7) /   2 /
c     data imach( 8) /  31 /
c     data imach( 9) / z7fffffff /
c     data imach(10) /  16 /
c     data imach(11) /   6 /
c     data imach(12) / -64 /
c     data imach(13) /  63 /
c     data imach(14) /  14 /
c     data imach(15) / -64 /
c     data imach(16) /  63 /
c
c     machine constants for the pdp-10 (ka processor).
c
c     data imach( 1) /    5 /
c     data imach( 2) /    6 /
c     data imach( 3) /    5 /
c     data imach( 4) /    6 /
c     data imach( 5) /   36 /
c     data imach( 6) /    5 /
c     data imach( 7) /    2 /
c     data imach( 8) /   35 /
c     data imach( 9) / "377777777777 /
c     data imach(10) /    2 /
c     data imach(11) /   27 /
c     data imach(12) / -128 /
c     data imach(13) /  127 /
c     data imach(14) /   54 /
c     data imach(15) / -101 /
c     data imach(16) /  127 /
c
c     machine constants for the pdp-10 (ki processor).
c
c     data imach( 1) /    5 /
c     data imach( 2) /    6 /
c     data imach( 3) /    5 /
c     data imach( 4) /    6 /
c     data imach( 5) /   36 /
c     data imach( 6) /    5 /
c     data imach( 7) /    2 /
c     data imach( 8) /   35 /
c     data imach( 9) / "377777777777 /
c     data imach(10) /    2 /
c     data imach(11) /   27 /
c     data imach(12) / -128 /
c     data imach(13) /  127 /
c     data imach(14) /   62 /
c     data imach(15) / -128 /
c     data imach(16) /  127 /
c
c     machine constants for pdp-11 fortran supporting
c     32-bit integer arithmetic.
c
      data imach( 1) /    5 /
      data imach( 2) /    6 /
      data imach( 3) /    6 /
      data imach( 4) /    0 /
      data imach( 5) /   32 /
      data imach( 6) /    4 /
      data imach( 7) /    2 /
      data imach( 8) /   31 /
      data imach( 9) / 2147483647 /
      data imach(10) /    2 /
      data imach(11) /   24 /
      data imach(12) / -127 /
      data imach(13) /  127 /
      data imach(14) /   56 /
      data imach(15) / -127 /
      data imach(16) /  127 /
c
c     machine constants for pdp-11 fortran supporting
c     16-bit integer arithmetic.
c
c     data imach( 1) /    5 /
c     data imach( 2) /    6 /
c     data imach( 3) /    5 /
c     data imach( 4) /    6 /
c     data imach( 5) /   16 /
c     data imach( 6) /    2 /
c     data imach( 7) /    2 /
c     data imach( 8) /   15 /
c     data imach( 9) / 32767 /
c     data imach(10) /    2 /
c     data imach(11) /   24 /
c     data imach(12) / -127 /
c     data imach(13) /  127 /
c     data imach(14) /   56 /
c     data imach(15) / -127 /
c     data imach(16) /  127 /
c
c     machine constants for the univac 1100 series.
c
c     note that the punch unit, i1mach(3), has been set to 7
c     which is appropriate for the univac-for system.
c     if you have the univac-ftn system, set it to 1.
c
c     data imach( 1) /    5 /
c     data imach( 2) /    6 /
c     data imach( 3) /    7 /
c     data imach( 4) /    6 /
c     data imach( 5) /   36 /
c     data imach( 6) /    6 /
c     data imach( 7) /    2 /
c     data imach( 8) /   35 /
c     data imach( 9) / o377777777777 /
c     data imach(10) /    2 /
c     data imach(11) /   27 /
c     data imach(12) / -128 /
c     data imach(13) /  127 /
c     data imach(14) /   60 /
c     data imach(15) /-1024 /
c     data imach(16) / 1023 /
c
c     machine constants for the vax-11 with
c     fortran iv-plus compiler
c
c     data imach( 1) /    5 /
c     data imach( 2) /    6 /
c     data imach( 3) /    5 /
c     data imach( 4) /    6 /
c     data imach( 5) /   32 /
c     data imach( 6) /    4 /
c     data imach( 7) /    2 /
c     data imach( 8) /   31 /
c     data imach( 9) / 2147483647 /
c     data imach(10) /    2 /
c     data imach(11) /   24 /
c     data imach(12) / -127 /
c     data imach(13) /  127 /
c     data imach(14) /   56 /
c     data imach(15) / -127 /
c     data imach(16) /  127 /
c
      if (i .lt. 1  .or.  i .gt. 16) go to 10
c
      i1mach=imach(i)
      return
c
 10   write(output,9000)
 9000 format(39h1error    1 in i1mach - i out of bounds)
c
      stop
c
      end
