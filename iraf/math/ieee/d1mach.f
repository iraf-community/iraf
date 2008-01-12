c
c----------------------------------------------------------------------
c  function:  d1mach
c  this routine is from the port mathematical subroutine library
c  it is described in the bell laboratories computing science
c  technical report #47 by p.a. fox, a.d. hall and n.l. schryer
c  a modification to the "i out of bounds" error message
c  has been made by c. a. mcgonegal - april, 1978
c----------------------------------------------------------------------
c
      double precision function d1mach(i)
c
c  double-precision machine constants
c
c  d1mach( 1) = b**(emin-1), the smallest positive magnitude.
c
c  d1mach( 2) = b**emax*(1 - b**(-t)), the largest magnitude.
c
c  d1mach( 3) = b**(-t), the smallest relative spacing.
c
c  d1mach( 4) = b**(1-t), the largest relative spacing.
c
c  d1mach( 5) = log10(b)
c
c  to alter this function for a particular environment,
c  the desired set of data statements should be activated by
c  removing the c from column 1.
c
c  where possible, octal or hexadecimal constants have been used
c  to specify the constants exactly which has in some cases
c  required the use of equivalent integer arrays.
c
      integer small(4)
      integer large(4)
      integer right(4)
      integer diver(4)
      integer log10(4)
c
      double precision dmach(5)
c
      equivalence (dmach(1),small(1))
      equivalence (dmach(2),large(1))
      equivalence (dmach(3),right(1))
      equivalence (dmach(4),diver(1))
      equivalence (dmach(5),log10(1))
c
c     machine constants for the burroughs 1700 system.
c
c     data small(1) / zc00800000 /
c     data small(2) / z000000000 /
c
c     data large(1) / zdffffffff /
c     data large(2) / zfffffffff /
c
c     data right(1) / zcc5800000 /
c     data right(2) / z000000000 /
c
c     data diver(1) / zcc6800000 /
c     data diver(2) / z000000000 /
c
c     data log10(1) / zd00e730e7 /
c     data log10(2) / zc77800dc0 /
c
c     machine constants for the burroughs 5700 system.
c
c     data small(1) / o1771000000000000 /
c     data small(2) / o0000000000000000 /
c
c     data large(1) / o0777777777777777 /
c     data large(2) / o0007777777777777 /
c
c     data right(1) / o1461000000000000 /
c     data right(2) / o0000000000000000 /
c
c     data diver(1) / o1451000000000000 /
c     data diver(2) / o0000000000000000 /
c
c     data log10(1) / o1157163034761674 /
c     data log10(2) / o0006677466732724 /
c
c     machine constants for the burroughs 6700/7700 systems.
c
c     data small(1) / o1771000000000000 /
c     data small(2) / o7770000000000000 /
c
c     data large(1) / o0777777777777777 /
c     data large(2) / o7777777777777777 /
c
c     data right(1) / o1461000000000000 /
c     data right(2) / o0000000000000000 /
c
c     data diver(1) / o1451000000000000 /
c     data diver(2) / o0000000000000000 /
c
c     data log10(1) / o1157163034761674 /
c     data log10(2) / o0006677466732724 /
c
c     machine constants for the cdc 6000/7000 series.
c
c     data small(1) / 00604000000000000000b /
c     data small(2) / 00000000000000000000b /
c
c     data large(1) / 37767777777777777777b /
c     data large(2) / 37167777777777777777b /
c
c     data right(1) / 15604000000000000000b /
c     data right(2) / 15000000000000000000b /
c
c     data diver(1) / 15614000000000000000b /
c     data diver(2) / 15010000000000000000b /
c
c     data log10(1) / 17164642023241175717b /
c     data log10(2) / 16367571421742254654b /
c
c     machine constants for the cray 1
c
c     data small(1) / 200004000000000000000b /
c     data small(2) / 000000000000000000000b /
c
c     data large(1) / 577767777777777777777b /
c     data large(2) / 000007777777777777776b /
c
c     data right(1) / 376424000000000000000b /
c     data right(2) / 000000000000000000000b /
c
c     data diver(1) / 376434000000000000000b /
c     data diver(2) / 000000000000000000000b /
c
c     data log10(1) / 377774642023241175717b /
c     data log10(2) / 000007571421742254654b /
c
c     machine constants for the data general eclipse s/200
c
c     note - it may be appropriate to include the following card -
c     static dmach(5)
c
c     data small/20k,3*0/,large/77777k,3*177777k/
c     data right/31420k,3*0/,diver/32020k,3*0/
c     data log10/40423k,42023k,50237k,74776k/
c
c     machine constants for the harris slash 6 and slash 7
c
c     data small(1),small(2) / '20000000, '00000201 /
c     data large(1),large(2) / '37777777, '37777577 /
c     data right(1),right(2) / '20000000, '00000333 /
c     data diver(1),diver(2) / '20000000, '00000334 /
c     data log10(1),log10(2) / '23210115, '10237777 /
c
c     machine constants for the honeywell 600/6000 series.
c
c     data small(1),small(2) / o402400000000, o000000000000 /
c     data large(1),large(2) / o376777777777, o777777777777 /
c     data right(1),right(2) / o604400000000, o000000000000 /
c     data diver(1),diver(2) / o606400000000, o000000000000 /
c     data log10(1),log10(2) / o776464202324, o117571775714 /
c
c     machine constants for the ibm 360/370 series,
c     the xerox sigma 5/7/9 and the sel systems 85/86.
c
c     data small(1),small(2) / z00100000, z00000000 /
c     data large(1),large(2) / z7fffffff, zffffffff /
c     data right(1),right(2) / z33100000, z00000000 /
c     data diver(1),diver(2) / z34100000, z00000000 /
c     data log10(1),log10(2) / z41134413, z509f79ff /
c
c     machine constants for the pdp-10 (ka processor).
c
c     data small(1),small(2) / "033400000000, "000000000000 /
c     data large(1),large(2) / "377777777777, "344777777777 /
c     data right(1),right(2) / "113400000000, "000000000000 /
c     data diver(1),diver(2) / "114400000000, "000000000000 /
c     data log10(1),log10(2) / "177464202324, "144117571776 /
c
c     machine constants for the pdp-10 (ki processor).
c
c     data small(1),small(2) / "000400000000, "000000000000 /
c     data large(1),large(2) / "377777777777, "377777777777 /
c     data right(1),right(2) / "103400000000, "000000000000 /
c     data diver(1),diver(2) / "104400000000, "000000000000 /
c     data log10(1),log10(2) / "177464202324, "476747767461 /
c
c     machine constants for pdp-11 fortran's supporting
c     32-bit integers (expressed in integer and octal).
c
      data small(1),small(2) /    8388608,           0 /
      data large(1),large(2) / 2147483647,          -1 /
      data right(1),right(2) /  612368384,           0 /
      data diver(1),diver(2) /  620756992,           0 /
      data log10(1),log10(2) / 1067065498, -2063872008 /
c
c     data small(1),small(2) / o00040000000, o00000000000 /
c     data large(1),large(2) / o17777777777, o37777777777 /
c     data right(1),right(2) / o04440000000, o00000000000 /
c     data diver(1),diver(2) / o04500000000, o00000000000 /
c     data log10(1),log10(2) / o07746420232, o20476747770 /
c
c     machine constants for pdp-11 fortran's supporting
c     16-bit integers (expressed in integer and octal).
c
c     data small(1),small(2) /    128,      0 /
c     data small(3),small(4) /      0,      0 /
c
c     data large(1),large(2) /  32767,     -1 /
c     data large(3),large(4) /     -1,     -1 /
c
c     data right(1),right(2) /   9344,      0 /
c     data right(3),right(4) /      0,      0 /
c
c     data diver(1),diver(2) /   9472,      0 /
c     data diver(3),diver(4) /      0,      0 /
c
c     data log10(1),log10(2) /  16282,   8346 /
c     data log10(3),log10(4) / -31493, -12296 /
c
c     data small(1),small(2) / o000200, o000000 /
c     data small(3),small(4) / o000000, o000000 /
c
c     data large(1),large(2) / o077777, o177777 /
c     data large(3),large(4) / o177777, o177777 /
c
c     data right(1),right(2) / o022200, o000000 /
c     data right(3),right(4) / o000000, o000000 /
c
c     data diver(1),diver(2) / o022400, o000000 /
c     data diver(3),diver(4) / o000000, o000000 /
c
c     data log10(1),log10(2) / o037632, o020232 /
c     data log10(3),log10(4) / o102373, o147770 /
c
c     machine constants for the univac 1100 series.
c
c     data small(1),small(2) / o000040000000, o000000000000 /
c     data large(1),large(2) / o377777777777, o777777777777 /
c     data right(1),right(2) / o170540000000, o000000000000 /
c     data diver(1),diver(2) / o170640000000, o000000000000 /
c     data log10(1),log10(2) / o177746420232, o411757177572 /
c
c     machine constants for the vax-11 with
c     fortran iv-plus compiler
c
c     data small(1),small(2) / z00000080, z00000000 /
c     data large(1),large(2) / zffff7fff, zffffffff /
c     data right(1),right(2) / z00002480, z00000000 /
c     data diver(1),diver(2) / z00002500, z00000000 /
c     data log10(1),log10(2) / z209a3f9a, zcffa84fb /
c
      if (i .lt. 1  .or.  i .gt. 5) goto 100
c
      d1mach = dmach(i)
      return
c
 100  iwunit = i1mach(4)
      write(iwunit, 99)
  99  format(24hd1mach - i out of bounds)
      stop
      end
