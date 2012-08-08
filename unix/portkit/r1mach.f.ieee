c r1mach     from portlib                                  03/25/82
      real function r1mach(i)
c
c single-precision machine constants
c     r1mach(1) = b**(emin-1), the smallest positive magnitude.
c
c     r1mach(2) = b**emax*(1 - b**(-t)), the largest magnitude.
c
c     r1mach(3) = b**(-t), the smallest relative spacing.
c
c     r1mach(4) = b**(1-t), the largest relative spacing.
c
c     r1mach(5) = log10(b)
c
c     to alter this function for a particular environment,
c     the desired set of data statements should be activated by
c     removing the c from column 1.
c
c     where possible, octal or hexadecimal constants have been used
c     to specify the constants exactly which has in some cases
c     required the use of equivalent integer arrays.
c
      integer small(2)
      integer large(2)
      integer right(2)
      integer diver(2)
      integer log10(2)
c
      real rmach(5)
c
      equivalence (rmach(1),small(1))
      equivalence (rmach(2),large(1))
      equivalence (rmach(3),right(1))
      equivalence (rmach(4),diver(1))
      equivalence (rmach(5),log10(1))
c
c     machine constants for the burroughs 1700 system.
c
c     data rmach(1) / z400800000 /
c     data rmach(2) / z5ffffffff /
c     data rmach(3) / z4e9800000 /
c     data rmach(4) / z4ea800000 /
c     data rmach(5) / z500e730e8 /
c
c     machine constants for the burroughs 5700/6700/7700 systems.
c
c     data rmach(1) / 01771000000000000 /
c     data rmach(2) / o0777777777777777 /
c     data rmach(3) / o1311000000000000 /
c     data rmach(4) / o1301000000000000 /
c     data rmach(5) / o1157163034761675 /
c
c     machine constants for the cdc 6000/7000 series.
c
c     data rmach(1) / 00014000000000000000b /
c     data rmach(2) / 37767777777777777777b /
c     data rmach(3) / 16404000000000000000b /
c     data rmach(4) / 16414000000000000000b /
c     data rmach(5) / 17164642023241175720b /
c
c     machine constants for the cray 1
c
c     data rmach(1) / 200004000000000000000b /
c     data rmach(2) / 577777777777777777777b /
c     data rmach(3) / 377214000000000000000b /
c     data rmach(4) / 377224000000000000000b /
c     data rmach(5) / 377774642023241175720b /
c
c     machine constants for the data general eclipse s/200
c
c     note - it may be appropriate to include the following card -
c     static rmach(5)
c
c     data small/20k,0/,large/77777k,177777k/
c     data right/35420k,0/,diver/36020k,0/
c     data log10/40423k,42023k/
c
c     machine constants for the harris 220
c
c     data small(1),small(2) / "20000000, "00000201 /
c     data large(1),large(2) / "37777777, "00000177 /
c     data right(1),right(2) / "20000000, "00000352 /
c     data diver(1),diver(2) / "20000000, "00000353 /
c     data log10(1),log10(2) / "23210115, "00000377 /
c
c     machine constants for the honeywell 600/6000 series.
c
c     data rmach(1) / o402400000000 /
c     data rmach(2) / o376777777777 /
c     data rmach(3) / o714400000000 /
c     data rmach(4) / o716400000000 /
c     data rmach(5) / o776464202324 /
c
c     machine constants for the ibm 360/370 series,
c     the xerox sigma 5/7/9 and the sel systems 85/86.
c
c     data rmach(1) / z00100000 /
c     data rmach(2) / z7fffffff /
c     data rmach(3) / z3b100000 /
c     data rmach(4) / z3c100000 /
c     data rmach(5) / z41134413 /
c
c     machine constants for the pdp-10 (ka or ki processor).
c
c     data rmach(1) / "000400000000 /
c     data rmach(2) / "377777777777 /
c     data rmach(3) / "146400000000 /
c     data rmach(4) / "147400000000 /
c     data rmach(5) / "177464202324 /
c
c     machine constants for pdp-11 fortran"s supporting
c     32-bit integers (expressed in integer and octal).
c
c     KPNO -- Hex machine constants for the VAX, which stores the bytes of
c     a real variable in a funny order.
c
c No standard hex constants in fortran!!!
c      data small(1) / x'00000080' /
c      data large(1) / x'ffff7fff' /
c      data right(1) / x'00003480' /
c      data diver(1) / x'00003500' /
c      data log10(1) / x'209b359a' /
c
c      data small(1) / 128 /
c      data large(1) / -32769 /
c      data right(1) / 13440 /
c      data diver(1) / 13696 /
c      data log10(1) / 547042714 /
c
c     data small(1) /    8388608 /
c     data large(1) / 2147483647 /
c     data right(1) /  880803840 /
c     data diver(1) /  889192448 /
c     data log10(1) / 1067065499 /
c
c     data rmach(1) / o00040000000 /
c     data rmach(2) / o17777777777 /
c     data rmach(3) / o06440000000 /
c     data rmach(4) / o06500000000 /
c     data rmach(5) / o07746420233 /
c
c     machine constants for pdp-11 fortran"s supporting
c     16-bit integers (expressed in integer and octal).
c
c     data small(1),small(2) /   128,     0 /
c     data large(1),large(2) / 32767,    -1 /
c     data right(1),right(2) / 13440,     0 /
c     data diver(1),diver(2) / 13568,     0 /
c     data log10(1),log10(2) / 16282,  8347 /
c
c     data small(1),small(2) / o000200, o000000 /
c     data large(1),large(2) / o077777, o177777 /
c     data right(1),right(2) / o032200, o000000 /
c     data diver(1),diver(2) / o032400, o000000 /
c     data log10(1),log10(2) / o037632, o020233 /
c
c     machine constants for the univac 1100 series.
c
c     data rmach(1) / o000400000000 /
c     data rmach(2) / o377777777777 /
c     data rmach(3) / o146400000000 /
c     data rmach(4) / o147400000000 /
c     data rmach(5) / o177464202324 /
c
c     machine constants for IEEE single precision floating point on 68000.
c
c      data small(1) / x'100000' / 
c      data large(1) / x'7EFFFFFF' /
c      data right(1) / x'33800000' /
c      data diver(1) / x'34000000' /
c      data log10(1) / x'3F317218' /
c
       data small(1) /  1048576 /
       data large(1) / 2130706431 /
       data right(1) /  864026624 /
       data diver(1) /  872415232 /
       data log10(1) / 1060205080 /
c
c-----------------------------------------------------------------------
c delete next two statements after supplying the proper data statements.
c     data rmach(5) /0./
c     if (rmach(5) .eq. 0.0)
c    1call uliber(2,45h r1mach - machine dependent constants not set,45)
c-----------------------------------------------------------------------
c     if (i .lt. 1 .or. i .gt. 5)
c    1 call uliber (1,34h error in r1mach - i out of bounds,34)
c
      r1mach = rmach(i)
      return
c
      end
