c
c-----------------------------------------------------------------------
c subroutine:  r4tr
c radix 4 iteration subroutine
c-----------------------------------------------------------------------
c
      subroutine r4tr(int, b0, b1, b2, b3)
      dimension b0(2), b1(2), b2(2), b3(2)
      do 10 k=1,int
        r0 = b0(k) + b2(k)
        r1 = b1(k) + b3(k)
        b2(k) = b0(k) - b2(k)
        b3(k) = b1(k) - b3(k)
        b0(k) = r0 + r1
        b1(k) = r0 - r1
  10  continue
      return
      end
