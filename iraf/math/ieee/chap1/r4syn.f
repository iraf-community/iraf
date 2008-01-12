c
c-----------------------------------------------------------------------
c subroutine:  r4syn
c radix 4 synthesis
c-----------------------------------------------------------------------
c
      subroutine r4syn(int, b0, b1, b2, b3)
      dimension b0(2), b1(2), b2(2), b3(2)
      do 10 k=1,int
        t0 = b0(k) + b1(k)
        t1 = b0(k) - b1(k)
        t2 = b2(k) + b2(k)
        t3 = b3(k) + b3(k)
        b0(k) = t0 + t2
        b2(k) = t0 - t2
        b1(k) = t1 + t3
        b3(k) = t1 - t3
  10  continue
      return
      end
