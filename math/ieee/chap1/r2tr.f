c
c-----------------------------------------------------------------------
c subroutine:  r2tr
c radix 2 iteration subroutine
c-----------------------------------------------------------------------
c
c
      subroutine r2tr(int, b0, b1)
      dimension b0(2), b1(2)
      do 10 k=1,int
        t = b0(k) + b1(k)
        b1(k) = b0(k) - b1(k)
        b0(k) = t
  10  continue
      return
      end
