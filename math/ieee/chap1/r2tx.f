c
c-----------------------------------------------------------------------
c subroutine:  r2tx
c radix 2 iteration subroutine
c-----------------------------------------------------------------------
c
      subroutine r2tx(nthpo, cr0, cr1, ci0, ci1)
      dimension cr0(2), cr1(2), ci0(2), ci1(2)
      do 10 k=1,nthpo,2
        r1 = cr0(k) + cr1(k)
        cr1(k) = cr0(k) - cr1(k)
        cr0(k) = r1
        fi1 = ci0(k) + ci1(k)
        ci1(k) = ci0(k) - ci1(k)
        ci0(k) = fi1
  10  continue
      return
      end
