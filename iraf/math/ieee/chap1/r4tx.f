c
c-----------------------------------------------------------------------
c subroutine:  r4tx
c radix 4 iteration subroutine
c-----------------------------------------------------------------------
c
      subroutine r4tx(nthpo, cr0, cr1, cr2, cr3, ci0, ci1, ci2, ci3)
      dimension cr0(2), cr1(2), cr2(2), cr3(2), ci0(2), ci1(2), ci2(2),
     *    ci3(2)
      do 10 k=1,nthpo,4
        r1 = cr0(k) + cr2(k)
        r2 = cr0(k) - cr2(k)
        r3 = cr1(k) + cr3(k)
        r4 = cr1(k) - cr3(k)
        fi1 = ci0(k) + ci2(k)
        fi2 = ci0(k) - ci2(k)
        fi3 = ci1(k) + ci3(k)
        fi4 = ci1(k) - ci3(k)
        cr0(k) = r1 + r3
        ci0(k) = fi1 + fi3
        cr1(k) = r1 - r3
        ci1(k) = fi1 - fi3
        cr2(k) = r2 - fi4
        ci2(k) = fi2 + r4
        cr3(k) = r2 + fi4
        ci3(k) = fi2 - r4
  10  continue
      return
      end
