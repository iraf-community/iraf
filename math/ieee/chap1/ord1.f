c
c-----------------------------------------------------------------------
c subroutine:  ord1
c in-place reordering subroutine
c-----------------------------------------------------------------------
c
      subroutine ord1(m, b)
      dimension b(2)
c
      k = 4
      kl = 2
      n = 2**m
      do 40 j=4,n,2
        if (k-j) 20, 20, 10
  10    t = b(j)
        b(j) = b(k)
        b(k) = t
  20    k = k - 2
        if (k-kl) 30, 30, 40
  30    k = 2*j
        kl = j
  40  continue
      return
      end
