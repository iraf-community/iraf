c
c-----------------------------------------------------------------------
c subroutine:  ord2
c in-place reordering subroutine
c-----------------------------------------------------------------------
c
      subroutine ord2(m, b)
      dimension l(15), b(2)
      equivalence (l15,l(1)), (l14,l(2)), (l13,l(3)), (l12,l(4)),
     *    (l11,l(5)), (l10,l(6)), (l9,l(7)), (l8,l(8)), (l7,l(9)),
     *    (l6,l(10)), (l5,l(11)), (l4,l(12)), (l3,l(13)), (l2,l(14)),
     *    (l1,l(15))
      n = 2**m
      l(1) = n
      do 10 k=2,m
        l(k) = l(k-1)/2
  10  continue
      do 20 k=m,14
        l(k+1) = 2
  20  continue
      ij = 2
      do 40 j1=2,l1,2
      do 40 j2=j1,l2,l1
      do 40 j3=j2,l3,l2
      do 40 j4=j3,l4,l3
      do 40 j5=j4,l5,l4
      do 40 j6=j5,l6,l5
      do 40 j7=j6,l7,l6
      do 40 j8=j7,l8,l7
      do 40 j9=j8,l9,l8
      do 40 j10=j9,l10,l9
      do 40 j11=j10,l11,l10
      do 40 j12=j11,l12,l11
      do 40 j13=j12,l13,l12
      do 40 j14=j13,l14,l13
      do 40 ji=j14,l15,l14
        if (ij-ji) 30, 40, 40
  30    t = b(ij-1)
        b(ij-1) = b(ji-1)
        b(ji-1) = t
        t = b(ij)
        b(ij) = b(ji)
        b(ji) = t
  40    ij = ij + 2
      return
      end
