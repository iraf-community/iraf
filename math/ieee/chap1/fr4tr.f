c
c-----------------------------------------------------------------------
c subroutine:  fr4tr
c radix 4 iteration subroutine
c-----------------------------------------------------------------------
c
      subroutine fr4tr(int, nn, b0, b1, b2, b3, b4, b5, b6, b7)
      dimension l(15), b0(2), b1(2), b2(2), b3(2), b4(2), b5(2), b6(2),
     *    b7(2)
      common /cons/ pii, p7, p7two, c22, s22, pi2
      equivalence (l15,l(1)), (l14,l(2)), (l13,l(3)), (l12,l(4)),
     *    (l11,l(5)), (l10,l(6)), (l9,l(7)), (l8,l(8)), (l7,l(9)),
     *    (l6,l(10)), (l5,l(11)), (l4,l(12)), (l3,l(13)), (l2,l(14)),
     *    (l1,l(15))
c
c jthet is a reversed binary counter, jr steps two at a time to
c locate the real parts of intermediate results, and ji locates
c the imaginary part corresponding to jr.
c
      l(1) = nn/4
      do 40 k=2,15
        if (l(k-1)-2) 10, 20, 30
  10    l(k-1) = 2
  20    l(k) = 2
        go to 40
  30    l(k) = l(k-1)/2
  40  continue
c
      piovn = pii/float(nn)
      ji = 3
      jl = 2
      jr = 2
c
      do 120 j1=2,l1,2
      do 120 j2=j1,l2,l1
      do 120 j3=j2,l3,l2
      do 120 j4=j3,l4,l3
      do 120 j5=j4,l5,l4
      do 120 j6=j5,l6,l5
      do 120 j7=j6,l7,l6
      do 120 j8=j7,l8,l7
      do 120 j9=j8,l9,l8
      do 120 j10=j9,l10,l9
      do 120 j11=j10,l11,l10
      do 120 j12=j11,l12,l11
      do 120 j13=j12,l13,l12
      do 120 j14=j13,l14,l13
      do 120 jthet=j14,l15,l14
        th2 = jthet - 2
        if (th2) 50, 50, 90
  50    do 60 k=1,int
          t0 = b0(k) + b2(k)
          t1 = b1(k) + b3(k)
          b2(k) = b0(k) - b2(k)
          b3(k) = b1(k) - b3(k)
          b0(k) = t0 + t1
          b1(k) = t0 - t1
  60    continue
c
        if (nn-4) 120, 120, 70
  70    k0 = int*4 + 1
        kl = k0 + int - 1
        do 80 k=k0,kl
          pr = p7*(b1(k)-b3(k))
          pi = p7*(b1(k)+b3(k))
          b3(k) = b2(k) + pi
          b1(k) = pi - b2(k)
          b2(k) = b0(k) - pr
          b0(k) = b0(k) + pr
  80    continue
        go to 120
c
  90    arg = th2*piovn
        c1 = cos(arg)
        s1 = sin(arg)
        c2 = c1**2 - s1**2
        s2 = c1*s1 + c1*s1
        c3 = c1*c2 - s1*s2
        s3 = c2*s1 + s2*c1
c
        int4 = int*4
        j0 = jr*int4 + 1
        k0 = ji*int4 + 1
        jlast = j0 + int - 1
        do 100 j=j0,jlast
          k = k0 + j - j0
          r1 = b1(j)*c1 - b5(k)*s1
          r5 = b1(j)*s1 + b5(k)*c1
          t2 = b2(j)*c2 - b6(k)*s2
          t6 = b2(j)*s2 + b6(k)*c2
          t3 = b3(j)*c3 - b7(k)*s3
          t7 = b3(j)*s3 + b7(k)*c3
          t0 = b0(j) + t2
          t4 = b4(k) + t6
          t2 = b0(j) - t2
          t6 = b4(k) - t6
          t1 = r1 + t3
          t5 = r5 + t7
          t3 = r1 - t3
          t7 = r5 - t7
          b0(j) = t0 + t1
          b7(k) = t4 + t5
          b6(k) = t0 - t1
          b1(j) = t5 - t4
          b2(j) = t2 - t7
          b5(k) = t6 + t3
          b4(k) = t2 + t7
          b3(j) = t3 - t6
 100    continue
c
        jr = jr + 2
        ji = ji - 2
        if (ji-jl) 110, 110, 120
 110    ji = 2*jr - 1
        jl = jr
 120  continue
      return
      end
