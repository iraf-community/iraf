c
c-----------------------------------------------------------------------
c subroutine:  fr4syn
c radix 4 synthesis
c-----------------------------------------------------------------------
c
c
      subroutine fr4syn(int, nn, b0, b1, b2, b3, b4, b5, b6, b7)
      dimension l(15), b0(2), b1(2), b2(2), b3(2), b4(2), b5(2), b6(2),
     *    b7(2)
      common /const/ pii, p7, p7two, c22, s22, pi2
      equivalence (l15,l(1)), (l14,l(2)), (l13,l(3)), (l12,l(4)),
     *    (l11,l(5)), (l10,l(6)), (l9,l(7)), (l8,l(8)), (l7,l(9)),
     *    (l6,l(10)), (l5,l(11)), (l4,l(12)), (l3,l(13)), (l2,l(14)),
     *    (l1,l(15))
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
          t0 = b0(k) + b1(k)
          t1 = b0(k) - b1(k)
          t2 = b2(k)*2.0
          t3 = b3(k)*2.0
          b0(k) = t0 + t2
          b2(k) = t0 - t2
          b1(k) = t1 + t3
          b3(k) = t1 - t3
  60    continue
c
        if (nn-4) 120, 120, 70
  70    k0 = int*4 + 1
        kl = k0 + int - 1
        do 80 k=k0,kl
          t2 = b0(k) - b2(k)
          t3 = b1(k) + b3(k)
          b0(k) = (b0(k)+b2(k))*2.0
          b2(k) = (b3(k)-b1(k))*2.0
          b1(k) = (t2+t3)*p7two
          b3(k) = (t3-t2)*p7two
  80    continue
        go to 120
  90    arg = th2*piovn
        c1 = cos(arg)
        s1 = -sin(arg)
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
          t0 = b0(j) + b6(k)
          t1 = b7(k) - b1(j)
          t2 = b0(j) - b6(k)
          t3 = b7(k) + b1(j)
          t4 = b2(j) + b4(k)
          t5 = b5(k) - b3(j)
          t6 = b5(k) + b3(j)
          t7 = b4(k) - b2(j)
          b0(j) = t0 + t4
          b4(k) = t1 + t5
          b1(j) = (t2+t6)*c1 - (t3+t7)*s1
          b5(k) = (t2+t6)*s1 + (t3+t7)*c1
          b2(j) = (t0-t4)*c2 - (t1-t5)*s2
          b6(k) = (t0-t4)*s2 + (t1-t5)*c2
          b3(j) = (t2-t6)*c3 - (t3-t7)*s3
          b7(k) = (t2-t6)*s3 + (t3-t7)*c3
 100    continue
        jr = jr + 2
        ji = ji - 2
        if (ji-jl) 110, 110, 120
 110    ji = 2*jr - 1
        jl = jr
 120  continue
      return
      end
