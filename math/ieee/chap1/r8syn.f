c
c-----------------------------------------------------------------------
c subroutine:  r8syn
c radix 8 synthesis subroutine
c-----------------------------------------------------------------------
c
      subroutine r8syn(int, nn, br0, br1, br2, br3, br4, br5, br6, br7,
     *    bi0, bi1, bi2, bi3, bi4, bi5, bi6, bi7)
      dimension l(15), br0(2), br1(2), br2(2), br3(2), br4(2), br5(2),
     *    br6(2), br7(2), bi0(2), bi1(2), bi2(2), bi3(2), bi4(2),
     *    bi5(2), bi6(2), bi7(2)
      common /con1/ pii, p7, p7two, c22, s22, pi2
      equivalence (l15,l(1)), (l14,l(2)), (l13,l(3)), (l12,l(4)),
     *    (l11,l(5)), (l10,l(6)), (l9,l(7)), (l8,l(8)), (l7,l(9)),
     *    (l6,l(10)), (l5,l(11)), (l4,l(12)), (l3,l(13)), (l2,l(14)),
     *    (l1,l(15))
      l(1) = nn/8
      do 40 k=2,15
        if (l(k-1)-2) 10, 20, 30
  10    l(k-1) = 2
  20    l(k) = 2
        go to 40
  30    l(k) = l(k-1)/2
  40  continue
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
          t0 = br0(k) + br1(k)
          t1 = br0(k) - br1(k)
          t2 = br2(k) + br2(k)
          t3 = br3(k) + br3(k)
          t4 = br4(k) + br6(k)
          t6 = br7(k) - br5(k)
          t5 = br4(k) - br6(k)
          t7 = br7(k) + br5(k)
          pr = p7*(t7+t5)
          pi = p7*(t7-t5)
          tt0 = t0 + t2
          tt1 = t1 + t3
          t2 = t0 - t2
          t3 = t1 - t3
          t4 = t4 + t4
          t5 = pr + pr
          t6 = t6 + t6
          t7 = pi + pi
          br0(k) = tt0 + t4
          br1(k) = tt1 + t5
          br2(k) = t2 + t6
          br3(k) = t3 + t7
          br4(k) = tt0 - t4
          br5(k) = tt1 - t5
          br6(k) = t2 - t6
          br7(k) = t3 - t7
  60    continue
        if (nn-8) 120, 120, 70
  70    k0 = int*8 + 1
        kl = k0 + int - 1
        do 80 k=k0,kl
          t1 = bi0(k) + bi6(k)
          t2 = bi7(k) - bi1(k)
          t3 = bi0(k) - bi6(k)
          t4 = bi7(k) + bi1(k)
          pr = t3*c22 + t4*s22
          pi = t4*c22 - t3*s22
          t5 = bi2(k) + bi4(k)
          t6 = bi5(k) - bi3(k)
          t7 = bi2(k) - bi4(k)
          t8 = bi5(k) + bi3(k)
          rr = t8*c22 - t7*s22
          ri = -t8*s22 - t7*c22
          bi0(k) = (t1+t5) + (t1+t5)
          bi4(k) = (t2+t6) + (t2+t6)
          bi1(k) = (pr+rr) + (pr+rr)
          bi5(k) = (pi+ri) + (pi+ri)
          t5 = t1 - t5
          t6 = t2 - t6
          bi2(k) = p7two*(t6+t5)
          bi6(k) = p7two*(t6-t5)
          rr = pr - rr
          ri = pi - ri
          bi3(k) = p7two*(ri+rr)
          bi7(k) = p7two*(ri-rr)
  80    continue
        go to 120
  90    arg = th2*piovn
        c1 = cos(arg)
        s1 = -sin(arg)
        c2 = c1**2 - s1**2
        s2 = c1*s1 + c1*s1
        c3 = c1*c2 - s1*s2
        s3 = c2*s1 + s2*c1
        c4 = c2**2 - s2**2
        s4 = c2*s2 + c2*s2
        c5 = c2*c3 - s2*s3
        s5 = c3*s2 + s3*c2
        c6 = c3**2 - s3**2
        s6 = c3*s3 + c3*s3
        c7 = c3*c4 - s3*s4
        s7 = c4*s3 + s4*c3
        int8 = int*8
        j0 = jr*int8 + 1
        k0 = ji*int8 + 1
        jlast = j0 + int - 1
        do 100 j=j0,jlast
          k = k0 + j - j0
          tr0 = br0(j) + bi6(k)
          ti0 = bi7(k) - br1(j)
          tr1 = br0(j) - bi6(k)
          ti1 = bi7(k) + br1(j)
          tr2 = br2(j) + bi4(k)
          ti2 = bi5(k) - br3(j)
          tr3 = bi5(k) + br3(j)
          ti3 = bi4(k) - br2(j)
          tr4 = br4(j) + bi2(k)
          ti4 = bi3(k) - br5(j)
          t0 = br4(j) - bi2(k)
          t1 = bi3(k) + br5(j)
          tr5 = p7*(t0+t1)
          ti5 = p7*(t1-t0)
          tr6 = br6(j) + bi0(k)
          ti6 = bi1(k) - br7(j)
          t0 = br6(j) - bi0(k)
          t1 = bi1(k) + br7(j)
          tr7 = -p7*(t0-t1)
          ti7 = -p7*(t1+t0)
          t0 = tr0 + tr2
          t1 = ti0 + ti2
          t2 = tr1 + tr3
          t3 = ti1 + ti3
          tr2 = tr0 - tr2
          ti2 = ti0 - ti2
          tr3 = tr1 - tr3
          ti3 = ti1 - ti3
          t4 = tr4 + tr6
          t5 = ti4 + ti6
          t6 = tr5 + tr7
          t7 = ti5 + ti7
          ttr6 = ti4 - ti6
          ti6 = tr6 - tr4
          ttr7 = ti5 - ti7
          ti7 = tr7 - tr5
          br0(j) = t0 + t4
          bi0(k) = t1 + t5
          br1(j) = c1*(t2+t6) - s1*(t3+t7)
          bi1(k) = c1*(t3+t7) + s1*(t2+t6)
          br2(j) = c2*(tr2+ttr6) - s2*(ti2+ti6)
          bi2(k) = c2*(ti2+ti6) + s2*(tr2+ttr6)
          br3(j) = c3*(tr3+ttr7) - s3*(ti3+ti7)
          bi3(k) = c3*(ti3+ti7) + s3*(tr3+ttr7)
          br4(j) = c4*(t0-t4) - s4*(t1-t5)
          bi4(k) = c4*(t1-t5) + s4*(t0-t4)
          br5(j) = c5*(t2-t6) - s5*(t3-t7)
          bi5(k) = c5*(t3-t7) + s5*(t2-t6)
          br6(j) = c6*(tr2-ttr6) - s6*(ti2-ti6)
          bi6(k) = c6*(ti2-ti6) + s6*(tr2-ttr6)
          br7(j) = c7*(tr3-ttr7) - s7*(ti3-ti7)
          bi7(k) = c7*(ti3-ti7) + s7*(tr3-ttr7)
 100    continue
        jr = jr + 2
        ji = ji - 2
        if (ji-jl) 110, 110, 120
 110    ji = 2*jr - 1
        jl = jr
 120  continue
      return
      end
