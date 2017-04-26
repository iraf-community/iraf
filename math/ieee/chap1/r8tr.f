c
c-----------------------------------------------------------------------
c subroutine: r8tr
c radix 8 iteration subroutine
c-----------------------------------------------------------------------
c
      subroutine r8tr(int, nn, br0, br1, br2, br3, br4, br5, br6, br7,
     *    bi0, bi1, bi2, bi3, bi4, bi5, bi6, bi7)
      dimension l(15), br0(2), br1(2), br2(2), br3(2), br4(2), br5(2),
     *    br6(2), br7(2), bi0(2), bi1(2), bi2(2), bi3(2), bi4(2),
     *    bi5(2), bi6(2), bi7(2)
      common /con/ pii, p7, p7two, c22, s22, pi2
      equivalence (l15,l(1)), (l14,l(2)), (l13,l(3)), (l12,l(4)),
     *    (l11,l(5)), (l10,l(6)), (l9,l(7)), (l8,l(8)), (l7,l(9)),
     *    (l6,l(10)), (l5,l(11)), (l4,l(12)), (l3,l(13)), (l2,l(14)),
     *    (l1,l(15))
c
c set up counters such that jthet steps through the arguments
c of w, jr steps through starting locations for the real part of the
c intermediate results and ji steps through starting locations
c of the imaginary part of the intermediate results.
c
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
          t0 = br0(k) + br4(k)
          t1 = br1(k) + br5(k)
          t2 = br2(k) + br6(k)
          t3 = br3(k) + br7(k)
          t4 = br0(k) - br4(k)
          t5 = br1(k) - br5(k)
          t6 = br2(k) - br6(k)
          t7 = br3(k) - br7(k)
          br2(k) = t0 - t2
          br3(k) = t1 - t3
          t0 = t0 + t2
          t1 = t1 + t3
          br0(k) = t0 + t1
          br1(k) = t0 - t1
          pr = p7*(t5-t7)
          pi = p7*(t5+t7)
          br4(k) = t4 + pr
          br7(k) = t6 + pi
          br6(k) = t4 - pr
          br5(k) = pi - t6
  60    continue
        if (nn-8) 120, 120, 70
  70    k0 = int*8 + 1
        kl = k0 + int - 1
        do 80 k=k0,kl
          pr = p7*(bi2(k)-bi6(k))
          pi = p7*(bi2(k)+bi6(k))
          tr0 = bi0(k) + pr
          ti0 = bi4(k) + pi
          tr2 = bi0(k) - pr
          ti2 = bi4(k) - pi
          pr = p7*(bi3(k)-bi7(k))
          pi = p7*(bi3(k)+bi7(k))
          tr1 = bi1(k) + pr
          ti1 = bi5(k) + pi
          tr3 = bi1(k) - pr
          ti3 = bi5(k) - pi
          pr = tr1*c22 - ti1*s22
          pi = ti1*c22 + tr1*s22
          bi0(k) = tr0 + pr
          bi6(k) = tr0 - pr
          bi7(k) = ti0 + pi
          bi1(k) = pi - ti0
          pr = -tr3*s22 - ti3*c22
          pi = tr3*c22 - ti3*s22
          bi2(k) = tr2 + pr
          bi4(k) = tr2 - pr
          bi5(k) = ti2 + pi
          bi3(k) = pi - ti2
  80    continue
        go to 120
  90    arg = th2*piovn
        c1 = cos(arg)
        s1 = sin(arg)
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
          tr1 = br1(j)*c1 - bi1(k)*s1
          ti1 = br1(j)*s1 + bi1(k)*c1
          tr2 = br2(j)*c2 - bi2(k)*s2
          ti2 = br2(j)*s2 + bi2(k)*c2
          tr3 = br3(j)*c3 - bi3(k)*s3
          ti3 = br3(j)*s3 + bi3(k)*c3
          tr4 = br4(j)*c4 - bi4(k)*s4
          ti4 = br4(j)*s4 + bi4(k)*c4
          tr5 = br5(j)*c5 - bi5(k)*s5
          ti5 = br5(j)*s5 + bi5(k)*c5
          tr6 = br6(j)*c6 - bi6(k)*s6
          ti6 = br6(j)*s6 + bi6(k)*c6
          tr7 = br7(j)*c7 - bi7(k)*s7
          ti7 = br7(j)*s7 + bi7(k)*c7
c
          t0 = br0(j) + tr4
          t1 = bi0(k) + ti4
          tr4 = br0(j) - tr4
          ti4 = bi0(k) - ti4
          t2 = tr1 + tr5
          t3 = ti1 + ti5
          tr5 = tr1 - tr5
          ti5 = ti1 - ti5
          t4 = tr2 + tr6
          t5 = ti2 + ti6
          tr6 = tr2 - tr6
          ti6 = ti2 - ti6
          t6 = tr3 + tr7
          t7 = ti3 + ti7
          tr7 = tr3 - tr7
          ti7 = ti3 - ti7
c
          tr0 = t0 + t4
          ti0 = t1 + t5
          tr2 = t0 - t4
          ti2 = t1 - t5
          tr1 = t2 + t6
          ti1 = t3 + t7
          tr3 = t2 - t6
          ti3 = t3 - t7
          t0 = tr4 - ti6
          t1 = ti4 + tr6
          t4 = tr4 + ti6
          t5 = ti4 - tr6
          t2 = tr5 - ti7
          t3 = ti5 + tr7
          t6 = tr5 + ti7
          t7 = ti5 - tr7
          br0(j) = tr0 + tr1
          bi7(k) = ti0 + ti1
          bi6(k) = tr0 - tr1
          br1(j) = ti1 - ti0
          br2(j) = tr2 - ti3
          bi5(k) = ti2 + tr3
          bi4(k) = tr2 + ti3
          br3(j) = tr3 - ti2
          pr = p7*(t2-t3)
          pi = p7*(t2+t3)
          br4(j) = t0 + pr
          bi3(k) = t1 + pi
          bi2(k) = t0 - pr
          br5(j) = pi - t1
          pr = -p7*(t6+t7)
          pi = p7*(t6-t7)
          br6(j) = t4 + pr
          bi1(k) = t5 + pi
          bi0(k) = t4 - pr
          br7(j) = pi - t5
 100    continue
        jr = jr + 2
        ji = ji - 2
        if (ji-jl) 110, 110, 120
 110    ji = 2*jr - 1
        jl = jr
 120  continue
      return
      end
