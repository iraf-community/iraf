      function	 gen(anoise)
c     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1972 dec 15
c     to appear in 'solving least squares problems', prentice-hall, 1974
c	   generate numbers for construction of test cases.
      mi=891
      mj=457
      i=5
      j=7
      aj=0.
c
      if (anoise) 10,30,20
   10 gen=0.
      return
c
c     the sequence of values of j  is bounded between 1 and 996
c     if initial j = 1,2,3,4,5,6,7,8, or 9, the period is 332
c
   20 j=j*mj
      j=j-997*(j/997)
      aj=j-498
c     the sequence of values of i  is bounded between 1 and 999
c     if initial i = 1,2,3,6,7, or 9,  the period will be 50
c     if initial i = 4 or 8   the period will be 25
c     if initial i = 5	      the period will be 10
c
   30 i=i*mi
      i=i-1000*(i/1000)
      ai=i-500
      gen=ai+aj*anoise
      return
      end
