c     prog4
c     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 jun 12
c     to appear in 'solving least squares problems', prentice-hall, 1974
c	   demonstrate singular value analysis.
c
      dimension a(15,5),b(15),sing(15)
      data names/1h /
c
      read (5,10) ((a(i,j),j=1,5),b(i),i=1,15)
      write (6,20)
      write (6,30) ((a(i,j),j=1,5),b(i),i=1,15)
      write (6,40)
c
      call sva (a,15,15,5,15,b,sing,names,1,d)
c
      stop
   10 format (6f12.0)
   20 format (46h1prog4.    demonstrate singular value analysis/53h list
     1ing of input matrix, a, and vector, b, follows..)
   30 format (1h /(5f12.8,f20.4))
   40 format (1h1)
      end
