c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c
      parameter  (SIZE = 1024, ILOOP = 100)
      complex  w, b(SIZE), qb(SIZE), a
      common   /aa/ b
c
      ioutd = i1mach(2)
      nn = SIZE
      tpi = 8.*atan(1.)
      tpion = tpi/float(nn)
      w = cmplx(cos(tpion),-sin(tpion))
c
c generate a**k as test function
c result to b(i) for modification by dft and idft subroutines and
c a copy to qb(i) to compare final result with for error difference.
c
      a = (.9,.3)
      b(1) = (1.,0.)
      qb(1) = b(1)
      do 10 k=2,nn
        b(k) = a**(k-1)
        qb(k) = b(k)
  10  continue
c
c now compute dft, idft, dft, idft, ...
c first dft is computed specially, in case subroutine needs to be started.
c
      call radix4(5, 1, -1)
      do 25 icount = 1, ILOOP
         call radix4(5, 0,  1)
         call radix4(5, 0, -1)
   25 continue
      call radix4(5, 0,  1)
c
c calculate rms error between b(i) and qb(i).
c
      err = 0.
      do 30 i=1,nn
        de = cabs(qb(i)-b(i))
        err = err + de**2
  30  continue
      err = sqrt(err / float(nn))
      write (ioutd,9994) ILOOP, err
 9994 format(' rms error, after ', i5, ' loops = ', e15.8)
      stop
      end
