c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c
      parameter  (SIZE = 1008, ILOOP = 100)
      complex  a, w
      real     breal(SIZE), bimag(SIZE), qbreal(SIZE), qbimag(SIZE)
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
      breal(1) = 1.0
      bimag(1) = 0.0
      qbreal(1) = 1.0
      qbimag(1) = 0.0
      do 10 k=2,nn
        w = a**(k-1)
        breal(k) = real(w)
        bimag(k) = aimag(w)
        qbreal(k) = breal(k)
        qbimag(k) = bimag(k)
  10  continue
c
c now compute dft, idft, dft, idft, ...
c first dft is computed specially, in case subroutine needs to be started.
c
      call wfta(breal, bimag, SIZE, 0, 0, ierr)
      do 25 icount = 1, ILOOP
         call wfta(breal, bimag, SIZE, 1, 1, ierr)
         call wfta(breal, bimag, SIZE, 0, 1, ierr)
   25 continue
      call wfta(breal, bimag, SIZE, 1, 1, ierr)
c
c calculate rms error between b(i) and qb(i).
c
      err = 0.
      do 30 i=1,nn
        err = err + (breal(i)-qbreal(i))**2
     *            + (bimag(i)-qbimag(i))**2
  30  continue
      err = sqrt(err / float(nn))
      write (ioutd,9994) ILOOP, err
 9994 format(' rms error, after ', i5, ' loops = ', e15.8)
      stop
      end
