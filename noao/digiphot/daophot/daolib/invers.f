c subroutine invers (a, max, n, iflag)
c
c Although it seems counter-intuitive, the tests that I have run
c so far suggest that the 180 x 180 matrices that NSTAR needs can
c be inverted with sufficient accuracy if the elements are REAL*4
c rather than REAL*8.
c
c Arguments
c 
c     a (input/output) is a square matrix of dimension N.  The inverse 
c       of the input matrix A is returned in A.
c
c   max (input) is the size assigned to the matrix A in the calling 
c       routine.  It is needed for the dimension statement below.
c
c iflag (output) is an error flag.  iflag  =  1 if the matrix could not
c       be inverted; iflag  =  0 if it could.
c
      subroutine invers (a, max, n, iflag)
c
      implicit none
      integer max, n, iflag
      real a(max,max)
      integer i, j, k
c
      iflag = 0
      i = 1
  300 if (a(i,i) .eq. 0.0e0) go to 9100
      a(i,i) = 1.0e0 / a(i,i)
      j = 1
  301 if (j .eq. i) go to 304
      a(j,i) = -a(j,i) * a(i,i)
      k = 1
  302 if (k .eq. i) go to 303
      a(j,k) = a(j,k) + a(j,i) * a(i,k)
  303 if (k .eq. n) go to 304
      k = k + 1
      go to 302
  304 if (j .eq. n) go to 305
      j = j + 1
      go to 301
  305 k = 1
  306 if (k .eq. i) go to 307
      a(i,k) = a(i,k) * a(i,i)
  307 if (k .eq. n) go to 308
      k = k + 1
      go to 306
  308 if (i .eq. n) return
      i = i+1
      go to 300
c
c Error:  zero on the diagonal.
c
 9100 iflag = 1
      return
c
      end
c
c
c
c subroutine dinvers (a, max, n, iflag)
c
c Arguments
c 
c     a (input/output) is a square matrix of dimension N.  The inverse 
c       of the input matrix A is returned in A.
c
c   max (input) is the size assigned to the matrix A in the calling 
c       routine.  It's needed for the dimension statement below.
c
c iflag (output) is an error flag.  iflag  =  1 if the matrix could not
c       be inverted; iflag  =  0 if it could.
c
      subroutine dinvers (a, max, n, iflag)
c
      implicit none
      integer max, n, iflag
      double precision a(max,max)
      integer i, j, k
c
      iflag = 0
      i = 1
  300 if (a(i,i) .eq. 0.0e0) go to 9100
      a(i,i) = 1.0e0 / a(i,i)
      j = 1
  301 if (j .eq. i) go to 304
      a(j,i) = -a(j,i) * a(i,i)
      k = 1
  302 if (k .EQ. i) go to 303
      a(j,k) = a(j,k) + a(j,i) * a(i,k)
  303 if (k .eq. n) go to 304
      k = k + 1
      go to 302
  304 if (j .eq. n) go to 305
      j = j + 1
      go to 301
  305 k = 1
  306 if (k .eq. i) go to 307
      a(i,k) = a(i,k) * a(i,i)
  307 if (k .eq. n) go to 308
      k = k + 1
      go to 306
  308 if (i .eq. n) return
      i = i+1
      go to 300
c
c Error:  zero on the diagonal.
c
 9100 iflag = 1
      return
c
      end
