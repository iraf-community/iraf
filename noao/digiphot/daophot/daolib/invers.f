      SUBROUTINE  INVERS (A, MAX, N, IFLAG)
C
C Although it seems counter-intuitive, the tests that I have run
C so far suggest that the 180 x 180 matrices that NSTAR needs can
C be inverted with sufficient accuracy if the elements are REAL*4
C rather than REAL*8.
C
C Arguments
C 
C     A (INPUT/OUTPUT) is a square matrix of dimension N.  The inverse 
C       of the input matrix A is returned in A.
C
C   MAX (INPUT) is the size assigned to the matrix A in the calling 
C       routine.  It's needed for the dimension statement below.
C
C IFLAG (OUTPUT) is an error flag.  IFLAG = 1 if the matrix could not
C       be inverted; IFLAG = 0 if it could.
C
      REAL*4 A(MAX,MAX)
C
C-----------------------------------------------------------------------
C
      IFLAG=0
      I=1
  300 IF(A(I,I).EQ.0.0E0)GO TO 9100
      A(I,I)=1.0E0/A(I,I)
      J=1
  301 IF(J.EQ.I)GO TO 304
      A(J,I)=-A(J,I)*A(I,I)
      K=1
  302 IF(K.EQ.I)GO TO 303
      A(J,K)=A(J,K)+A(J,I)*A(I,K)
  303 IF(K.EQ.N)GO TO 304
      K=K+1
      GO TO 302
  304 IF(J.EQ.N)GO TO 305
      J=J+1
      GO TO 301
  305 K=1
  306 IF(K.EQ.I)GO TO 307
      A(I,K)=A(I,K)*A(I,I)
  307 IF(K.EQ.N)GO TO 308
      K=K+1
      GO TO 306
  308 IF(I.EQ.N)RETURN
      I=I+1
      GO TO 300
C
C-----------------------------------------------------------------------
C
C Error:  zero on the diagonal.
C
 9100 IFLAG=1
      RETURN
C
      END
