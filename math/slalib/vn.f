      SUBROUTINE slVN (V, UV, VM)
*+
*     - - -
*      V N
*     - - -
*
*  Normalises a 3-vector also giving the modulus
*  (single precision)
*
*  Given:
*     V       real(3)      vector
*
*  Returned:
*     UV      real(3)      unit vector in direction of V
*     VM      real         modulus of V
*
*  If the modulus of V is zero, UV is set to zero as well
*
*  P.T.Wallace   Starlink   November 1984
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      REAL V(3),UV(3),VM

      INTEGER I
      REAL W1,W2


*  Modulus
      W1=0.0
      DO I=1,3
         W2=V(I)
         W1=W1+W2*W2
      END DO
      W1=SQRT(W1)
      VM=W1

*  Normalise the vector
      IF (W1.LE.0.0) W1=1.0
      DO I=1,3
         UV(I)=V(I)/W1
      END DO

      END
