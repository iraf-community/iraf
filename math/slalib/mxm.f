      SUBROUTINE slMXM (A, B, C)
*+
*     - - - -
*      M X M
*     - - - -
*
*  Product of two 3x3 matrices:
*      matrix C  =  matrix A  x  matrix B
*
*  (single precision)
*
*  Given:
*      A      real(3,3)        matrix
*      B      real(3,3)        matrix
*
*  Returned:
*      C      real(3,3)        matrix result
*
*  To comply with the ANSI Fortran 77 standard, A, B and C must
*  be different arrays.  However, the routine is coded so as to
*  work properly on many platforms even if this rule is violated.
*
*  Last revision:   26 December 2004
*
*  Copyright P.T.Wallace.  All rights reserved.
*
*  License:
*    This program is free software; you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation; either version 2 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program (see SLA_CONDITIONS); if not, write to the
*    Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
*    Boston, MA  02110-1301  USA
*
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      REAL A(3,3),B(3,3),C(3,3)

      INTEGER I,J,K
      REAL W,WM(3,3)


*  Multiply into scratch matrix
      DO I=1,3
         DO J=1,3
            W=0.0
            DO K=1,3
               W=W+A(I,K)*B(K,J)
            END DO
            WM(I,J)=W
         END DO
      END DO

*  Return the result
      DO J=1,3
         DO I=1,3
            C(I,J)=WM(I,J)
         END DO
      END DO

      END
