      SUBROUTINE slTPSC (XI, ETA, RA, DEC, RAZ1, DECZ1,
     :                                        RAZ2, DECZ2, N)
*+
*     - - - - - -
*      T P S C
*     - - - - - -
*
*  From the tangent plane coordinates of a star of known RA,Dec,
*  determine the RA,Dec of the tangent point.
*
*  (single precision)
*
*  Given:
*     XI,ETA      r    tangent plane rectangular coordinates
*     RA,DEC      r    spherical coordinates
*
*  Returned:
*     RAZ1,DECZ1  r    spherical coordinates of tangent point, solution 1
*     RAZ2,DECZ2  r    spherical coordinates of tangent point, solution 2
*     N           i    number of solutions:
*                        0 = no solutions returned (note 2)
*                        1 = only the first solution is useful (note 3)
*                        2 = both solutions are useful (note 3)
*
*  Notes:
*
*  1  The RAZ1 and RAZ2 values are returned in the range 0-2pi.
*
*  2  Cases where there is no solution can only arise near the poles.
*     For example, it is clearly impossible for a star at the pole
*     itself to have a non-zero XI value, and hence it is
*     meaningless to ask where the tangent point would have to be
*     to bring about this combination of XI and DEC.
*
*  3  Also near the poles, cases can arise where there are two useful
*     solutions.  The argument N indicates whether the second of the
*     two solutions returned is useful.  N=1 indicates only one useful
*     solution, the usual case;  under these circumstances, the second
*     solution corresponds to the "over-the-pole" case, and this is
*     reflected in the values of RAZ2 and DECZ2 which are returned.
*
*  4  The DECZ1 and DECZ2 values are returned in the range +/-pi, but
*     in the usual, non-pole-crossing, case, the range is +/-pi/2.
*
*  5  This routine is the spherical equivalent of the routine slDPVC.
*
*  Called:  slRA2P
*
*  P.T.Wallace   Starlink   5 June 1995
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
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

      REAL XI,ETA,RA,DEC,RAZ1,DECZ1,RAZ2,DECZ2
      INTEGER N

      REAL X2,Y2,SD,CD,SDF,R2,R,S,C

      REAL slRA2P


      X2=XI*XI
      Y2=ETA*ETA
      SD=SIN(DEC)
      CD=COS(DEC)
      SDF=SD*SQRT(1.0+X2+Y2)
      R2=CD*CD*(1.0+Y2)-SD*SD*X2
      IF (R2.GE.0.0) THEN
         R=SQRT(R2)
         S=SDF-ETA*R
         C=SDF*ETA+R
         IF (XI.EQ.0.0.AND.R.EQ.0.0) R=1.0
         RAZ1=slRA2P(RA-ATAN2(XI,R))
         DECZ1=ATAN2(S,C)
         R=-R
         S=SDF-ETA*R
         C=SDF*ETA+R
         RAZ2=slRA2P(RA-ATAN2(XI,R))
         DECZ2=ATAN2(S,C)
         IF (ABS(SDF).LT.1.0) THEN
            N=1
         ELSE
            N=2
         END IF
      ELSE
         N=0
      END IF

      END
