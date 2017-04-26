      DOUBLE PRECISION FUNCTION slEPCO (K0, K, E)
*+
*     - - - - -
*      E P C O
*     - - - - -
*
*  Convert an epoch into the appropriate form - 'B' or 'J'
*
*  Given:
*     K0    char    form of result:  'B'=Besselian, 'J'=Julian
*     K     char    form of given epoch:  'B' or 'J'
*     E     dp      epoch
*
*  Called:  slEPB, slEJ2D, slEPJ, slEB2D
*
*  Notes:
*
*     1) The result is always either equal to or very close to
*        the given epoch E.  The routine is required only in
*        applications where punctilious treatment of heterogeneous
*        mixtures of star positions is necessary.
*
*     2) K0 and K are not validated.  They are interpreted as follows:
*
*        o  If K0 and K are the same the result is E.
*        o  If K0 is 'B' or 'b' and K isn't, the conversion is J to B.
*        o  In all other cases, the conversion is B to J.
*
*        Note that K0 and K won't match if their cases differ.
*
*  P.T.Wallace   Starlink   5 September 1993
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

      CHARACTER*(*) K0,K
      DOUBLE PRECISION E
      DOUBLE PRECISION slEPB,slEJ2D,slEPJ,slEB2D


      IF (K.EQ.K0) THEN
         slEPCO=E
      ELSE IF (K0.EQ.'B'.OR.K0.EQ.'b') THEN
         slEPCO=slEPB(slEJ2D(E))
      ELSE
         slEPCO=slEPJ(slEB2D(E))
      END IF

      END
