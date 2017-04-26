      DOUBLE PRECISION FUNCTION slDSEPV (V1, V2)
*+
*     - - - - - -
*      D S E P V
*     - - - - - -
*
*  Angle between two vectors.
*
*  (double precision)
*
*  Given:
*     V1      d(3)    first vector
*     V2      d(3)    second vector
*
*  The result is the angle, in radians, between the two vectors.  It
*  is always positive.
*
*  Notes:
*
*  1  There is no requirement for the vectors to be unit length.
*
*  2  If either vector is null, zero is returned.
*
*  3  The simplest formulation would use dot product alone.  However,
*     this would reduce the accuracy for angles near zero and pi.  The
*     algorithm uses both cross product and dot product, which maintains
*     accuracy for all sizes of angle.
*
*  Called:  slDVXV, slDVN, slDVDV
*
*  Last revision:   14 June 2005
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

      DOUBLE PRECISION V1(3),V2(3)

      DOUBLE PRECISION V1XV2(3),WV(3),S,C
      DOUBLE PRECISION slDVDV



*  Modulus of cross product = sine multiplied by the two moduli.
      CALL slDVXV(V1,V2,V1XV2)
      CALL slDVN(V1XV2,WV,S)

*  Dot product = cosine multiplied by the two moduli.
      C = slDVDV(V1,V2)

*  Angle between the vectors.
      IF ( S.NE.0D0 .OR. C.NE.0D0 ) THEN
         slDSEPV = ATAN2(S,C)
      ELSE
         slDSEPV = 0D0
      END IF

      END
