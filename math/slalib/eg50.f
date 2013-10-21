      SUBROUTINE slEG50 (DR, DD, DL, DB)
*+
*     - - - - -
*      E G 5 0
*     - - - - -
*
*  Transformation from B1950.0 'FK4' equatorial coordinates to
*  IAU 1958 galactic coordinates (double precision)
*
*  Given:
*     DR,DD       dp       B1950.0 'FK4' RA,Dec
*
*  Returned:
*     DL,DB       dp       galactic longitude and latitude L2,B2
*
*  (all arguments are radians)
*
*  Called:
*     slDS2C, slDMXV, slDC2S, slSUET, slDA2P, slDA1P
*
*  Note:
*     The equatorial coordinates are B1950.0 'FK4'.  Use the
*     routine slEQGA if conversion from J2000.0 coordinates
*     is required.
*
*  Reference:
*     Blaauw et al, Mon.Not.R.Astron.Soc.,121,123 (1960)
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

      DOUBLE PRECISION DR,DD,DL,DB

      DOUBLE PRECISION slDA2P,slDA1P

      DOUBLE PRECISION V1(3),V2(3),R,D

*
*  L2,B2 system of galactic coordinates
*
*  P = 192.25       RA of galactic north pole (mean B1950.0)
*  Q =  62.6        inclination of galactic to mean B1950.0 equator
*  R =  33          longitude of ascending node
*
*  P,Q,R are degrees
*
*
*  Equatorial to galactic rotation matrix
*
*  The Euler angles are P, Q, 90-R, about the z then y then
*  z axes.
*
*         +CP.CQ.SR-SP.CR     +SP.CQ.SR+CP.CR     -SQ.SR
*
*         -CP.CQ.CR-SP.SR     -SP.CQ.CR+CP.SR     +SQ.CR
*
*         +CP.SQ              +SP.SQ              +CQ
*

      DOUBLE PRECISION RMAT(3,3)
      DATA RMAT(1,1),RMAT(1,2),RMAT(1,3),
     :     RMAT(2,1),RMAT(2,2),RMAT(2,3),
     :     RMAT(3,1),RMAT(3,2),RMAT(3,3) /
     : -0.066988739415D0,-0.872755765852D0,-0.483538914632D0,
     : +0.492728466075D0,-0.450346958020D0,+0.744584633283D0,
     : -0.867600811151D0,-0.188374601723D0,+0.460199784784D0 /



*  Remove E-terms
      CALL slSUET(DR,DD,1950D0,R,D)

*  Spherical to Cartesian
      CALL slDS2C(R,D,V1)

*  Rotate to galactic
      CALL slDMXV(RMAT,V1,V2)

*  Cartesian to spherical
      CALL slDC2S(V2,DL,DB)

*  Express angles in conventional ranges
      DL=slDA2P(DL)
      DB=slDA1P(DB)

      END
