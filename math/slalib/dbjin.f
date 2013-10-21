      SUBROUTINE slDBJI (STRING, NSTRT, DRESLT, J1, J2)
*+
*     - - - - - -
*      D B J I
*     - - - - - -
*
*  Convert free-format input into double precision floating point,
*  using DFLTIN but with special syntax extensions.
*
*  The purpose of the syntax extensions is to help cope with mixed
*  FK4 and FK5 data.  In addition to the syntax accepted by DFLTIN,
*  the following two extensions are recognized by DBJIN:
*
*     1)  A valid non-null field preceded by the character 'B'
*         (or 'b') is accepted.
*
*     2)  A valid non-null field preceded by the character 'J'
*         (or 'j') is accepted.
*
*  The calling program is notified of the incidence of either of these
*  extensions through an supplementary status argument.  The rest of
*  the arguments are as for DFLTIN.
*
*  Given:
*     STRING      char       string containing field to be decoded
*     NSTRT       int        pointer to 1st character of field in string
*
*  Returned:
*     NSTRT       int        incremented
*     DRESLT      double     result
*     J1          int        DFLTIN status: -1 = -OK
*                                            0 = +OK
*                                           +1 = null field
*                                           +2 = error
*     J2          int        syntax flag:  0 = normal DFLTIN syntax
*                                         +1 = 'B' or 'b'
*                                         +2 = 'J' or 'j'
*
*  Called:  slDFLI
*
*  For details of the basic syntax, see slDFLI.
*
*  P.T.Wallace   Starlink   23 November 1995
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

      CHARACTER*(*) STRING
      INTEGER NSTRT
      DOUBLE PRECISION DRESLT
      INTEGER J1,J2

      INTEGER J2A,LENSTR,NA,J1A,NB,J1B
      CHARACTER C



*   Preset syntax flag
      J2A=0

*   Length of string
      LENSTR=LEN(STRING)

*   Pointer to current character
      NA=NSTRT

*   Attempt normal decode
      CALL slDFLI(STRING,NA,DRESLT,J1A)

*   Proceed only if pointer still within string
      IF (NA.GE.1.AND.NA.LE.LENSTR) THEN

*      See if DFLTIN reported a null field
         IF (J1A.EQ.1) THEN

*         It did: examine character it stuck on
            C=STRING(NA:NA)
            IF (C.EQ.'B'.OR.C.EQ.'b') THEN
*            'B' - provisionally note
               J2A=1
            ELSE IF (C.EQ.'J'.OR.C.EQ.'j') THEN
*            'J' - provisionally note
               J2A=2
            END IF

*         Following B or J, attempt to decode a number
            IF (J2A.EQ.1.OR.J2A.EQ.2) THEN
               NB=NA+1
               CALL slDFLI(STRING,NB,DRESLT,J1B)

*            If successful, copy pointer and status
               IF (J1B.LE.0) THEN
                  NA=NB
                  J1A=J1B
*            If not, forget about the B or J
               ELSE
                  J2A=0
               END IF

            END IF

         END IF

      END IF

*   Return argument values and exit
      NSTRT=NA
      J1=J1A
      J2=J2A

      END
