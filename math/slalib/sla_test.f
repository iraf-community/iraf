      PROGRAM SLA_TEST
*+
*     - - - - -
*      T E S T
*     - - - - -
*
*  Simple test of SLALIB library - checks that a program can be
*  linked and a correct result returned from at least one subprogram.
*
*  P.T.Wallace   Starlink   24 August 1992
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*  Copyright (C) 1995 Association of Universities for Research in Astronomy Inc.
*-

      IMPLICIT NONE

      DOUBLE PRECISION D
      INTEGER J


      CALL slCADJ(1946,4,30,D,J)
      IF (J.EQ.0.AND.NINT(D).EQ.31940) THEN
         PRINT *,'SLALIB test completed satisfactorily.'
      ELSE
         PRINT *,'SLALIB test fails!'
      END IF

      END
