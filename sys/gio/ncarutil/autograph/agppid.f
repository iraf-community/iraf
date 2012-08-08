C
C                                                                               
C +-----------------------------------------------------------------+           
C |                                                                 |           
C |                Copyright (C) 1986 by UCAR                       |           
C |        University Corporation for Atmospheric Research          |           
C |                    All Rights Reserved                          |           
C |                                                                 |           
C |                 NCARGRAPHICS  Version 1.00                      |           
C |                                                                 |           
C +-----------------------------------------------------------------+           
C                                                                               
C                                                                               
C ---------------------------------------------------------------------
C
      SUBROUTINE AGPPID (TPID)
C
      CHARACTER*(*) TPID
C
C The object of this routine is to print out a parameter identifier
C which has caused some kind of problem.
C
C Define a character variable to hold the print line.
C
      CHARACTER*124 TEMP
C
C +NOAO
      integer*2 itemp(124)
C -NOAO
C
C Set up the print line.
C
      TEMP='0PARAMETER IDENTIFIER - '
C
C Transfer characters of the parameter identifier, one at a time, until
C 100 have been transferred or a period is encountered, whichever occurs
C first.  This is done so as to allow for old programs on the Cray which
C used Hollerith strings as parameter identifiers.
C
      I=24
C
      DO 101 J=1,100
        I=I+1
        TEMP(I:I)=TPID(J:J)
        IF (TEMP(I:I).EQ.'.') GO TO 102
  101 CONTINUE
C
C Print the line.
C
C +NOAO - replace FTN write and format statement.
C 102 WRITE (I1MACH(4),1001) TEMP
  102 CONTINUE
      call f77upk (temp, itemp, 125)
      call pstr (itemp)
C
C Done.
C
      RETURN
C
C Format.
C
C1001 FORMAT (A124)
C -NOAO
C
      END
