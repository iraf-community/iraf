      SUBROUTINE CONSSD(X,Y,IC)
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
C
C  THIS SUBROUTINE SETS THE SHIELDING FLAG AND CONNECTS THE
C  USERS SHIELD ARRAYS TO SOME INTERNAL POINTERS
C
C  INPUT
C       X-X COORDINATE STRING
C       Y-Y COORDINATE STRING
C       IC-NUMBER OF COORDINATES
C
C  NOTE THE USERS ARRAYS CANNOT BE MUCKED WITH DURING EXECUTION
C       THOSE ARRAYS ARE USED DURING CONRAN EXECUTION
C
      DIMENSION X(1),Y(1)
      COMMON /CONR13/XVS(50),YVS(50),ICOUNT,SPVAL,SHIELD,
     1               SLDPLT
      LOGICAL SHIELD,SLDPLT
C
        SAVE
C
C  SET COUNTER
C
      ICOUNT = IC
C
C  CHECK THE DIMENSION OF SHIELD ARRAYS
C
      IERUNT = I1MACH(4)
      IF (ICOUNT .GT. 50) THEN
        CALL SETER (' CONSSD -- NUMBER OF SHIELD POINTS .GT. 50',1,1)
C
C + NOAO - FTN write and format statement commented out; SETER is enough.
C       WRITE(IERUNT,1001)
        ICOUNT = 50
      ENDIF
C1001 FORMAT(' ERROR 1 IN CONSSD -- NUMBER OF SHIELD POINTS .GT. 50')
C - NOAO
C
C  SET THE SHIELDING FLAG TO TRUE
C
      SHIELD = .TRUE.
C
C  COMPUTE POINTERS FOR THE USERS SHIELDING ARRAYS
C
      DO 300 I = 1,ICOUNT
        XVS(I) = X(I)
 300  YVS(I) = Y(I)
C
      RETURN
      END
