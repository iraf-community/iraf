      SUBROUTINE CONOT2 (IVER,IUNIT)
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
C + NOAO - This routine is a no-op in IRAF.
C - NOAO
C
C  OUTPUT THE OPTION VALUES TO THE LINE PRINTER
C
C  CONTINUE FOR CONRAN AND CONRAS
C
C
C
C      COMMON /CONRA1/ CL(30)     ,NCL        ,OLDZ       ,PV(210)    ,
C     1                FINC       ,HI         ,FLO
C      COMMON /CONRA2/ REPEAT     ,EXTRAP     ,PER        ,MESS       ,
C     1                ISCALE     ,LOOK       ,PLDVLS     ,GRD        ,
C     2                CINC       ,CHILO      ,CON        ,LABON      ,
C     3                PMIMX      ,SCALE      ,FRADV      ,EXTRI      ,
C     4                BPSIZ      ,LISTOP
C      COMMON /CONRA3/ IREC
C      COMMON /CONRA4/ NCP        ,NCPSZ
C      COMMON /CONRA5/ NIT        ,ITIPV
C      COMMON /CONRA6/ XST        ,YST        ,XED        ,YED        ,
C     1                STPSZ      ,IGRAD      ,IG         ,XRG        ,
C     2                YRG        ,BORD       ,PXST       ,PYST       ,
C     3                PXED       ,PYED       ,ITICK
C      COMMON /CONRA7/ TITLE      ,ICNT   ,ITLSIZ
C      COMMON /CONRA8/ IHIGH      ,INMAJ      ,INLAB      ,INDAT      ,
C     1              LEN      ,IFMT       ,LEND       ,
C     2                IFMTD      ,ISIZEP     ,INMIN
C      COMMON /CONRA9/ ICOORD(500),NP         ,MXXY       ,TR         ,
C     1                BR         ,TL         ,BL         ,CONV       ,
C     2                XN         ,YN         ,ITLL       ,IBLL       ,
C     3                ITRL       ,IBRL       ,XC         ,YC         ,
C     4                ITLOC(210) ,JX         ,JY         ,ILOC       ,
C     5                ISHFCT     ,XO         ,YO         ,IOC        ,NC
C      COMMON /CONR10/ NT         ,NL         ,NTNL       ,JWIPT      ,
C     1                JWIWL      ,JWIWP      ,JWIPL      ,IPR        ,
C     2                ITPV
C      COMMON /CONR11/ NREP       ,NCRT       ,ISIZEL     ,
C     1                MINGAP     ,ISIZEM         ,
C     2                TENS
C      COMMON /CONR12/ IXMAX      ,IYMAX      ,XMAX       ,YMAX
C      LOGICAL         REPEAT     ,EXTRAP     ,PER        ,MESS       ,
C     1                LOOK       ,PLDVLS     ,GRD        ,LABON      ,
C     2                PMIMX      ,FRADV      ,EXTRI      ,CINC       ,
C     3                TITLE      ,LISTOP     ,CHILO      ,CON
C      COMMON /CONR15/ ISTRNG
C        CHARACTER*64 ISTRNG
C        COMMON /CONR16/ FORM
C        CHARACTER*10 FORM
C        COMMON /CONR17/ NDASH, IDASH, EDASH
C        CHARACTER*10 NDASH, IDASH, EDASH
C
C
C      SAVE
C
C  LABEL THE CONTOURS
C
C      WRITE (IUNIT,1001)
C      IF (LABON) GO TO  100
C      WRITE (IUNIT,1002)
C      GO TO  110
C  100 WRITE (IUNIT,1003)
C
C  LABEL SIZE
C
C  110 WRITE (IUNIT,1004) ISIZEL
C
C  SCALE DATA ON CONTOURS
C
C      WRITE (IUNIT,1005)
C      IF (SCALE .NE. 1.) GO TO  120
C      WRITE (IUNIT,1006)
C      GO TO  130
C  120 WRITE (IUNIT,1007) SCALE
C
C  TENSION FACTOR
C
C  130 WRITE (IUNIT,1008) TENS
C
C  PLOT RELATIVE MINS AND MAXS
C
C      WRITE (IUNIT,1009)
C      IF (PMIMX) GO TO  140
C      WRITE (IUNIT,1010)
C      GO TO  150
C  140 WRITE (IUNIT,1011)
C
C  SIZE OF MINIMUM AND MAXIMUM LABELS
C
C  150 WRITE (IUNIT,1012) ISIZEM
C
C  DASH PATTERN
C
C      WRITE (IUNIT,1013)
C      IF (IDASH(1:1) .EQ. ' ') GO TO  170
C      WRITE (IUNIT,1014) IDASH
C      GO TO  180
C  170 WRITE (IUNIT,1015)
C  180 IF (EDASH(1:1) .EQ. ' ') GO TO  200
C      WRITE (IUNIT,1016) EDASH
C      GO TO  210
C  200 WRITE (IUNIT,1017)
C  210 IF (NDASH(1:1) .EQ. ' ') GO TO  230
C      WRITE (IUNIT,1018) NDASH
C      GO TO  240
C  230 WRITE (IUNIT,1019)
C
C  DASH PATTERN BREAK POINT
C
C  240 WRITE (IUNIT,1020) BPSIZ
C
C  PRINT MINOR LINE GAP
C
C      ITT = MINGAP-1
C      WRITE (IUNIT,1021) ITT
C      RETURN
C
C 1001 FORMAT (5X,'LABEL THE CONTOURS, LAB=')
C 1002 FORMAT ('+',28X,'OFF')
C 1003 FORMAT ('+',28X,'ON')
C 1004 FORMAT (5X,'CONTOUR LABEL SIZE IN PWRIT UNITS, LSZ=',I4)
C 1005 FORMAT (5X,'SCALE THE DATA ON CONTOUR LINES, SDC=')
C 1006 FORMAT ('+',41X,'OFF')
C 1007 FORMAT ('+','ON, SCALE FACTOR=',G10.3)
C 1008 FORMAT (5X,'TENSION FACTOR (USED FOR SMOOTH AND SUPER), TEN=',
C     1        F6.2)
C 1009 FORMAT (5X,'PLOT RELATIVE MINIMUMS AND MAXIMUMS, PMM=')
C 1010 FORMAT ('+',45X,'OFF')
C 1011 FORMAT ('+',45X,'ON')
C 1012 FORMAT (5X,'SIZE OF MIN AND MAX LABELS IN PWRIT UNITS SML=',
C     1        I4)
C 1013 FORMAT (5X,'DASH PATTERN GTR=GREATER, EQU=EQUAL, LSS=LESS')
C 1014 FORMAT (10X,'GTR=',A10)
C 1015 FORMAT (10X,'GTR=$$$$$$$$$$')
C 1016 FORMAT (10X,'EQU=',A10)
C 1017 FORMAT (10X,'EQU=$$$$$$$$$$')
C 1018 FORMAT (10X,'LSS=',A10)
C 1019 FORMAT (10X,'LSS=$$$$$$$$$$')
C 1020 FORMAT (5X,'DASH PATTERN BREAK POINT, DBP=',G10.3)
C 1021 FORMAT (5X,'MINOR LINE COUNT=',I3)
C
C
C******************************************************************
C*                                                                *
C*                   REVISION HISTORY                             *
C*                                                                *
C*  JUNE 1980   ADDED CONTERP TO ULIB                             *
C*  AUGUST 1980 FIXED THE FOLLOWING PROBLEMS                      *
C*              1.PLOTTING OF INPUT DATA VALUES                   *
C*              2.SETTING OF MINIMUM INTENSITY IN ALL OPTION      *
C*              3.SETTING OF EQU FLAG IN CONTOUR DASH PATTERN     *
C*              4.TURNING OFF OF SIZE OF PLOTTED DATA OPTION      *
C*  DECEMBER 1980 FIXED CONTOUR SELECTION ALGORITHM  AND MOVED IN *
C*                DASH PACKAGE COMMON BLOCK INTPR
C*  MARCH 1981  FIXED NON-PORTABLE STATEMENT ORDERING IN CONSET   *
C*  APRIL 1981  FIXED OPTION LISTING ROUTINE                      *
C*              ADDED MINOR LINE COUNT OPTION                     *
C*  JULY 1983   ADDED LINEAR INTERPOLATION AND SHIELDING          *
C*  JULY 1984     CONVERTED TO STANDARD FORTRAN77 AND GKS         *
C*  AUGUST 1985 DELETED LOC (MACHINE DEPENDENT FUNCTION), CHANGED *
C*              COMMON /CONR13/                                   *
C*                                                                *
C******************************************************************
C
      END
