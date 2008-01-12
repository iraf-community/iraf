      SUBROUTINE HAFTON (Z,L,M,N,FLO,HI,NLEV,NOPT,NPRM,ISPV,SPVAL)
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
C     SUBROUTINE HAFTON (Z,L,M,N,FLO,HI,NLEV,NOPT,NPRM,ISPV,SPVAL)
C
C
C     DIMENSION OF           Z(L,M)
C     ARGUMENTS
C
C     LATEST REVISION        JULY,1984
C
C     PURPOSE                HAFTON DRAWS A HALF-TONE PICTURE FROM DATA
C                            STORED IN A RECTANGULAR ARRAY WITH THE
C                            INTENSITY IN THE PICTURE PROPORTIONAL TO
C                            THE DATA VALUE.
C
C     USAGE                  IF THE FOLLOWING ASSUMPTIONS ARE MET, USE
C
C                                 CALL EZHFTN (Z,M,N)
C
C                              ASSUMPTIONS:
C                                  .ALL OF THE ARRAY IS TO BE DRAWN.
C                                  .LOWEST VALUE IN Z WILL BE AT LOWEST
C                                   INTENSITY ON READER/PRINTER OUTPUT.
C                                  .HIGHEST VALUE IN Z WILL BE AT
C                                   HIGHEST INTENSITY.
C                                  .VALUES IN BETWEEN WILL APPEAR
C                                   LINEARLY SPACED.
C                                  .MAXIMUM POSSIBLE NUMBER OF
C                                   INTENSITIES ARE USED.
C                                  .THE PICTURE WILL HAVE A PERIMETER
C                                   DRAWN.
C                                  .FRAME WILL BE CALLED AFTER THE
C                                   PICTURE IS DRAWN.
C                                  .Z IS FILLED WITH NUMBERS THAT SHOULD
C                                   BE USED (NO MISSING VALUES).
C
C                            IF THESE ASSUMPTIONS ARE NOT MET, USE
C
C                                 CALL HAFTON (Z,L,M,N,FLO,HI,NLEV,
C                                              NOPT,NPRM,ISPV,SPVAL)
C
C     ARGUMENTS
C
C     ON INPUT               Z
C     FOR EZHFTN               M BY N ARRAY TO BE USED TO GENERATE A
C                              HALF-TONE PLOT.
C
C                            M
C                              FIRST DIMENSION OF Z.
C
C                            N
C                              SECOND DIMENSION OF Z.
C
C     ON OUTPUT              ALL ARGUMENTS ARE UNCHANGED.
C     FOR EZHFTN
C
C     ON INPUT               Z
C     FOR HAFTON               THE ORIGIN OF THE ARRAY TO BE PLOTTED.
C
C                            L
C                              THE FIRST DIMENSION OF Z IN THE CALLING
C                              PROGRAM.
C
C                            M
C                              THE NUMBER OF DATA VALUES TO BE PLOTTED
C                              IN THE X-DIRECTION (THE FIRST SUBSCRIPT
C                              DIRECTION).  WHEN PLOTTING ALL OF AN
C                              ARRAY, L = M.
C
C                            N
C                              THE NUMBER OF DATA VALUES TO BE PLOTTED
C                              IN THE Y-DIRECTION (THE SECOND SUBSCRIPT
C                              DIRECTION).
C
C                            FLO
C                              THE VALUE OF Z THAT CORRESPONDS TO THE
C                              LOWEST INTENSITY.  (WHEN NOPT.LT.0, FLO
C                              CORRESPONDS TO THE HIGHEST INTENSITY.)
C                              IF FLO=HI=0.0, MIN(Z) WILL BE USED FOR FLO.
C
C                            HI
C                              THE VALUE OF Z THAT CORRESPONDS TO THE
C                              HIGHEST INTENSITY.  (WHEN NOPT.LT.0, HI
C                              CORRESPONDS TO THE LOWEST INTENSITY.)  IF
C                              HI=FLO=0.0, MAX(Z) WILL BE USED FOR HI.
C
C                            NLEV
C                              THE NUMBER OF INTENSITY LEVELS DESIRED.
C                              16 MAXIMUM.  IF NLEV = 0 OR 1, 16 LEVELS
C                              ARE USED.
C
C                            NOPT
C                              FLAG TO CONTROL THE MAPPING OF Z ONTO THE
C                              INTENSITIES.  THE SIGN OF NOPT CONTROLS
C                              THE DIRECTNESS OR INVERSENESS OF THE
C                              MAPPING.
C
C                              . NOPT POSITIVE YIELDS DIRECT MAPPING.
C                                THE LARGEST VALUE OF Z PRODUCES THE
C                                MOST DENSE DOTS.  ON MECHANICAL PLOTTERS,
C                                LARGE VALUES OF Z WILL PRODUCE A DARK
C                                AREA ON THE PAPER.  WITH THE FILM
C                                DEVELOPMENT METHODS USED AT NCAR,
C                                LARGE VALUES OF Z WILL PRODUCE MANY
C                                (WHITE) DOTS ON THE FILM, ALSO
C                                RESULTING IN A DARK AREA ON
C                                READER-PRINTER PAPER.
C                              . NOPT NEGATIVE YIELDS INVERSE MAPPING.
C                                THE SMALLEST VALUES OF Z PRODUCE THE
C                                MOST DENSE DOTS RESULTING IN DARK
C                                AREAS ON THE PAPER.
C
C                              THE ABSOLUTE VALUE OF NOPT DETERMINES THE
C                              MAPPING OF Z ONTO THE INTENSITIES.  FOR
C                              IABS(NOPT)
C                              = 0  THE MAPPING IS LINEAR.  FOR
C                                   EACH INTENSITY THERE IS AN EQUAL
C                                   RANGE IN Z VALUE.
C                              = 1  THE MAPPING IS LINEAR.  FOR
C                                   EACH INTENSITY THERE IS AN EQUAL
C                                   RANGE IN Z VALUE.
C                              = 2  THE MAPPING IS EXPONENTIAL.  FOR
C                                   LARGER VALUES OF Z, THERE IS A
C                                   LARGER DIFFERENCE IN INTENSITY FOR
C                                   RELATIVELY CLOSE VALUES OF Z.  DETAILS
C                                   IN THE LARGER VALUES OF Z ARE DISPLAYED
C                                   AT THE EXPENSE OF THE SMALLER VALUES
C                                   OF Z.
C                              = 3  THE MAPPING IS LOGRITHMIC, SO
C                                   DETAILS OF SMALLER VALUES OF Z ARE SHOWN
C                                   AT THE EXPENSE OF LARGER VALUES OF Z.
C                              = 4  SINUSOIDAL MAPPING, SO MID-RANGE VALUES
C                                   OF Z SHOW DETAILS AT THE EXPENSE OF
C                                   EXTREME VALUES OF Z.
C                              = 5  ARCSINE MAPPING, SO EXTREME VALUES OF
C                                   Z ARE SHOWN AT THE EXPENSE OF MID-RANGE
C                                   VALUES OF Z.
C
C                            NPRM
C                              FLAG TO CONTROL THE DRAWING OF A
C                              PERIMETER AROUND THE HALF-TONE PICTURE.
C
C                              . NPRM=0:  THE PERIMETER IS DRAWN WITH
C                                TICKS POINTING AT DATA LOCATIONS.
C                                (SIDE LENGTHS ARE PROPORTIONAL TO NUMBER
C                                OF DATA VALUES.)
C                              . NPRM POSITIVE:  NO PERIMETER IS DRAWN.  THE
C                                PICTURE FILLS THE FRAME.
C                              . NPRM NEGATIVE:  THE PICTURE IS WITHIN THE
C                                CONFINES OF THE USER'S CURRENT VIEWPORT
C                                SETTING.
C
C                            ISPV
C                              FLAG TO TELL IF THE SPECIAL VALUE FEATURE
C                              IS BEING USED.  THE SPECIAL VALUE FEATURE
C                              IS USED TO MARK AREAS WHERE THE DATA IS
C                              NOT KNOWN OR HOLES ARE WANTED IN THE
C                              PICTURE.
C
C                              . ISPV = 0:  SPECIAL VALUE FEATURE NOT IN
C                                USE.  SPVAL IS IGNORED.
C                              . ISPV NON-ZERO:  SPECIAL VALUE FEATURE
C                                IN USE.  SPVAL DEFINES THE SPECIAL
C                                VALUE.  WHERE Z CONTAINS THE SPECIAL
C                                VALUE, NO HALF-TONE IS DRAWN.  IF ISPV
C                                = 0  SPECIAL VALUE FEATURE NOT IN USE.
C                                     SPVAL IS IGNORED.
C                                = 1  NOTHING IS DRAWN IN SPECIAL VALUE
C                                     AREA.
C                                = 2  CONTIGUOUS SPECIAL VALUE AREAS ARE
C                                     SURROUNDED BY A POLYGONAL LINE.
C                                = 3  SPECIAL VALUE AREAS ARE FILLED
C                                     WITH X(S).
C                                = 4  SPECIAL VALUE AREAS ARE FILLED IN
C                                     WITH THE HIGHEST INTENSITY.
C
C                            SPVAL
C                              THE VALUE USED IN Z TO DENOTE MISSING
C                              VALUES.  THIS ARGUMENT IS IGNORED IF
C                              ISPV = 0.
C
C     ON OUTPUT              ALL ARGUMENTS ARE UNCHANGED.
C     FOR HAFTON
C
C     NOTE                   THIS ROUTINE PRODUCES A HUGE NUMBER OF
C                            PLOTTER INSTRUCTIONS PER PICTURE, AVERAGING
C                            OVER 100,000 LINE-DRAWS PER FRAME WHEN M = N.
C
C
C     ENTRY POINTS           EZHFTN, HAFTON, ZLSET, GRAY, BOUND, HFINIT
C
C     COMMON BLOCKS          HAFT01, HAFT02, HAFT03, HAFT04
C
C     REQUIRED LIBRARY       GRIDAL, THE ERPRT77 PACKAGE AND THE SPPS.
C     ROUTINES
C
C     I/O                    PLOTS HALF-TONE PICTURE.
C
C     PRECISION              SINGLE
C
C     LANGUAGE               FORTRAN
C
C     HISTORY                REWRITE OF PHOMAP ORIGINALLY WRITTEN BY
C                            M. PERRY OF HIGH ALTITUDE OBSERVATORY,
C                            NCAR.
C
C     ALGORITHM              BI-LINEAR INTERPOLATION ON PLOTTER
C                            (RESOLUTION-LIMITED) GRID OF NORMALIZED
C                            REPRESENTATION OF DATA.
C
C     PORTABILITY            ANSI FORTRAN 77.
C
C
C
C INTERNAL PARAMTERSS
C VALUES SET IN BLOCK DATA
C         NAME  DEFAULT                   FUNCTION
C         ----  -------                   ________
C
C         XLT     0.1    LEFT-HAND EDGE OF THE PLOT WHEN NSET=0.  (0.0=
C                        LEFT EDGE OF FRAME, 1.0=RIGHT EDGE OF FRAME.)
C         YBT     0.1    BOTTOM EDGE OF THE PLOT WHEN NSET=0.  (0.0=
C                        BOTTOM OF FRAME, 1.0=TOP OF FRAME.)
C         SIDE    0.8    LENGTH OF LONGER EDGE OF PLOT (SEE ALSO EXT).
C         EXT     .25    LENGTHS OF THE SIDES OF THE PLOT ARE PROPOR-
C                        TIONAL TO M AND N (WHEN NSET=0) EXCEPT IN
C                        EXTREME CASES, NAMELY, WHEN MIN(M,N)/MAX(M,N)
C                        IS LESS THAN EXT.  THEN A SQUARE PLOT IS PRO-
C                        DUCED.  WHEN A RECTANGULAR PLOT IS PRODUCED,
C                        THE PLOT IS CENTERED ON THE FRAME (AS LONG AS
C                        SIDE+2*XLT = SIDE+2*YBT=1., AS WITH THE
C                        DEFAULTS.)
C         ALPHA   1.6    A PARAMETER TO CONTROL THE EXTREMENESS OF THE
C                        MAPPING FUNCTION SPECIFIED BY NOPT.  (FOR
C                        IABS(NOPT)=0 OR 1, THE MAPPING FUNCTION IS
C                        LINEAR AND INDEPENDENT OF ALPHA.)  FOR THE NON-
C                        LINEAR MAPPING FUNCTIONS, WHEN ALPHA IS CHANGED
C                        TO A NUMBER CLOSER TO 1., THE MAPPING FUNCTION
C                        BECOMES MORE LINEAR; WHEN ALPHA IS CHANGED TO
C                        A LARGER NUMBER, THE MAPPING FUNCTION BECOMES
C                        MORE EXTREME.
C         MXLEV   16     MAXIMUM NUMBER OF LEVELS.  LIMITED BY PLOTTER.
C         NCRTG   8      NUMBER OF CRT UNITS PER GRAY-SCALE CELL.
C                        LIMITED BY PLOTTER.
C         NCRTF   1024   NUMBER OF PLOTTER ADDRESS UNITS PER FRAME.
C         IL    (BELOW)  AN ARRAY DEFINING WHICH OF THE AVAILABLE IN-
C                        TENSITIES ARE USED WHEN LESS THAN THE MAXIMUM
C                        NUMBER OF INTENSITIES ARE REQUESTED.
C
C
C   NLEV         INTENSITIES USED
C   ____         ________________
C    2           5,11,
C    3           4, 8,12,
C    4           3, 6,10,13,
C    5           2, 5, 8,11,14,
C    6           1, 4, 7, 9,12,15,
C    7           1, 4, 6, 8,10,12,15,
C    8           1, 3, 5, 7, 9,11,13,15,
C    9           1, 3, 4, 6, 8,10,12,13,15
C   10           1, 3, 4, 6, 7, 9,10,12,13,15,
C   11           1, 2, 3, 5, 6, 8,10,11,13,14,15,
C   12           1, 2, 3, 5, 6, 7, 9,10,11,13,14,15,
C   13           1, 2, 3, 4, 6, 7, 8, 9,10,12,13,14,15
C   14           1, 2, 3, 4, 5, 6, 7, 9,10,11,12,13,14,15,
C   15           1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,
C   16           0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15
C
C

      SAVE
      DIMENSION       Z(L,N)      ,PX(2)       ,PY(2)
      DIMENSION       ZLEV(16)    ,VWPRT(4)    ,WNDW(4)
      DIMENSION                    VWPR2(4)    ,WND2(4)
      CHARACTER*11    IDUMMY
C
C
      COMMON /HAFTO1/ I          ,J          ,INTEN
      COMMON /HAFTO2/ GLO        ,HA         ,NOPTN      ,ALPHA      ,
     1                NSPV       ,SP         ,ICNST
      COMMON /HAFTO3/ XLT        ,YBT        ,SIDE       ,EXT        ,
     1                IOFFM      ,ALPH       ,MXLEV      ,NCRTG      ,
     2                NCRTF      ,IL(135)
      COMMON /HAFTO4/ NPTMAX     ,NPOINT     ,XPNT(50)  ,YPNT(50)
C +NOAO - Blockdata rewritten as run time initialization subroutine
C
C     EXTERNAL HFINIT
      call hfinit
C -NOAO
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('GRAPHX','HAFTON','HAFTON','VERSION  1')
C
      NPOINT = 0
      ALPHA = ALPH
      GLO = FLO
      HA = HI
      NLEVL = MIN0(IABS(NLEV),MXLEV)
      IF (NLEVL .LE. 1) NLEVL = MXLEV
      NOPTN = NOPT
      IF (NOPTN .EQ. 0) NOPTN = 1
      NPRIM = NPRM
      NSPV = MAX0(MIN0(ISPV,4),0)
      IF (NSPV .NE. 0) SP = SPVAL
      MX = L
      NX = M
      NY = N
      CRTF = NCRTF
      MSPV = 0
C
C SET INTENSITY BOUNDARY LEVELS
C
      CALL ZLSET (Z,MX,NX,NY,ZLEV,NLEVL)
C
C SET UP PERIMETER
C
      X3 = NX
      Y3 = NY
      CALL GQCNTN (IERR,NTORIG)
      CALL GETUSV('LS',IOLLS)
      IF (NPRIM.LT.0) THEN
         CALL GQNT (NTORIG,IERR,WNDW,VWPRT)
         X1 = VWPRT(1)
         X2 = VWPRT(2)
         Y1 = VWPRT(3)
         Y2 = VWPRT(4)
      ELSE IF (NPRIM.EQ.0) THEN
         X1 = XLT
         X2 = XLT+SIDE
         Y1 = YBT
         Y2 = YBT+SIDE
         IF (AMIN1(X3,Y3)/AMAX1(X3,Y3) .GE. EXT) THEN
            IF (NX-NY.LT.0) THEN
               X2 =SIDE*X3/Y3+XLT
               X2 = (AINT(X2*CRTF/FLOAT(NCRTG))*FLOAT(NCRTG))/CRTF
            ELSE IF (NX-NY.GT.0) THEN
               Y2 = SIDE*Y3/X3+YBT
               Y2 = (AINT(Y2*CRTF/FLOAT(NCRTG))*FLOAT(NCRTG))/CRTF
            END IF
         END IF
      ELSE IF (NPRIM.GT.0) THEN
         X1 = 0.0
         X2 = 1.0
         Y1 = 0.0
         Y2 = 1.0
      END IF
      MX1 = X1*CRTF
      MX2 = X2*CRTF
      MY1 = Y1*CRTF
      MY2 = Y2*CRTF
      IF (NPRIM.GT.0) THEN
         MX1 = 1
         MY1 = 1
         MX2 = NCRTF
         MY2 = NCRTF
      END IF
C
C SAVE NORMALIZATION TRANS 1
C
      CALL GQNT (1,IERR,WNDW,VWPRT)
C
C DEFINE NORMALIZATION TRANS 1 AND LOG SCALING FOR USE WITH PERIM
C DRAW PERIMETER IF NPRIM EQUALS 0
C
      CALL SET(X1,X2,Y1,Y2,1.0,X3,1.0,Y3,1)
      IF (NPRIM .EQ. 0) CALL PERIM (NX-1,1,NY-1,1)
      IF (ICNST .NE. 0) THEN
        CALL GSELNT (0)
        CALL WTSTR(XLT*1.1,0.5,'CONSTANT FIELD',2,0,0)
        GO TO 132
      END IF
C
C FIND OFFSET FOR REFERENCE TO IL, WHICH IS TRIANGULAR
C
      IOFFST = NLEVL*((NLEVL-1)/2)+MOD(NLEVL-1,2)*(NLEVL/2)-1
C
C OUTPUT INTENSITY SCALE
C
      IF (NPRIM .GT. 0) GO TO 112
      LEV = 0
      KX = (1.1*XLT+SIDE)*CRTF
      KY = YBT*CRTF
      NNX = KX/NCRTG
  109 LEV = LEV+1
C +NOAO
C The following statement moved from after statement label 111 (CONTINUE) to
C here.  Otherwise an extra (unlabelled) grayscale box was being drawn.
C This was (eventually) causing a [floating operand error] on a Sun-3.
      IF (LEV .GT. NLEVL) GO TO 112
C -NOAO
      ISUB = IOFFST+LEV
      INTEN = IL(ISUB)
      IF (NOPTN .LT. 0) INTEN = MXLEV-INTEN
      NNY = KY/NCRTG
      DO 111 JJ=1,3
         DO 110 II=1,10
            I = NNX+II
            J = NNY+JJ
            CALL GRAY
  110    CONTINUE
  111 CONTINUE
C +NOAO - FTN internal write rewritten as call to encode.
C       WRITE(IDUMMY,'(G11.4)') ZLEV(LEV)
	call encode (11, '(g11.4)', idummy, zlev(lev))
C -NOAO
      TKX = KX
      TKY = KY+38
      CALL GQNT(1,IERR,WND2,VWPR2)
      CALL SET(0.,1.,0.,1.,0.,1023.,0.,1023.,1)
      CALL WTSTR (TKX,TKY,IDUMMY,0,0,-1)
      CALL SET(VWPR2(1),VWPR2(2),VWPR2(3),VWPR2(4),
     -         WND2(1),WND2(2),WND2(3),WND2(4),1)
C
C ADJUST 38 TO PLOTTER.
C
      KY = KY+52
C
C ADJUST 52 TO PLOTTER.
C
      GO TO 109
C
C STEP THROUGH PLOTTER GRID OF INTENSITY CELLS.
C
  112 IMIN = (MX1-1)/NCRTG+1
      IMAX = (MX2-1)/NCRTG
      JMIN = (MY1-1)/NCRTG+1
      JMAX = (MY2-1)/NCRTG
      XL = IMAX-IMIN+1
      YL = JMAX-JMIN+1
      XN = NX
      YN = NY
      LSRT = NLEVL/2
      DO 130 J=JMIN,JMAX
C
C FIND Y FOR THIS J AND Z FOR THIS Y.
C
       YJ = (FLOAT(J-JMIN)+.5)/YL*(YN-1.)+1.
         LOWY = YJ
         YPART = YJ-FLOAT(LOWY)
         IF (LOWY .NE. NY) GO TO 113
         LOWY = LOWY-1
         YPART = 1.
  113    IPEN = 0
         ZLFT = Z(1,LOWY)+YPART*(Z(1,LOWY+1)-Z(1,LOWY))
         ZRHT = Z(2,LOWY)+YPART*(Z(2,LOWY+1)-Z(2,LOWY))
         IF (NSPV .EQ. 0) GO TO 114
         IF (Z(1,LOWY).EQ.SP .OR. Z(2,LOWY).EQ.SP .OR.
     1       Z(1,LOWY+1).EQ.SP .OR. Z(2,LOWY+1).EQ.SP) IPEN = 1
  114    IF (IPEN .EQ. 1) GO TO 117
C
C FIND INT FOR THIS Z.
C
         IF (ZLFT .GT. ZLEV(LSRT+1)) GO TO 116
  115    IF (ZLFT .GE. ZLEV(LSRT)) GO TO 117
C
C LOOK LOWER
C
         IF (LSRT .LE. 1) GO TO 117
         LSRT = LSRT-1
         GO TO 115
C
C LOOK HIGHER
C
  116    IF (LSRT .GE. NLEVL) GO TO 117
         LSRT = LSRT+1
         IF (ZLFT .GT. ZLEV(LSRT+1)) GO TO 116
C
C OK
C
  117    IRHT = 2
         LAST = LSRT
         DO 129 I=IMIN,IMAX
C
C FIND X FOR THIS I AND Z FOR THIS X AND Y.
C
            IADD = 1
            XI = (FLOAT(I-IMIN)+.5)/XL*(XN-1.)+1.
            LOWX = XI
            XPART = XI-FLOAT(LOWX)
            IF (LOWX .NE. NX) GO TO 118
            LOWX = LOWX-1
            XPART = 1.
C
C TEST FOR INTERPOLATION POSITIONING
C
  118       IF (LOWX .LT. IRHT) GO TO 119
C
C MOVE INTERPOLATION ONE CELL TO THE RIGHT
C
            ZLFT = ZRHT
            IRHT = IRHT+1
            ZRHT = Z(IRHT,LOWY)+YPART*(Z(IRHT,LOWY+1)-Z(IRHT,LOWY))
            IF (NSPV .EQ. 0) GO TO 118
            IPEN = 0
            IF (Z(IRHT-1,LOWY).EQ.SP .OR. Z(IRHT,LOWY).EQ.SP .OR.
     1          Z(IRHT-1,LOWY+1).EQ.SP .OR. Z(IRHT,LOWY+1).EQ.SP)
     2          IPEN = 1
            GO TO 118
  119       IF (IPEN .NE. 1) GO TO 123
C
C SPECIAL VALUE AREA
C
            GO TO (129,120,121,122),NSPV
  120       MSPV = 1
            GO TO 129
  121       PX(1) = I*NCRTG
            PY(1) = J*NCRTG
            PX(2) = PX(1)+NCRTG-1
            PY(2) = PY(1)+NCRTG-1
            CALL GPL (2,PX,PY)
            PYTMP = PY(1)
            PY(1) = PY(2)
            PY(2) = PYTMP
            CALL GPL (2,PX,PY)
C
            GO TO 129
  122       INTEN = MXLEV
            GO TO 128
  123       ZZ = ZLFT+XPART*(ZRHT-ZLFT)
C
C TEST FOR SAME INT AS LAST TIME.
C
            IF (ZZ .GT. ZLEV(LAST+1)) GO TO 126
  124       IF (ZZ .GE. ZLEV(LAST)) GO TO 127
C
C LOOK LOWER
C
            IF (LAST .LE. 1) GO TO 125
            LAST = LAST-1
            GO TO 124
  125       IF (ZZ .LT. ZLEV(LAST)) IADD = 0
            GO TO 127
C
C LOOK HIGHER
C
  126       IF (LAST .GE. NLEVL) GO TO 127
            LAST = LAST+1
            IF (ZZ .GE. ZLEV(LAST+1)) GO TO 126
C
C OK
C
  127       ISUB = LAST+IOFFST+IADD
            INTEN = IL(ISUB)
            IF (NOPTN .LT. 0) INTEN = MXLEV-INTEN
  128       CALL GRAY
  129    CONTINUE
  130 CONTINUE
C
C PUT OUT ANY REMAINING BUFFERED POINTS.
C
      IF (NPOINT.GT.0) THEN
         CALL GQNT(1,IERR,WND2,VWPR2)
         CALL SET(0.,1.,0.,1.,0.,1023.,0.,1023.,1)
         CALL POINTS(XPNT,YPNT,NPOINT,0,0)
         CALL SET(VWPR2(1),VWPR2(2),VWPR2(3),VWPR2(4),
     -            WND2(1),WND2(2),WND2(3),WND2(4),1)
      ENDIF
C
C CALL BOUND IF ISPV=2 AND SPECIAL VALUES WERE FOUND.
C
      IF (MSPV .EQ. 1) THEN
        CALL SET(X1,X2,Y1,Y2,1.0,X3,1.0,Y3,1)
        CALL BOUND (Z,MX,NX,NY,SP)
      END IF
  132 CONTINUE
C
C RESTORE NORMALIZATION TRANS 1 AND ORIGINAL NORMALIZATION NUMBER
C
      CALL SET(VWPRT(1),VWPRT(2),VWPRT(3),VWPRT(4),
     -         WNDW(1),WNDW(2),WNDW(3),WNDW(4),IOLLS)
      CALL SETUSV('LS',IOLLS)
      CALL GSELNT (NTORIG)
      RETURN
C
      END
      SUBROUTINE ZLSET (Z,MX,NX,NY,ZL,NLEVL)
      SAVE
C
      DIMENSION       Z(MX,NY)   ,ZL(NLEVL)
C
      COMMON /HAFTO2/ GLO        ,HA         ,NOPTN      ,ALPHA      ,
     1                NSPV       ,SP         ,ICNST
C
      BIG = R1MACH(2)
C
C ZLSET PUTS THE INTENSITY LEVEL BREAK POINTS IN ZL.
C ALL ARGUMENTS ARE AS IN HAFTON.
C
      LX = NX
      LY = NY
      NLEV = NLEVL
      NOPT = IABS(NOPTN)
      RALPH = 1./ALPHA
      ICNST = 0
      IF (GLO.NE.0. .OR. HA.NE.0.) GO TO 106
C
C FIND RANGE IF NOT KNOWN.
C
      GLO = BIG
      HA = -GLO
      IF (NSPV .NE. 0) GO TO 103
      DO 102 J=1,LY
         DO 101 I=1,LX
            ZZ = Z(I,J)
            GLO = AMIN1(ZZ,GLO)
            HA = AMAX1(ZZ,HA)
  101    CONTINUE
  102 CONTINUE
      GO TO 106
  103 DO 105 J=1,LY
         DO 104 I=1,LX
            ZZ = Z(I,J)
            IF (ZZ .EQ. SP) GO TO 104
            GLO = AMIN1(ZZ,GLO)
            HA = AMAX1(ZZ,HA)
  104    CONTINUE
  105 CONTINUE
C
C FILL ZL
C
  106 DELZ = HA-GLO
      IF (DELZ .EQ. 0.) GO TO 115
      DZ = DELZ/FLOAT(NLEV)
      NLEVM1 = NLEV-1
      DO 114 K=1,NLEVM1
         ZNORM = FLOAT(K)/FLOAT(NLEV)
         GO TO (107,108,109,110,111),NOPT
C
C NOPT=1
C
  107    ZL(K) = GLO+FLOAT(K)*DZ
         GO TO 114
C
C NOPT=2
C
  108    ONORM = (1.-(1.-ZNORM)**ALPHA)**RALPH
         GO TO 113
C
C NOPT=3
C
  109    ONORM = 1.-(1.-ZNORM**ALPHA)**RALPH
         GO TO 113
C
C NOPT=4
C
  110    ONORM = .5*(1.-(ABS(ZNORM+ZNORM-1.))**ALPHA)**RALPH
         GO TO 112
C
C NOPT=5
C
  111    ZNORM2 = ZNORM+ZNORM
         IF (ZNORM .GT. .5) ZNORM2 = 2.-ZNORM2
         ONORM = .5*(1.-(1.-ABS(ZNORM2)**ALPHA)**RALPH)
  112    IF (ZNORM .GT. .5) ONORM = 1.-ONORM
  113    ZL(K) = GLO+DELZ*ONORM
  114 CONTINUE
      ZL(NLEV) = BIG
      RETURN
  115 ICNST = 1
      RETURN
      END
      SUBROUTINE GRAY
C
C SUBROUTINE GRAY COLORS HALF-TONE CELL (I,J) WITH INTENSITY INTEN.
C THE ROUTINE ASSUMES 8X8 CELL SIZE ON A VIRTUAL SCREEN 1024X1024.
C
      DIMENSION       IFOT(16)   ,JFOT(16)
      DIMENSION       WNDW(4)    ,VWPRT(4)
CCC      DIMENSION       MX(16)  ,MY(16)
      COMMON /HAFTO1/ I          ,J          ,INTEN
      COMMON /HAFTO4/ NPTMAX     ,NPOINT     ,XPNT(50)  ,YPNT(50)
      SAVE
C
      DATA
     1 IFOT(1),IFOT(2),IFOT(3),IFOT(4),IFOT(5),IFOT(6),IFOT(7),IFOT(8)/
     2   1,      5,      1,      5,      3,      7,      3,      7   /
      DATA
     1 IFOT(9),IFOT(10),IFOT(11),IFOT(12),IFOT(13),IFOT(14),IFOT(15)/
     2   3,      7,       3,       7,       1,       5,       1/,
     3 IFOT(16)/
     4   5    /
C
      DATA
     1 JFOT(1),JFOT(2),JFOT(3),JFOT(4),JFOT(5),JFOT(6),JFOT(7),JFOT(8)/
     2   1,      5,      5,      1,      3,     7,      7,      3   /
      DATA
     1 JFOT(9),JFOT(10),JFOT(11),JFOT(12),JFOT(13),JFOT(14),JFOT(15)/
     2   1,      5,       5,       1,       3,       7,       7/,
     3 JFOT(16)/
     4   3    /
C
      IF (INTEN) 103,103,101
  101 I1 = I*8
      J1 = J*8
      IF ((NPOINT+INTEN) .LE.NPTMAX)  GO TO 1015
      CALL GQNT(1,IERR,WNDW,VWPRT)
      CALL SET(0.,1.,0.,1.,0.,1023.,0.,1023.,1)
      CALL POINTS(XPNT,YPNT,NPOINT,0,0)
      CALL SET(VWPRT(1),VWPRT(2),VWPRT(3),VWPRT(4),
     -         WNDW(1),WNDW(2),WNDW(3),WNDW(4),1)
      NPOINT = 0
 1015 DO 102 I2=1,INTEN
      NPOINT = NPOINT + 1
      XPNT(NPOINT) = I1+IFOT(I2)
      YPNT(NPOINT) = J1+JFOT(I2)
  102 CONTINUE
  103 RETURN
      END
      SUBROUTINE BOUND (Z,MX,NNX,NNY,SSP)
      DIMENSION       Z(MX,NNY)   ,PX(2)   ,PY(2)
C
C BOUND DRAWS A POLYGONAL BOUNDRY AROUND ANY SPECIAL-VALUE AREAS IN Z.
C
      SAVE
      NX = NNX
      NY = NNY
C
C VERTICAL LINES
C
      SP = SSP
      DO 103 IP1=3,NX
         I = IP1-1
         PX(1) = I
         PX(2) = I
         IM1 = I-1
         DO 102 JP1=2,NY
            PY(2) = JP1
            J = JP1-1
            PY(1) = J
            KLEFT = 0
            IF (Z(IM1,J).EQ.SP .OR. Z(IM1,JP1).EQ.SP) KLEFT = 1
            KCENT = 0
            IF (Z(I,J).EQ.SP .OR. Z(I,JP1).EQ.SP) KCENT = 1
            KRIGT = 0
            IF (Z(IP1,J).EQ.SP .OR. Z(IP1,JP1).EQ.SP) KRIGT = 1
            JUMP = KLEFT*4+KCENT*2+KRIGT+1
            GO TO (102,101,102,102,101,102,102,102,102),JUMP
  101       CALL GPL (2,PX,PY)
  102    CONTINUE
  103 CONTINUE
C
C HORIZONTAL
C
      DO 106 JP1=3,NY
         J = JP1-1
         PY(1) = J
         PY(2) = J
         JM1 = J-1
         DO 105 IP1=2,NX
            PX(2) = IP1
            I = IP1-1
            PX(1) = I
            KLOWR = 0
            IF (Z(I,JM1).EQ.SP .OR. Z(IP1,JM1).EQ.SP) KLOWR = 1
            KCENT = 0
            IF (Z(I,J).EQ.SP .OR. Z(IP1,J).EQ.SP) KCENT = 1
            KUPER = 0
            IF (Z(I,JP1).EQ.SP .OR. Z(IP1,JP1).EQ.SP) KUPER = 1
            JUMP = KLOWR*4+KCENT*2+KUPER+1
            GO TO (105,104,105,105,104,105,105,105,105),JUMP
  104       CALL GPL (2,PX,PY)
  105    CONTINUE
  106 CONTINUE
      RETURN
      END
      SUBROUTINE EZHFTN (Z,M,N)
C
      DIMENSION       Z(M,N)
      SAVE
C
C HALF-TONE PICTURE VIA SHORTEST ARGUMENT LIST.
C ASSUMPTIONS--
C     ALL OF THE ARRAY IS TO BE DRAWN,
C     LOWEST VALUE IN Z WILL BE AT LOWEST INTENSITY ON READER/PRINTER
C     OUTPUT, HIGHEST VALUE IN Z WILL BE AT HIGHEST INTENSITY, VALUES IN
C     BETWEEN WILL APPEAR LINEARLY SPACED, MAXIMUM POSSIBLE NUMBER OF
C     INTENSITIES ARE USED, THE PICTURE WILL HAVE A PERIMETER DRAWN,
C     FRAME WILL BE CALLED AFTER THE PICTURE IS DRAWN, Z IS FILLED WITH
C     NUMBERS THAT SHOULD BE USED (NO UNKNOWN VALUES).
C IF THESE CONDITIONS ARE NOT MET, USE HAFTON.
C EZHFTN ARGUMENTS--
C     Z   2 DIMENSIONAL ARRAY TO BE USED TO GENERATE A HALF-TONE PLOT.
C     M   FIRST DIMENSION OF Z.
C     N   SECOND DIMENSION OF Z.
C
      DATA FLO,HI,NLEV,NOPT,NPRM,ISPV,SPV/0.0,0.0,0,0,0,0,0.0/
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('GRAPHX','HAFTON','EZHFTN','VERSION  1')
C
      CALL HAFTON (Z,M,M,N,FLO,HI,NLEV,NOPT,NPRM,ISPV,SPV)
C
C +NOAO - EZHFTN no longer calls frame.
C     CALL FRAME
C -NOAO
      RETURN
      END
C
C-----------------------------------------------------------------------
C
C REVISION HISTORY---
C
C JULY     1984    CONVERTED TO FORTAN 77 AND GKS
C
C MARCH    1983    INSTITUTED BUFFERING OF POINTS WITHIN ROUTINE GRAY,
C                  WHICH DRAMATICALLY REDUCES SIZE OF OUTPUT PLOT CODE,
C                  METACODE.  THIS IN TURN GENERALLY IMPROVES THROUGHPUT
C                  OF METACODE INTERPRETERS.
C
C FEBRUARY 1979    MODIFIED CODE TO CONFORM TO FORTRAN 66 STANDARD
C
C JANUARY  1978    DELETED REFERENCES TO THE  *COSY  CARDS AND
C                  ADDED REVISION HISTORY
C
C-----------------------------------------------------------------------
C
