      SUBROUTINE GRIDAL(MAJRX,MINRX,MAJRY,MINRY,IXLAB,IYLAB,IGPH,X,Y)
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
C LATEST REVISION        JULY, 1985
C
C PURPOSE                THIS IS A PACKAGE OF ROUTINES FOR DRAWING
C                        GRAPH PAPER, AXES, AND OTHER BACKGROUNDS.
C
C USAGE                  EACH USER ENTRY POINT IN THIS PACKAGE (GRID,
C                        GRIDL, PERIM, PERIML, HALFAX, LABMOD,
C                        TICK4, AND GRIDAL) WILL BE DESCRIBED
C                        SEPARATELY BELOW.  FIRST, HOWEVER, WE
C                        WILL DISCUSS HOW MAJOR AND MINOR DIVISIONS
C                        IN THE GRAPH PAPER ARE HANDLED BY ALL
C                        ENTRIES WHICH USE THEM.
C
C                        GRIDAL, GRID, GRIDL, PERIM, PERIML, AND
C                        HALFAX HAVE ARGUMENTS  MAJRX,MINRX,MAJRY,
C                        MINRY WHICH CONTROL THE NUMBER OF MAJOR AND
C                        MINOR DIVISIONS IN THE GRAPH PAPER OR
C                        PERIMETERS.  THE NUMBER OF DIVISIONS REFERS
C                        TO THE HOLES BETWEEN LINES RATHER THAN THE
C                        LINES THEMSELVES.  THIS MEANS THAT THERE
C                        IS ALWAYS ONE MORE MAJOR DIVISION LINE THAN
C                        THE NUMBER OF MAJOR DIVISIONS.  SIMILARLY,
C                        THERE IS ONE LESS MINOR DIVISION LINE THAN
C                        MINOR DIVISIONS (PER MAJOR DIVISION.)
C
C                        MAJRX,MAJRY,MINRX,MINRY HAVE DIFFERENT
C                        MEANINGS DEPENDING UPON WHETHER LOG
C                        SCALING IS IN EFFECT (SET VIA SETUSV OR
C                        SET IN THE SPPS PACKAGE.)
C
C                        FOR LINEAR SCALING,
C                        MAJRX AND MAJRY SPECIFY THE NUMBER OF MAJOR
C                        DIVISIONS ALONG THE X-AXIS OR Y-AXIS
C                        RESPECTIVELY, AND MINRX AND MINRY SPECIFY
C                        THE NUMBER OF MINOR DIVISIONS PER MAJOR
C                        DIVISION.
C
C                        FOR LOG SCALING ALONG THE X-AXIS
C                        EACH MAJOR DIVISION OCCURS AT A FACTOR OF
C                        10**MAJRX TIMES THE PREVIOUS DIVISION.
C                        FOR EXAMPLE, IF THE MINIMUM X-AXIS VALUE IS
C                        3., AND THE MAXIMUM X-AXIS VALUE IS 3000.,
C                        AND MAJRX IS 1, THEN MAJOR DIVISIONS WILL
C                        OCCUR AT 3., 30., 300., AND 3000.  SIMILARLY
C                        FOR MAJRY.   IF LOG SCALING IS IN EFFECT ON
C                        THE X-AXIS AND MINRX.LE.10, THEN THERE ARE
C                        NINE MINOR DIVISIONS BETWEEN EACH MAJOR
C                        DIVISION.  FOR EXAMPLE, BETWEEN 3. AND 30.
C                        THERE WOULD BE A MINOR DIVISION AT 6., 9.,
C                        12.,...,27.  IF LOG SCALING IS IN EFFECT ON
C                        THE X-AXIS AND MINRX.GT.10, THEN THERE WILL
C                        BE NO MINOR SUBDIVISIONS.  MINRY IS TREATED
C                        IN THE SAME MANNER AS MINRX.
C
C                        IF DIFFERENT COLORS (OR INTENSITIES) ARE TO
C                        BE USED FOR NORMAL INTENSITY, LOW INTENSITY,
C                        OR TEXT COLOR, THEN THE VALUES IN COMMON
C                        BLOCK  GRIINT  SHOULD BE CHANGED AS FOLLOWS:
C
C                          IGRIMJ      COLOR INDEX FOR NORMAL (MAJOR)
C                                      INTENSITY LINES.
C                          IGRIMN      COLOR INDEX FOR LOW INTENSITY
C                                      LINES.
C                          IGRITX      COLOR INDEX FOR TEXT (LABELS.)
C
C WE NOW DESCRIBE EACH ENTRY IN THIS PACKAGE.
C
C-----------------------------------------------------------------------
C SUBROUTINE GRID
C-----------------------------------------------------------------------
C
C PURPOSE       TO DRAW GRAPH PAPER.
C
C USAGE         CALL GRID (MAJRX,MINRX,MAJRY,MINRY)
C
C DESCRIPTION   THIS SUBROUTINE DRAWS GRAPH LINES IN THE PORTION
C               OF THE PLOTTER SPECIFIED BY THE CURRENT VIEWPORT
C               SETTING WITH THE NUMBER OF MAJOR AND MINOR
C               DIVISIONS AS SPECIFIED BY THE ARGUMENTS.
C
C-----------------------------------------------------------------------
C SUBROUTINE GRIDAL
C-----------------------------------------------------------------------
C
C PURPOSE       A GENERAL ENTRY POINT FOR ALL BACKGROUND ROUTINES
C               WITH THE OPTION OF LINE LABELLING ON EACH AXIS.
C
C USAGE         CALL GRIDAL (MAJRX,MINRX,MAJRY,MINRY,IXLAB,IYLAB,
C                            IGPH,X,Y)
C
C ARGUMENTS     MAJRX,MINRX,MAJRY,MINRY
C                 MAJOR AND MINOR AXIS DIVISIONS AS DESCRIBED IN THE
C                 USAGE SECTION OF THE PACKAGE DOCUMENTATION ABOVE.
C
C               IXLAB,IYLAB (INTEGERS)
C                 FLAGS FOR AXIS LABELS:
C
C                   IXLAB = -1  NO X-AXIS DRAWN
C                               NO X-AXIS LABELS
C
C                         =  0  X-AXIS DRAWN
C                               NO X-AXIS LABELS
C
C                         =  1  X-AXIS DRAWN
C                               X-AXIS LABELS
C
C                   IYLAB = -1  NO Y-AXIS DRAWN
C                               NO Y-AXIS LABELS
C
C                         =  0  Y-AXIS DRAWN
C                               NO Y-AXIS LABELS
C
C                         =  1  Y-AXIS DRAWN
C                               Y-AXIS LABELS
C
C
C               IGPH
C                 FLAG FOR BACKGROUND TYPE:
C
C                    IGPH         X-AXIS BACKGROUND  Y-AXIS BACKGROUND
C                    ----         -----------------  -----------------
C                     0           GRID               GRID
C                     1           GRID               PERIM
C                     2           GRID               HALFAX
C                     4           PERIM              GRID
C                     5           PERIM              PERIM
C                     6           PERIM              HALFAX
C                     8           HALFAX             GRID
C                     9           HALFAX             PERIM
C                    10           HALFAX             HALFAX
C
C               X,Y
C                 WORLD COORDINATES OF THE INTERSECTION OF THE AXES
C                 IF  IGPH=10 .
C
C-----------------------------------------------------------------------
C SUBROUTINE GRIDL
C-----------------------------------------------------------------------
C
C PURPOSE       TO DRAW GRAPH PAPER.
C
C USAGE         CALL GRIDL (MAJRX,MINRX,MAJRY,MINRY)
C
C DESCRIPTION   THIS SUBROUTINE BEHAVES EXACTLY AS GRID, BUT EACH
C               MAJOR DIVISION IS LABELED WITH ITS NUMERICAL VALUE.
C
C-----------------------------------------------------------------------
C SUBROUTINE HALFAX
C-----------------------------------------------------------------------
C
C PURPOSE       TO DRAW ORTHOGONAL AXES.
C
C USAGE         CALL HALFAX (MAJRX,MINRX,MAJRY,MINRY,X,Y,IXLAB,IYLAB)
C
C DESCRIPTION   THIS SUBROUTINE DRAWS ORTHOGONAL AXES INTERSECTING
C               AT COORDINATE (X,Y) WITH OPTIONAL LABELING OPTIONS AS
C               SPECIFIED BY IXLAB AND IYLAB.
C
C ARGUMENTS     MAJRX,MINRX,MAJRY,MINRY
C                 MAJOR AND MINOR DIVISION SPECIFICATIONS AS PER THE
C                 DESCRIPTION IN THE PACKAGE USAGE SECTION ABOVE.
C
C               X,Y
C                 WORLD COORDINATES SPECIFYING THE INTERSECTION POINT
C                 OF THE X AND Y AXES.
C
C               IXLAB,IYLAB (INTEGERS)
C                 FLAGS FOR AXIS LABELS:
C
C                   IXLAB = -1  NO X-AXIS DRAWN
C                               NO X-AXIS LABELS
C
C                         =  0  X-AXIS DRAWN
C                               NO X-AXIS LABELS
C
C                         =  1  X-AXIS DRAWN
C                               X-AXIS LABELS
C
C                   IYLAB = -1  NO Y-AXIS DRAWN
C                               NO Y-AXIS LABELS
C
C                         =  0  Y-AXIS DRAWN
C                               NO Y-AXIS LABELS
C
C                         =  1  Y-AXIS DRAWN
C                               Y-AXIS LABELS
C
C-----------------------------------------------------------------------
C SUBROUTINE LABMOD
C-----------------------------------------------------------------------
C
C PURPOSE       TO ALLOW MORE COMPLETE CONTROL OVER THE APPEARANCE
C               OF THE LABELS ON THE BACKGROUND PLOTS.
C
C USAGE         CALL LABMOD (FMTX,FMTY,NUMX,NUMY,ISIZX,ISIZY,
C                            IXDEC,IYDEC,IXOR)
C
C DESCRIPTION   THIS SUBROUTINE PRESETS PARAMETERS FOR THE OTHER
C               BACKGROUND ROUTINES IN THIS PACKAGE.  LABMOD ITSELF
C               DOES NO PLOTTING AND IT MUST BE CALLED BEFORE THE
C               THE BACKGROUND ROUTINES FOR WHICH IT IS PRESETTING
C               PARAMETERS.
C
C ARGUMENTS     FMTX,FMTY  (TYPE CHARACTER)
C                 FORMAT SPECIFICATIONS FOR THE X-AXIS AND Y-AXIS
C                 NUMERICAL LABELS IN GRIDL, PERIML, GRIDAL, OR
C                 HALFAX.  THE SPECIFICATION MUST START WITH A LEFT
C                 PARENTHESIS AND END WITH A RIGHT PARENTHESIS AND
C                 SHOULD NOT USE MORE THAN 8 CHARACTERS.  ONLY
C                 FLOATING-POINT CONVERSIONS (F, E, AND G) SUCH AS
C                 FMTX='(F8.2)' AND  FMTY='(E10.0)' FOR EXAMPLE.
C
C               NUMX,NUMY (INTEGER)
C                 THE NUMBER OF CHARACTERS SPECIFIED BY  FMTX  AND
C                 FMTY.  FOR THE ABOVE EXAMPLES, THESE WOULD BE
C                 NUMX=8  AND  NUMY=10  (NOT 6 AND 7).
C
C               ISIZX,ISIZY
C                 CHARACTER SIZE CODES FOR THE LABELS.  THESE SIZE
C                 CODES ARE THE SAME AS THOSE FOR THE SPPS ENTRY
C                 PWRIT.
C
C               IXDEC
C                 THE DECREMENT IN PLOTTER ADDRESS UNITS FROM THE
C                 LEFTMOST PLOTTER COORDINATE (AS SPECIFIED BY THE
C                 CURRENT VIEWPORT) TO THE NEAREST X-ADDRESS OF THE
C                 LABEL SPECIFIED BY  FMTY, NUMY, AND ISIZY.  FOR
C                 EXAMPLE, IF THE MINIMUM X-COORDINATE OF THE CURRENT
C                 VIEWPORT IS .1,  MINX  IS 102 (.1*1024).  IF  IXDEC
C                 IS 60, THE LABEL WILL START AT 42 (102-60).  THE
C                 FOLLOWING CONVENTIONS ARE USED:
C
C                 O  IF  IXDEC=0, IT IS AUTOMATICALLY RESET TO PROPERLY
C                    POSITION THE  Y-AXIS  LABELS TO THE LEFT OF THE
C                    LEFT Y-AXIS,  IXDEC=20 .
C
C                 O  IF  IXDEC=1,  Y-AXIS LABELS WILL GO TO THE RIGHT
C                    OF THE GRAPH, IXDEC=-20 .
C
C                 WHEN EITHER  HALFAX  OR  GRIDAL  IS CALLED TO DRAW AN
C                 AXIS,  IXDEC  IS THE DISTANCE FROM THE AXIS RATHER
C                 THAN FROM THE MINIMUM VIEWPORT COORDINATE.
C
C               IYDEC
C                 THE DECREMENT IN PLOTTER ADDRESS UNITS FROM THE
C                 MINIMUM Y-AXIS COORDINATE AS SPECIFIED BY THE
C                 CURRENT VIEWPORT TO THE NEAREST Y-ADDRESS OF THE
C                 LABEL SPECIFIED BY  FMTX, NUMX, AND ISIZX.  FOR
C                 EXAMPLE, IF THE MINIMUM Y-COORDINATE OF THE
C                 CURRENT VIEWPORT IS .2,  MINY IS 205 (.2*1024).
C                 IF  IYDEC=30, THE LABEL WILL END AT  205-30=175.
C                 THE FOLLOWING CONVENTIONS ARE USED:
C
C                 O  IF  IYDEC=0, IT IS AUTOMATICALLY RESET TO
C                    PROPERLY POSITION X-AXIS LABELS ALONG THE
C                    BOTTOM,  IYDEC=20 .
C
C                 O  IF  IYDEC=1, X-AXIS LABELS WILL GO ALONG THE
C                    TOP OF THE GRAPH,  IYDEC=-20 .
C
C               IXOR (INTEGER)
C                 ORIENTATION OF THE X-AXIS LABELS.
C
C                   IXOR = 0    +X (HORIZONTAL)
C                        = 1    +Y (VERTICAL)
C
C                 IN NORMAL ORIENTATION, THE ACTUAL NUMBER OF
C                 NON-BLANK DIGITS IS CENTERED UNDER THE LINE
C                 OR TICK TO WHICH IT APPLIES.
C
C-----------------------------------------------------------------------
C SUBROUTINE PERIM
C-----------------------------------------------------------------------
C
C PURPOSE       TO DRAW A PERIMETER WITH TICK MARKS.
C
C USAGE         CALL PERIM (MAJRX,MINRX,MAJRY,MINRY)
C
C DESCRIPTION   THIS SUBROUTINE BEHAVES JUST AS GRID EXCEPT THAT
C               INTERIOR LINES ARE REPLACED WITH TICK MARKS ALONG
C               THE EDGES.  TICK MARKS AT MAJOR DIVISIONS ARE
C               SLIGHTLY LARGER THAN TICK MARKS AT MINOR DIVISIONS.
C
C-----------------------------------------------------------------------
C SUBROUTINE PERIML
C-----------------------------------------------------------------------
C
C PURPOSE       TO DRAW A PERIMETER WITH TICK MARKS AND LABELS.
C
C USAGE         CALL PERIML (MAJRX,MINRX,MAJRY,MINRY)
C
C DESCRIPTION   THIS SUBROUTINE BEHAVES JUST AS PERIM, BUT EACH
C               MAJOR DIVISION IS LABELED WITH ITS NUMERICAL VALUE.
C
C-----------------------------------------------------------------------
C SUBROUTINE TICK4
C-----------------------------------------------------------------------
C
C PURPOSE       TO ALLOW PROGRAM CONTROL OF TICK MARK LENGTH.
C
C USAGE         CALL TICK4 (LMAJX,LMINX,LMAJY,LMINY)
C
C DESCRIPTION   THIS SUBROUTINE ALLOWS PROGRAM CONTROL OF TICK
C               MARK LENGTH IN  PERIM, PERIML, GRIDAL, AND HALFAX.
C
C ARGUMENTS     LMAJX,LMAJY
C                 LENGTH IN PLOTTER ADDRESS UNITS OF MAJOR DIVISION
C                 TICK MARKS ON THE X-AXIS AND Y-AXIS RESPECTIVELY.
C                 THESE VALUES ARE INITIALLY SET TO 12 .
C
C               MINRX,MINRY
C                 LENGTH IN PLOTTER ADDRESS UNITS OF MINOR DIVISION
C                 TICK MARKS ON THE X-AXIS AND Y-AXIS RESPECTIVELY.
C                 THESE VALUES ARE INITIALLY SET TO 8 .
C
C-----------------------------------------------------------------------
C
C WE NOW RESUME THE PACKAGE DOCUMENTATION.
C
C ENTRY POINTS  GRID,GRIDAL,GRIDL,HALFAX,LABMOD,PERIM,PERIML,TICK4,
C               TICKS,CHSTR,EXPAND,GRIDT
C
C COMMON BLOCKS LAB,CLAB,TICK,GRIINT
C
C REQUIRED      THE ERPRT77 PACKAGE AND THE SPPS.
C ROUTINES
C
C I/O           PLOTS BACKGROUNDS
C
C PRECISION     SINGLE
C
C LANGUAGE      FORTRAN 77
C
C HISTORY       WRITTEN IN JUNE, 1984.  BASED ON THE NCAR SYSTEM
C               PLOT PACKAGE ENTRIES HAVING THE SAME NAMES.
C
        COMMON /LAB/ SIZX,SIZY,XDEC,YDEC,IXORI
        COMMON /CLAB/ XFMT, YFMT
        COMMON /TICK/ MAJX, MINX, MAJY, MINY
        COMMON /GRIINT/ IGRIMJ, IGRIMN, IGRITX
C
C INTERNAL VARIABLES:
C
C               CHUPX,CHUPY     CHARACTER UP VECTOR VALUES ON ENTRY
C
C               CURMAJ          IF LOGMIN=.TRUE., THEN THIS IS THE
C                               CURRENT MAJOR TICK/GRID POSITION
C
C               ICNT            NORMALIZATION TRANSFORMATION NUMBER IN
C                               EFFECT ON ENTRY TO GRIDAL
C
C               LASF(13)        ASPECT SOURCE FLAG TABLE AS USED BY GKS.
C
C               LGRID           .TRUE. IF GRIDS ARE TO BE DRAWN ON THE
C                               CURRENT AXIS (OPPOSED TO TICKS)
C
C               LOGMIN          .TRUE.  IF LOG SCALING IS IN EFFECT AND
C                               MINOR TICK MARKS OR GRIDS ARE DESIRED
C
C               LOGVAL          LINEAR OR LOG SCALING
C                               1 = X LINEAR, Y LINEAR
C                               2 = X LINEAR, Y LOG
C                               3 = X LOG, Y LINEAR
C                               4 = X LOG, Y LOG
C
C               MINCNT          NUMBER OF MINOR DIVISIONS PER MAJOR
C
C               NERR            COUNTS ERROR NUMBER
C
C               NEXTMAJ         IF LOGMIN=.TRUE., THEN THIS IS THE NEXT
C                               MAJOR TICK/GRID POSITION
C
C               NWIND(4)        WINDOW LIMITS IN WORLD COORDINATES
C                               AFTER EXPANSION
C
C               OCOLI           COLOR INDEX ON ENTRY TO GRIDAL
C
C               OLDALH,OLDALV   TEXT ALIGNMENT VALUES ON ENTRY
C                               (HORIZONTAL AND VERTICAL)
C
C               OLDCH           CHARACTER HEIGHT ON ENTRY TO GRIDAL
C
C               OPLASF          STORES VALUE OF POLYLINE COLOR ASF ON
C                               ENTRY TO GRIDAL
C
C               OTXASF          STORES VALUE OF TEXT COLOR ASF ON
C                               ENTRY TO GRIDAL
C
C               OTXCOL          TEXT COLOR INDEX ON ENTRY TO GRIDAL
C
C               OWIND(4)        WINDOW LIMITS IN WORLD COORDINATES
C                               ON ENTRY TO GRIDAL
C
C               PY(2)           2 Y-COORDINATES FOR LINE TO BE DRAWN
C                               VIA GKS ROUTINE GPL
C
C               PX(2)           2 X-COORDINATES FOR LINE TO BE DRAWN
C                               VIA GKS ROUTINE GPL
C
C               START           IF DRAWING TICKS/GRIDS ON X-AXIS:
C                                       Y-COORD OF ORIGIN OF EACH LINE;
C                               IF DRAWING TICKS/GRIDS ON Y-AXIS:
C                                       X-COORD OF ORIGIN OF EACH LINE
C
C               TICBIG          END OF MAJOR TICK LINE IN WORLD
C                               COORDINATES
C
C               TICEND          END OF MINOR TICK LINE IN WORLD
C                               COORDINATES
C
C               TICMAJ          LENGTH OF MAJOR TICKS IN WORLD
C                               COORDINATES
C
C               TICMIN          LENGTH OF MINOR TICKS IN WORLD
C                               COORDINATES
C
C               VIEW(4)         VIEWPORT LIMITS IN NDC PRIOR TO
C                               EXPANSION FOR LABELLING
C
C               WIND(4)         SAME AS IN OWIND(4)
C
C               XCUR            A TICK/GRID IS DRAWN AT THIS POSITION
C                               IF LOG SCALING IS IN EFFECT.
C
C               XDEC            LENGTH IN WORLD COORDINATES FROM
C                               X-AXIS TO LABEL
C
C               XI              ALOG10(X), IF LOG SCALING
C
C               XINT            INTERVAL BETWEEN MINOR X-AXIS
C                               TICKS/GRIDS IN WORLD COORDINATES
C
C               XINTM           INTERVAL BETWEEN MAJOR X-AXIS
C                               TICKS/GRIDS IN WORLD COORDINATES
C
C               XMIRRO          LOGICAL FLAGS FOR MIRROR-IMAGE
C
C               XNUM            TOTAL NUMBER OF X-AXIS TICKS/GRIDS
C                               WITH LINEAR SCALING
C
C               XPOS            IF LINEAR SCALING, KEEPS TRACK OF X-AXIS
C                               POSITION FOR CURRENT TICK/GRID
C
C               XRANGE          TOTAL RANGE IN X DIRECTION IN WORLD
C                               COORDINATES PRIOR TO EXPANSION FOR
C                               LABELLING.
C
C               XRNEW           RANGE IN X DIRECTION IN WORLD
C                               COORDINATES, AFTER EXPANSION
C
C               YCUR            A TICK/GRID IS DRAWN AT THIS POSITION
C                               IF LOG SCALING IS IN EFFECT.
C
C               YDEC            LENGTH IN WORLD COORDINATES FROM
C                               Y-AXIS TO LABEL
C
C               YI              ALOG10(Y), IF LOG SCALING
C
C               YINTM           INTERVAL BETWEEN MAJOR Y-AXIS
C                               TICKS/GRIDS IN WORLD COORDINATES
C
C               YMIRRO          PLOTTING.
C
C               YNUM            TOTAL NUMBER OF Y-AXIS TICKS/GRIDS
C                               WITH LINEAR SCALING
C
C               YPOS            IF LINEAR SCALING, KEEPS TRACK OF Y-AXIS
C                               POSITION FOR CURRENT TICK/GRID
C
C               YRANGE          TOTAL RANGE IN Y DIRECTION IN WORLD
C                               COORDINATES PRIOR TO EXPANSION FOR
C                               LABELLING.
C
C               YRNEW           RANGE IN Y DIRECTION IN WORLD
C                               COORDINATES, AFTER EXPANSION
C
C               XLAB,YLAB       IF LABELLING X-AXIS, Y-COORDINATE FOR
C                               FOR TEXT POSITION;
C                               IF LABELLING Y-AXIS, X-COORDINATE FOR
C                               TEXT POSITION.
C
C
C
      CHARACTER*8 XFMT,YFMT
      REAL WIND(4), VIEW(4), PX(2), PY(2), NWIND(4), OWIND(4)
      REAL MAJX, MINX, MAJY, MINY
      INTEGER TCOUNT, XTNUM, YTNUM, FIRST, LAST
      INTEGER OPLASF, OTXASF, LASF(13), OCOLI, OTEXCI, OLDALH ,OLDALV
      LOGICAL LGRID,LOGMIN
      LOGICAL XMIRRO,YMIRRO
      REAL MAJDIV, NEXTMA
      CHARACTER*15 LABEL
C
      DATA TICMIN,TICMAJ,XCUR,YCUR,EXCUR,EYCUR/0.,0.,0.,0.,0.,0./
C
C +NOAO - Blockdata rewritten as run time initialization.
C       EXTERNAL GRIDT
	call gridt
C -NOAO
C  THE FOLLOWING IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR.
C
      CALL Q8QST4('GRAPHX','GRIDAL','GRIDAL','VERSION 01')
      XRNEW = 0.
      YRNEW = 0.
C
C  INITIALIZE ERROR COUNT.
C
      NERR = 0
C
C  CHECK FOR BAD VALUES OF IGPH.
C
      IF (IGPH.LT.0.OR.IGPH.EQ.3.OR.IGPH.EQ.7.OR.IGPH.GT.10) THEN
          NERR = NERR + 1
          CALL SETER(' GRIDAL--INVALID IGPH VALUE',NERR,2)
      ENDIF
C
C  GET STANDARD ERROR MESSAGE UNIT
C
      IERUNT = I1MACH(4)
      XMIRRO = .FALSE.
      YMIRRO = .FALSE.
C
C  SET POLYLINE COLOR ASF TO INDIVIDAUL.
C
      CALL GQASF(IERR,LASF)
      OPLASF = LASF(3)
      LASF(3) = 1
      OTXASF = LASF(10)
      LASF(10) = 1
      CALL GSASF(LASF)
C
C  INQUIRE CURRENT POLYLINE COLOR INDEX.
C
      CALL GQPLCI(IERR,OCOLI)
C
C  SET POLYLINE COLOR TO THE VALUE SPECIFIED IN COMMON.
C
      CALL GSPLCI(IGRIMJ)
C
C  INQUIRE CURRENT NORMALIZATION TRANSFORMATION NUMBER.
C
      CALL GQCNTN(IERR,ICNT)
C
C  INQUIRE CURRENT WINDOW AND VIEWPORT LIMITS.
C
      CALL GQNT(ICNT,IERR,WIND,VIEW)
C
C  STORE WINDOW VALUES
C
      DO 10 I = 1,4
      OWIND(I) = WIND(I)
   10 CONTINUE
C
C  LOG OR LINEAR SCALING?
C
C           1 = X LINEAR, Y LINEAR
C           2 = X LINEAR, Y LOG
C           3 = X LOG, Y LINEAR
C           4 = X LOG, Y LOG
C
      CALL GETUSV('LS',LOGVAL)
C
C  ADJUST WINDOW TO ACCOUNT FOR LOG SCALING.
C
      IF (LOGVAL .EQ. 2) THEN
        WIND(3) = 10.**WIND(3)
        WIND(4) = 10.**WIND(4)
      ELSE IF (LOGVAL .EQ. 3) THEN
        WIND(1) = 10.**WIND(1)
        WIND(2) = 10.**WIND(2)
      ELSE IF (LOGVAL .EQ. 4) THEN
        WIND(1) = 10.**WIND(1)
        WIND(2) = 10.**WIND(2)
        WIND(3) = 10.**WIND(3)
        WIND(4) = 10.**WIND(4)
      ENDIF
C
C  DETERMINE IF MIRROR-IMAGE MAPPING IS REQUIRED.
C
      IF (WIND(1) .GT. WIND(2)) THEN
        XMIRRO = .TRUE.
      ENDIF
      IF (WIND(3) .GT. WIND(4)) THEN
        YMIRRO = .TRUE.
      ENDIF
C
C  IF IGPH=10, CHECK FOR X(Y) VALUES IN RANGE (IF NOT, CHANGE TO
C  DEFAULT.
C
      IF (IGPH .EQ. 10) THEN
        XI = X
        YI = Y
        IF (((XI .LT. WIND(1) .OR. XI .GT. WIND(2)) .AND. .NOT.
     1  XMIRRO) .OR. (XMIRRO.AND.(XI.GT.WIND(1).OR.XI.LT.WIND(2))))
     2  THEN
          NERR = NERR + 1
          CALL SETER(' GRIDAL--X VALUE OUT OF WINDOW RANGE',NERR,1)
C +NOAO - FTN writes and format statements deleted.  Call to SETER okay.
C
C         WRITE(IERUNT,1001)NERR
C1001     FORMAT(' ERROR',I3,' IN GRIDAL--X VALUE OUT OF WINDOW RANGE')
          CALL ERROF
          XI = WIND(1)
        ENDIF
        IF (((YI .LT. WIND(3) .OR. YI .GT. WIND(4)) .AND. .NOT.
     1  YMIRRO).OR.(YMIRRO.AND.(YI.GT.WIND(3).OR.YI.LT.WIND(4))))
     2  THEN
          NERR = NERR + 1
          CALL SETER(' GRIDAL--Y VALUE OUT OF WINDOW RANGE',NERR,1)
C         WRITE(IERUNT,1002)NERR
C1002     FORMAT(' ERROR',I3,' IN GRIDAL--Y VALUE OUT OF WINDOW RANGE')
C -NOAO
          CALL ERROF
          YI = WIND(3)
        ENDIF
      ENDIF
      MX = MAJRX
      MY = MAJRY
      IF (LOGVAL .EQ. 4 .OR. LOGVAL .EQ. 3) THEN
        IF (MX .LT. 1) MX = 1
        IF (WIND(1) .LE. 0.) THEN
          NERR = NERR + 1
          CALL SETER(' GRIDAL--NON-POSITIVE WINDOW BOUNDARY WITH LOG SCA
     1LING',NERR,2)
        ELSE
          WIND(1) = ALOG10(WIND(1))
        ENDIF
        IF (WIND(2) .LE. 0.) THEN
          NERR = NERR + 1
          CALL SETER(' GRIDAL--NON-POSITIVE WINDOW BOUNDARY WITH LOG SCA
     1LING',NERR,2)
        ELSE
          WIND(2) = ALOG10(WIND(2))
        ENDIF
        IF (IGPH .EQ. 10) THEN
          XI = ALOG10(XI)
        ENDIF
      ENDIF
C
      IF(LOGVAL .EQ. 4 .OR. LOGVAL .EQ. 2) THEN
        IF (MY .LT. 1) MY = 1
        IF (WIND(3) .LE. 0.) THEN
          NERR = NERR + 1
          CALL SETER(' GRIDAL--NON-POSITIVE WINDOW BOUNDARY WITH LOG SCA
     1LING',NERR,2)
        ELSE
          WIND(3) = ALOG10(WIND(3))
        ENDIF
        IF (WIND(4) .LE. 0.) THEN
          NERR = NERR + 1
          CALL SETER(' GRIDAL--NON-POSITIVE WINDOW BOUNDARY WITH LOG SCA
     1LING',NERR,2)
        ELSE
          WIND(4) = ALOG10(WIND(4))
        ENDIF
        IF (IGPH .EQ. 10) THEN
          YI = ALOG10(YI)
        ENDIF
      ENDIF
C
C  DEFINE NORMALIZATION TRANSFORMATION NUMBER 1.
C
      CALL GSWN(1,WIND(1),WIND(2),WIND(3),WIND(4))
      CALL GSVP(1,VIEW(1),VIEW(2),VIEW(3),VIEW(4))
      CALL GSELNT(1)
C
C  CALCULATE X AND Y WORLD COORDINATE RANGES.
C
      XRANGE = WIND(2) - WIND(1)
      YRANGE = WIND(4) - WIND(3)
C
C  IF LABELS ARE REQUESTED, INQUIRE AND SAVE TEXT ATTRIBUTES.
C
      IF (IXLAB .EQ. 1 .OR. IYLAB .EQ. 1) THEN
        CALL GQCHH(IERR,OLDCHH)
        CALL GQCHUP(IERR,CHUPX,CHUPY)
        CALL GQTXAL(IERR,OLDALH,OLDALV)
        CALL GQTXCI (IERR,OTEXCI)
        CALL GSTXCI (IGRITX)
C
C  EXPAND WINDOW AND VIEWPORT FOR LABELS AND CALCULATE NEW
C  X AND Y WORLD COORDINATE RANGES.
C
        CALL EXPAND(NWIND)
        XRNEW = NWIND(2) - NWIND(1)
        YRNEW = NWIND(4) - NWIND(3)
C
C  SET CHARACTER HEIGHT (1% OF Y RANGE.)
C
        CHARH = SIZX * YRNEW
        IF (YMIRRO) THEN
          CHARH = -CHARH
        ENDIF
        CALL GSCHH(CHARH)
      ENDIF
C
      IF (IGPH .EQ. 0) GOTO 50
C
C  CALCULATE TIC LENGTH.
C
C  IF NO LABELS AND TICK4 (OR TICKS) WERE NOT CALLED.
C
      IF (MAJX .EQ. 0.) THEN
        MAJX = .013
        MINX = .007
        TICMIN = MINX * YRANGE
        TICMAJ = MAJX * YRANGE
      ELSE
C
C  EXPAND WINDOW IF NOT ALREADY EXPANDED.
C  (IF LABMOD WAS NOT CALLED BUT TICK4(S) WAS.)
C
        IF (IXLAB.NE.1 .AND. IYLAB.NE.1) THEN
          CALL EXPAND (NWIND)
          XRNEW = NWIND(2) - NWIND(1)
          YRNEW = NWIND(4) - NWIND(3)
        ENDIF
        TICMIN = MINX * YRNEW
        TICMAJ = MAJX * YRNEW
      ENDIF
C
C  **** X-AXIS TICS/GRIDS AND LABELS ****
C
C  CALCULATE TIC/GRID INTERVALS ON X AXIS.
C
  50  IF (IXLAB .EQ. -1) GOTO 175
      MINCNT = MINRX
      IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ. 2) THEN
        LOGMIN = .FALSE.
        XINTM = XRANGE/MX
        XINT = XINTM
        IF (MINCNT .GT. 1) THEN
          XINT = XINT/MINCNT
        ENDIF
C
C  CALCULATE TOTAL NUMBER OF TICS/GRIDS ON AXIS.
C
        XTNUM = MX * MINCNT
        IF (MINCNT .EQ. 0) XTNUM = MX
      ELSE
        XTNUM = 50
        XCUR = 10.**OWIND(1)
        MAJDIV = 10 ** MX
        IF (MINCNT .LE. 10 .AND. MX .LE. 1) THEN
          LOGMIN = .TRUE.
          CURMAJ = XCUR
          NEXTMA = XCUR * MAJDIV
          XINT = (NEXTMA - CURMAJ) / 9.
          MINCNT = 9
        ELSE
          LOGMIN = .FALSE.
          MINCNT = 1
        ENDIF
      ENDIF
C
      LGRID = .FALSE.
      LOOP = 1
C
C  DETERMINE ORIGIN OF TICK/GRID LINES (Y COORDINATE.)
C
      IF (IGPH .NE. 10) THEN
        START = WIND(3)
      ELSE
        START = YI
      ENDIF
C
      XPOS = WIND(1)
      PY(1) = START
      TICEND = START + TICMIN
      TICBIG = START + TICMAJ
C
      PX(1) = XPOS
      PX(2) = PX(1)
C
C  DRAW LEFT-MOST TICK ON X-AXIS (IF IGPH = 10 AND
C  INTERSECTION OF AXES IS NOT AT BOTTOM LEFT OF WINDOW.)
C
      IF (IGPH .EQ. 10) THEN
        IF (XI .NE. WIND(1)) THEN
          PY(2) = TICBIG
          CALL GPL(2,PX,PY)
        ENDIF
C
C  DRAW X-AXIS FOR IGPH = 10
C
        PX(2) = WIND(2)
        PY(2) = PY(1)
        CALL GPL(2,PX,PY)
        PX(2) = PX(1)
      ELSE
C
C  DRAW Y-AXIS FOR ANY OTHER IGPH (FIRST TICK.)
C
        PY(2) = WIND(4)
        CALL GPL(2,PX,PY)
      ENDIF
C
C  TICKS OR GRIDS ?
C
      IF (IGPH .EQ. 0 .OR. IGPH .EQ. 1 .OR. IGPH .EQ.2) THEN
        PY(2) = WIND(4)
        LGRID = .TRUE.
      ELSE
        PY(2) = TICEND
      ENDIF
C
      IF (IXLAB .EQ. 1) THEN
C
C  IF VERTICAL X-AXIS LABEL ORIENTATION, THEN SET CHAR UP VECTOR
C  TO BE VERTICAL AND TEXT ALIGNMENT TO (RIGHT,HALF),
C  OTHERWISE TO (CENTER,TOP)
C
        IF (YMIRRO) THEN
          IF (IXORI .EQ. 1) THEN
            CALL GSCHUP(1.,0.)
            CALL GSTXAL(3,3)
          ELSE
            CALL GSCHUP(0.,-1.)
            CALL GSTXAL(2,1)
          ENDIF
        ELSE
          IF (IXORI .EQ. 1) THEN
            CALL GSCHUP(-1.,0.)
            CALL GSTXAL(3,3)
          ELSE
            CALL GSTXAL(2,1)
          ENDIF
        ENDIF
        IF (XDEC.NE.0. .AND. XDEC.NE.1.) THEN
          DEC = XDEC * YRNEW
        ELSE
          DEC = .02 * YRNEW
        ENDIF
        IF (XDEC .NE. 1.) THEN
          XLAB = START - DEC
        ELSE
          IF (IGPH .NE. 10) THEN
            XLAB = WIND(4)+DEC
          ELSE
            XLAB = YI+DEC
          ENDIF
C
C  IF LABELS ARE ON TOP OF THE X-AXIS, SET THE TEXT
C  ALIGNMENT TO (LEFT,HALF) IF THE X-AXIS LABELS ARE
C  VERTICAL, OTHERWISE TO (CENTER,BASE).
C
          IF (IXORI .EQ. 1) THEN
            CALL GSTXAL(1,3)
          ELSE
            CALL GSTXAL(2,4)
          ENDIF
        ENDIF
        IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ. 2) THEN
C +NOAO
C         WRITE(LABEL,XFMT)XPOS
          call encode (10, xfmt, label, xpos)
C -NOAO
        ELSE
C +NOAO
C         WRITE(LABEL,XFMT)XCUR
          call encode (10, yfmt, label, xcur)
C -NOAO
        ENDIF
        CALL CHSTR(LABEL,FIRST,LAST)
        CALL GTX (XPOS,XLAB,LABEL(FIRST:LAST))
      ENDIF
C
 80   TCOUNT = 1
C
      DO 100 I = 1,XTNUM
      IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ. 2) THEN
        XPOS = XPOS + XINT
      ELSE
        IF (.NOT. LOGMIN) THEN
          XCUR = XCUR * MAJDIV
        ELSE
          IF (TCOUNT .NE. MINCNT) THEN
            XCUR = XCUR + XINT
          ELSE
            XCUR = XCUR + XINT
            CURMAJ = NEXTMA
            NEXTMA = CURMAJ * MAJDIV
            XINT = (NEXTMA - CURMAJ) / 9.
          ENDIF
        ENDIF
        IF (XCUR .GT. 10.**OWIND(2)-.1*XINT) THEN
          XPOS = WIND(2)
        ELSE
          XPOS = ALOG10(XCUR)
        ENDIF
      ENDIF
C
      PX(1) = XPOS
      PX(2) = XPOS
C
C  IF IGPH = 0,1,2,4,5,8 OR 9 AND XPOS=RIGHT AXIS, THEN
C  DRAW AXIS, ELSE IF IGPH = 6 OR 10 DRAW TIC AND LABEL.
C
      IF (LOGVAL .EQ. 3 .OR. LOGVAL .EQ. 4) EXCUR = 10.**OWIND(2)
C
      IF ((((LOGVAL .EQ. 1.OR.LOGVAL.EQ.2) .AND. (I .EQ. XTNUM))
     1 .OR.((LOGVAL .EQ.4 .OR.LOGVAL .EQ.3).AND.XCUR.GE.EXCUR-.1*XINT))
     2 .AND.(IGPH.NE.6.AND.IGPH.NE.10)) THEN
        IF (LOOP .EQ. 1) THEN
          PY(2) = WIND(4)
          CALL GPL(2,PX,PY)
          IF (IXLAB .EQ. 1) THEN
            IF (LOGVAL.EQ.1 .OR. LOGVAL.EQ.2) THEN
C (NOAO)      WRITE(LABEL,XFMT) XPOS
              call encode (10, xfmt, label, xpos)
            ELSE
              IF (XCUR .GT. EXCUR+.1*XINT) THEN
                GOTO 101
              ELSE
C (NOAO)        WRITE(LABEL,XFMT) XCUR
                call encode (10, xfmt, label, xcur)
              ENDIF
            ENDIF
            CALL CHSTR(LABEL,FIRST,LAST)
            CALL GTX (XPOS,XLAB,LABEL(FIRST:LAST))
          ENDIF
        ENDIF
        GOTO 101
      ENDIF
      IF ((LOGVAL.EQ.4 .OR. LOGVAL.EQ.3) .AND. XCUR.GT.EXCUR+.1*XINT)
     1    GOTO 101
C
C  MINOR TIC/GRID ?
C
      IF (TCOUNT .NE. MINCNT .AND. MINCNT .NE. 0) THEN
        IF (LGRID) THEN
          CALL GSPLCI(IGRIMN)
        ENDIF
        CALL GPL(2,PX,PY)
        IF (LGRID) THEN
          CALL GSPLCI(IGRIMJ)
        ENDIF
        TCOUNT = TCOUNT + 1
C
C  MAJOR TIC/GRID
C
      ELSE
        IF (.NOT. LGRID) THEN
          PY(2) = TICBIG
        ENDIF
        CALL GPL(2,PX,PY)
C
C  LABEL.
C
        IF (IXLAB .EQ. 1 .AND. LOOP .EQ. 1) THEN
          IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ. 2) THEN
C (NOAO)    WRITE(LABEL,XFMT)XPOS
            call encode (10, xfmt, label, xpos)
          ELSE
C (NOAO)    WRITE(LABEL,XFMT)XCUR
            call encode (10, xfmt, label, xcur)
          ENDIF
          CALL CHSTR(LABEL,FIRST,LAST)
          CALL GTX (XPOS,XLAB,LABEL(FIRST:LAST))
        ENDIF
        TCOUNT = 1
        IF (.NOT. LGRID) THEN
          PY(2) = TICEND
        ENDIF
      ENDIF
      IF ((LOGVAL .EQ. 4 .OR. LOGVAL .EQ. 3) .AND.
     1     XCUR .GE. EXCUR-.1*XINT) GOTO 101
 100  CONTINUE
 101  CONTINUE
C
C  TOP X-AXIS TICKS ?
C
      IF (LOOP.EQ.1 .AND. (IGPH.EQ.4 .OR. IGPH.EQ.5 .OR. IGPH.EQ.6))
     1  THEN
        START = WIND(4)
        TICEND = START - TICMIN
        TICBIG = START - TICMAJ
        PY(1) = START
        PY(2) = TICEND
        XPOS = WIND(1)
        LOOP = 2
        IF (LOGVAL .EQ. 4 .OR. LOGVAL .EQ.3) THEN
          XCUR = 10.**OWIND(1)
          IF (LOGMIN) THEN
            CURMAJ = XCUR
            NEXTMA = XCUR * MAJDIV
            XINT = (NEXTMA - CURMAJ) / 9.
          ENDIF
        ENDIF
        GOTO 80
      ENDIF
C
C  **** Y-AXIS TICS/GRIDS AND LABELS ****
C
  175 IF (IYLAB .EQ. -1) GOTO 999
C
C  CALCULATE Y-AXIS TICS
C
      MINCNT = MINRY
      IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ. 3) THEN
        LOGMIN = .FALSE.
        YINTM = YRANGE/MY
        YINT = YINTM
        IF (MINCNT .GT. 1) THEN
          YINT = YINT/MINCNT
        ENDIF
        YTNUM = MY * MINCNT
        IF (MINCNT .EQ. 0) YTNUM = MY
      ELSE
        YTNUM = 50
        YCUR = 10.**OWIND(3)
        MAJDIV = 10 ** MY
        IF (MINCNT .LE. 10 .AND. MY .LE. 1) THEN
          LOGMIN = .TRUE.
          CURMAJ = YCUR
          NEXTMA = YCUR * MAJDIV
          YINT = (NEXTMA - CURMAJ) / 9.
          MINCNT = 9
        ELSE
          LOGMIN = .FALSE.
          MINCNT = 1
        ENDIF
      ENDIF
C
      LGRID = .FALSE.
      LOOP = 1
C
C  DETERMINE ORIGIN OF TICK/GRID LINES (X COORDINATE.)
C
      IF (IGPH .NE. 10) THEN
        START = WIND(1)
      ELSE
        START = XI
      ENDIF
C
      YPOS = WIND(3)
      PX(1) = START
C
C  DETERMINE Y-AXIS TICK LENGTHS.
C
      IF (MAJY .EQ. 0.) THEN
        MAJY = .013
        MINY = .007
      ENDIF
      IF (XRNEW .EQ. 0.) THEN
        TICMIN = MINY * XRANGE
        TICMAJ = MAJY * XRANGE
      ELSE
        TICMIN = MINY * XRNEW
        TICMAJ = MAJY * XRNEW
      ENDIF
      TICEND = START + TICMIN
      TICBIG = START + TICMAJ
C
      PY(1) = YPOS
      PY(2) = PY(1)
C
C  DRAW BOTTOM-MOST TICK ON Y-AXIS IF (IGPH = 10
C  AND INTERSECTION OF AXES IS NOT AT BOTTOM LEFT
C  OF WINDOW.)
C
      IF (IGPH .EQ. 10) THEN
        IF (YI .NE. WIND(3)) THEN
          PX(2) = TICBIG
          CALL GPL(2,PX,PY)
        ENDIF
C
C  DRAW Y-AXIS FOR IGPH = 10
C
        PY(2) = WIND(4)
        PX(2) = PX(1)
        CALL GPL(2,PX,PY)
        PY(2) = PY(1)
      ELSE
C
C  DRAW X-AXIS FOR ANY OTHER IGPH (FIRST TICK.)
C
        PX(2) = WIND(2)
        CALL GPL(2,PX,PY)
      ENDIF
C
C  GRIDS OR TICS ?
C
      IF ((IGPH .EQ. 0 .OR. IGPH .EQ. 4).OR. IGPH .EQ. 8) THEN
        PX(2) = WIND(2)
        LGRID = .TRUE.
      ELSE
        PX(2) = TICEND
      ENDIF
C
C  SET TEXT ATTRIBUTES IF Y-AXIS IS TO BE LABELLED.
C
      IF (IYLAB .EQ. 1) THEN
        IF (IXORI .EQ. 1) THEN
          IF (YMIRRO) THEN
            CALL GSCHUP(0.,-1.)
          ELSE
            CALL GSCHUP(0.,1.)
          ENDIF
        ENDIF
C
C  SET TEXT ALIGNMENT TO (RIGHT,HALF)
C
        CALL GSTXAL(3,3)
C
C  RECALCULATE CHARACTER HEIGHT IF Y-AXIS LABELS ARE OF DIFFERENT
C  SIZE FORM X-AXIS LABELS.
C
        CHARH = SIZY * YRNEW
        IF (YMIRRO) THEN
          CHARH = -CHARH
        ENDIF
        CALL GSCHH(CHARH)
        IF (YDEC .NE. 0. .AND. YDEC .NE. 1.) THEN
          DEC = YDEC * XRNEW
        ELSE
          DEC = .02 * XRNEW
        ENDIF
        IF (YDEC .NE. 1.) THEN
          YLAB = START - DEC
        ELSE
          IF (IGPH .NE. 10) THEN
            YLAB = WIND(2)+DEC
          ELSE
            YLAB = XI+DEC
          ENDIF
C
C  SET TEXT ALIGNMENT TO (LEFT,HALF) IF LABELLING ON RIGHT OF Y-AXIS.
C
          CALL GSTXAL(1,3)
        ENDIF
        IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ.3) THEN
C (NOAO)  WRITE(LABEL,YFMT)YPOS
          call encode (10, yfmt, label, ypos)
        ELSE
C (NOAO)  WRITE(LABEL,YFMT)YCUR
          call encode (10, yfmt, label, ycur)
        ENDIF
        CALL CHSTR(LABEL,FIRST,LAST)
        CALL GTX (YLAB,YPOS,LABEL(FIRST:LAST))
      ENDIF
C
 180  TCOUNT = 1
C
      DO 200 I = 1,YTNUM
      IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ. 3) THEN
        YPOS = YPOS + YINT
      ELSE
        IF (.NOT. LOGMIN) THEN
          YCUR = YCUR * MAJDIV
        ELSE
          IF (TCOUNT .NE. MINCNT) THEN
            YCUR = YCUR + YINT
          ELSE
            YCUR = YCUR + YINT
            CURMAJ = NEXTMA
            NEXTMA = CURMAJ * MAJDIV
            YINT = (NEXTMA - CURMAJ) / 9.
          ENDIF
        ENDIF
        IF (YCUR .GT. 10.**OWIND(4)-.1*YINT) THEN
          YPOS = WIND(4)
        ELSE
          YPOS = ALOG10(YCUR)
        ENDIF
      ENDIF
C
      PY(1) = YPOS
      PY(2) = YPOS
C
C IF IGPH = 0,1,2,4,5,6 OR 8 AND YPOS = TOP AXIS, THEN
C DRAW AXIS, ELSE IF IGPH = 9 OR 10 DRAW TIC.
C
      IF (LOGVAL .EQ. 3 .OR. LOGVAL .EQ. 4) EYCUR = 10.**OWIND(4)
C
      IF ((((LOGVAL .EQ. 1.OR.LOGVAL.EQ.3) .AND. (I .EQ. YTNUM))
     1 .OR.((LOGVAL .EQ.4 .OR.LOGVAL .EQ.2).AND.YCUR.GE.EYCUR-.1*YINT))
     2 .AND.(IGPH.NE.9.AND.IGPH.NE.10)) THEN
        IF (LOOP .EQ. 1) THEN
          PX(2) = WIND(2)
          CALL GPL(2,PX,PY)
          IF (IYLAB .EQ. 1) THEN
            IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ.3) THEN
C (NOAO)      WRITE(LABEL,YFMT)YPOS
              call encode (10, yfmt, label, ypos)
            ELSE
              IF (YCUR .GT. EYCUR+.1*YINT) THEN
                GOTO 201
              ELSE
C (NOAO)        WRITE(LABEL,YFMT)YCUR
                call encode (10, yfmt, label, ycur)
              ENDIF
            ENDIF
            CALL CHSTR(LABEL,FIRST,LAST)
            CALL GTX (YLAB,YPOS,LABEL(FIRST:LAST))
          ENDIF
        ENDIF
        GOTO 201
      ENDIF
      IF ((LOGVAL.EQ.4 .OR. LOGVAL.EQ.2) .AND. YCUR.GT.EYCUR+.1*YINT)
     1  GOTO 201
C
C  MINOR TIC/GRID ?
C
      IF (TCOUNT .NE. MINCNT .AND. MINCNT .NE. 0) THEN
        IF (LGRID) THEN
          CALL GSPLCI(IGRIMN)
        ENDIF
        CALL GPL(2,PX,PY)
        IF (LGRID) THEN
          CALL GSPLCI(IGRIMJ)
        ENDIF
        TCOUNT = TCOUNT + 1
C
C  MAJOR TIC/GRID.
C
      ELSE
        IF (.NOT. LGRID) THEN
          PX(2) = TICBIG
        ENDIF
        CALL GPL(2,PX,PY)
C
C  LABEL.
C
        IF (IYLAB .EQ. 1 .AND. LOOP .EQ.1) THEN
          IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ.3) THEN
C (NOAO)   WRITE(LABEL,YFMT)YPOS
           call encode (10, yfmt, label, ypos)
          ELSE
C (NOAO)    WRITE(LABEL,YFMT)YCUR
            call encode (10, yfmt, label, ycur)
          ENDIF
          CALL CHSTR(LABEL,FIRST,LAST)
          CALL GTX(YLAB,YPOS,LABEL(FIRST:LAST))
        ENDIF
        TCOUNT = 1
        IF (.NOT. LGRID) THEN
          PX(2) = TICEND
        ENDIF
      ENDIF
      IF ((LOGVAL .EQ. 4 .OR. LOGVAL .EQ. 2) .AND.
     -     YCUR .GE. EYCUR-.1*YINT)
     1   GOTO 201
  200 CONTINUE
  201 CONTINUE
C
C  RIGHT Y-AXIS TICKS ?
C
      IF (LOOP .EQ. 1 .AND.(IGPH.EQ.1 .OR. IGPH .EQ. 5 .OR.
     1  IGPH .EQ. 9)) THEN
        START = WIND(2)
        TICEND = START - TICMIN
        TICBIG = START - TICMAJ
        PX(1) = START
        PX(2) = TICEND
        YPOS = WIND(3)
        LOOP = 2
        IF (LOGVAL .EQ. 4 .OR. LOGVAL .EQ. 2) THEN
          YCUR = 10.**OWIND(3)
          IF (LOGMIN) THEN
            CURMAJ = YCUR
            NEXTMA = YCUR * MAJDIV
            YINT = (NEXTMA - CURMAJ) / 9.
          ENDIF
        ENDIF
        GOTO 180
      ENDIF
C
C  RESET NORMALIZATION TRANSFORMATION TO WHAT IT WAS UPON ENTRY.
C
      IF (ICNT .NE. 0) THEN
        CALL GSWN(ICNT,OWIND(1),OWIND(2),OWIND(3),OWIND(4))
        CALL GSVP(ICNT,VIEW(1),VIEW(2),VIEW(3),VIEW(4))
      ENDIF
      CALL GSELNT(ICNT)
C
C  IF LABELS, RESTORE TEXT ATTRIBUTES.
C
      IF (IXLAB .EQ. 1 .OR. IYLAB .EQ. 1) THEN
        CALL GSCHH(OLDCHH)
        CALL GSCHUP(CHUPX,CHUPY)
        CALL GSTXAL(OLDALH,OLDALV)
        CALL GSTXCI(OTEXCI)
      ENDIF
C
C  RESTORE ORIGINAL COLOR.
C
      CALL GSPLCI(OCOLI)
C
C  RESTORE POLYLINE COLOR ASF TO WHAT IS WAS ON ENTRY.
C
      LASF(10) = OTXASF
      LASF(3) = OPLASF
      CALL GSASF(LASF)
C
 999  RETURN
      END
      SUBROUTINE GRID(MAJRX,MINRX,MAJRY,MINRY)
C
      COMMON /LAB/ SIZX,SIZY,XDEC,YDEC,IXORI
      COMMON /CLAB/ XFMT, YFMT
      COMMON /TICK/ MAJX, MINX, MAJY, MINY
      COMMON /GRIINT/ IGRIMJ, IGRIMN, IGRITX
      CHARACTER*8 XFMT,YFMT
      REAL MAJX,MINX,MAJY,MINY
C
C  THE FOLLOWING IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4('GRAPHX','GRIDAL','GRID','VERSION 01')
C
      CALL GRIDAL(MAJRX,MINRX,MAJRY,MINRY,0,0,0,0.,0.)
      RETURN
      END
      SUBROUTINE GRIDL(MAJRX,MINRX,MAJRY,MINRY)
C
      COMMON /LAB/ SIZX,SIZY,XDEC,YDEC,IXORI
      COMMON /CLAB/ XFMT, YFMT
      COMMON /TICK/ MAJX, MINX, MAJY, MINY
      COMMON /GRIINT/ IGRIMJ, IGRIMN, IGRITX
      CHARACTER*8 XFMT,YFMT
      REAL MAJX,MINX,MAJY,MINY
C
C  THE FOLLOWING IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4('GRAPHX','GRIDAL','GRIDL','VERSION 01')
C
      CALL GRIDAL(MAJRX,MINRX,MAJRY,MINRY,1,1,0,0.,0.)
      RETURN
      END
      SUBROUTINE PERIM(MAJRX,MINRX,MAJRY,MINRY)
C
      COMMON /LAB/ SIZX,SIZY,XDEC,YDEC,IXORI
      COMMON /CLAB/ XFMT, YFMT
      COMMON /TICK/ MAJX, MINX, MAJY, MINY
      COMMON /GRIINT/ IGRIMJ, IGRIMN, IGRITX
      CHARACTER*8 XFMT,YFMT
      REAL MAJX,MINX,MAJY,MINY
C
C  THE FOLLOWING IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4('GRAPHX','GRIDAL','PERIM','VERSION 01')
C
      CALL GRIDAL(MAJRX,MINRX,MAJRY,MINRY,0,0,5,0.,0.)
      RETURN
      END
      SUBROUTINE PERIML(MAJRX,MINRX,MAJRY,MINRY)
C
      COMMON /LAB/ SIZX,SIZY,XDEC,YDEC,IXORI
      COMMON /CLAB/ XFMT, YFMT
      COMMON /TICK/ MAJX, MINX, MAJY, MINY
      COMMON /GRIINT/ IGRIMJ, IGRIMN, IGRITX
      CHARACTER*8 XFMT,YFMT
      REAL MAJX,MINX,MAJY,MINY
C
C  THE FOLLOWING IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4('GRAPHX','GRIDAL','PERIML','VERSION 01')
C
      CALL GRIDAL(MAJRX,MINRX,MAJRY,MINRY,1,1,5,0.,0.)
      RETURN
      END
      SUBROUTINE HALFAX(MAJRX,MINRX,MAJRY,MINRY,X,Y,IXLAB,IYLAB)
C
      COMMON /LAB/ SIZX,SIZY,XDEC,YDEC,IXORI
      COMMON /CLAB/ XFMT, YFMT
      COMMON /TICK/ MAJX, MINX, MAJY, MINY
      COMMON /GRIINT/ IGRIMJ, IGRIMN, IGRITX
      CHARACTER*8 XFMT,YFMT
      REAL MAJX,MINX,MAJY,MINY
C
C  THE FOLLOWING IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4('GRAPHX','GRIDAL','HALFAX','VERSION 01')
C
      CALL GRIDAL(MAJRX,MINRX,MAJRY,MINRY,IXLAB,IYLAB,10,X,Y)
      RETURN
      END
      SUBROUTINE LABMOD(FMTX,FMTY,NUMX,NUMY,ISIZX,ISIZY,IXDEC,IYDEC,
     1                  IXOR)
C
C  RESETS PARAMETERS FOR TEXT GRAPHICS FROM DEFAULT VALUES.
C
      COMMON /LAB/ SIZX,SIZY,XDEC,YDEC,IXORI
      COMMON /CLAB/ XFMT, YFMT
      CHARACTER*8 XFMT,YFMT,FMTX,FMTY
C
C  THE FOLLOWING IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4('GRAPHX','GRIDAL','LABMOD','VERSION 01')
C
C
C +NOAO - Blockdata rewritten as run time initialization.
C       EXTERNAL GRIDT
	call gridt
C -NOAO
      XFMT = '        '
      YFMT = '        '
      XFMT = FMTX
      YFMT = FMTY
C
      CALL GETUSV('XF',IVAL)
      XRANGE = 2. ** IVAL
      CALL GETUSV('YF', IVAL)
      YRANGE = 2. ** IVAL
C
C  SIZX AND SIZY ARE COMPUTED TO BE PERCENTAGES OF TOTAL SCREEN
C  WIDTH.
C
      IF (ISIZX .GT. 3) THEN
        SIZX = FLOAT(ISIZX)/XRANGE
      ELSEIF (ISIZX .EQ. 3) THEN
        SIZX = 24./1024.
      ELSEIF (ISIZX .EQ. 2) THEN
        SIZX = 16./1024.
      ELSEIF (ISIZX .EQ. 1) THEN
        SIZX = 12./1024.
      ELSE
        SIZX = 8./1024.
      ENDIF
C
      IF (ISIZY .GT. 3) THEN
        SIZY = FLOAT(ISIZY)/XRANGE
      ELSEIF (ISIZY .EQ. 3) THEN
        SIZY = 24./1024.
      ELSEIF (ISIZY .EQ. 2) THEN
        SIZY = 16./1024.
      ELSEIF (ISIZY .EQ. 1) THEN
        SIZY = 12./1024.
      ELSE
        SIZY = 8./1024.
      ENDIF
C
C  CALCULATE XDEC AND YDEC AS PERCENTAGES OF TOTAL SCREEN WIDTH
C  IN PLOTTER ADDRESS UNITS.
C
      IF (IXDEC .EQ. 0 .OR. IXDEC .EQ. 1) THEN
        YDEC = FLOAT(IXDEC)
      ELSE
        YDEC = FLOAT(IXDEC)/XRANGE
      ENDIF
      IF (IYDEC .EQ. 0 .OR. IYDEC .EQ. 1) THEN
        XDEC = FLOAT(IYDEC)
      ELSE
        XDEC = FLOAT(IYDEC)/YRANGE
      ENDIF
C
      IXORI = IXOR
C
      RETURN
      END
      SUBROUTINE TICK4(LMAJX,LMINX,LMAJY,LMINY)
C
C  CHANGES TICK LENGTH FOR EACH AXIS.
C
      COMMON /TICK/ MAJX, MINX, MAJY, MINY
      REAL MAJX, MINX, MAJY, MINY
C
C  THE FOLLOWING IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4('GRAPHX','GRIDAL','TICK4','VERSION 01')
C
      CALL GETUSV('XF', IVAL)
      XRANGE = 2. ** IVAL
      CALL GETUSV('YF', IVAL)
      YRANGE = 2. ** IVAL
C
      MAJX = FLOAT(LMAJX)/YRANGE
      MINX = FLOAT(LMINX)/YRANGE
      MAJY = FLOAT(LMAJY)/XRANGE
      MINY = FLOAT(LMINY)/XRANGE
C
      RETURN
      END
      SUBROUTINE TICKS(LMAJ,LMIN)
C
      COMMON /TICK/ MAJX,MINX,MAJY,MINY
      REAL MAJX,MINX,MAJY,MINY
C
C  THE FOLLOWING IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4('GRAPHX','GRIDAL','TICKS','VERSION 01')
C
      CALL TICK4(LMAJ,LMIN,LMAJ,LMIN)
C
      RETURN
      END
      SUBROUTINE CHSTR(LABEL,FIRST,LAST)
C
C  THIS CALCULATES THE POSITION OF THE FIRST NON-BLANK CHARACTER
C  AND THE POSITION OF THE LAST NON-BLANK CHARACTER IN LABEL.
C
      INTEGER   FIRST, LAST
      CHARACTER*15 LABEL
C
      DO 100 I = 1,15
      IF (LABEL(I:I) .NE. ' ') GOTO 200
 100  CONTINUE
 200  FIRST = I
      LAST = 15
      IF (FIRST .NE. 15) THEN
        DO 300 J = FIRST+1,15
        IF (LABEL(J:J) .EQ. ' ') THEN
          LAST = J-1
          GOTO 999
        ENDIF
 300    CONTINUE
 999    CONTINUE
      ENDIF
      RETURN
      END
      SUBROUTINE EXPAND(MAXW)
C
C  THE WINDOW IS EXPANDED AND THE NEW WORLD COORDINATES ARE
C  CALCULATED TO CORRESPOND TO THE MAXIMUM VIEWPORT.
C  THE ORIGINAL ASPECT RATIO OF WORLD COORDINATES TO VIEWPORT
C  COORDINATES REMAINS THE SAME.  UNDER THE NEWLY-DEFINED
C  NORMALIZATION TRANSFORMATION, THE WINDOW OF THE ORIGINAL
C  NORMALIZATION TRANSFORMATION IS MAPPED TO THE VIEWPORT
C  OF THE ORIGINAL NORMALIZATION TRANSFORMATION IN EXACTLY
C  THE SAME WAY AS IN THE INITIAL NORMALIZATION TRANSFORMATION.
C
      REAL MAXW(4), VIEW(4), WIND(4)
      REAL LEFT
C
C  INQUIRE CURRENT WINDOW AND VIEWPORT SETTINGS.
C
      CALL GQCNTN(IERR,ICNT)
      CALL GQNT(ICNT,IERR,WIND,VIEW)
C
C  CALCULATE RATIO OF Y WORLD/VIEWPORT COORDINATES.
C
      YRATIO = (WIND(4) - WIND(3))/(VIEW(4) - VIEW(3))
C
C  CALCULATE RATIO OF X WORLD/VIEWPORT COORDINATES.
C
      XRATIO = (WIND(2) - WIND(1))/(VIEW(2) - VIEW(1))
C
C  GET EXPANDED LOWER LIMIT Y COORDINATE.
C
      VBOTTM = VIEW(3) - 0.
      BOTTOM = YRATIO * VBOTTM
      MAXW(3) = WIND(3) - BOTTOM
C
C  GET EXPANDED UPPER LIMIT Y COORDINATE.
C
      VTOP = 1. - VIEW(4)
      TOP = YRATIO * VTOP
      MAXW(4) = WIND(4) + TOP
C
C  GET EXPANDED LEFT LIMIT X COORDINATE.
C
      VLEFT = VIEW(1) - 0.
      LEFT = XRATIO * VLEFT
      MAXW(1) = WIND(1) - LEFT
C
C  GET EXPANDED RIGHT LIMIT X COORDINATE.
C
      VRIGHT = 1. - VIEW(2)
      RIGHT = XRATIO * VRIGHT
      MAXW(2) = WIND(2) + RIGHT
C
C  SET NEW (EXPANDED) NORMALIZATION TRANSFORMATION.
C
      CALL GSWN(1,MAXW(1),MAXW(2),MAXW(3),MAXW(4))
      CALL GSVP(1, 0., 1., 0., 1. )
      CALL GSELNT(1)
C
      RETURN
      END
