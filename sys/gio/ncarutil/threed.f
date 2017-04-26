      SUBROUTINE SET3 (XA,XB,YA,YB,ULO,UHI,VLO,VHI,WLO,WHI,EYE)
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
C
C THREE-DIMENSIONAL LINE DRAWING PACKAGE
C
C
C LATEST REVISION        JULY, 1984
C
C PURPOSE                THREED IS A PACKAGE OF SUBROUTINES THAT
C                        PROVIDES LINE DRAWING CAPABILITIES IN
C                        THREE-SPACE.
C
C USAGE                  EACH ENTRY POINT IN THIS PACKAGE IS
C                        DESCRIBED BELOW.
C
C                        SET3 (XA,XB,YA,YB,UC,UD,VC,VD,WC,WD,EYE)
C
C                          XA, XB, YA, YB  DEFINE THE PORTION OF THE
C                          PLOTTING SURFACE INTO WHICH THE USER'S
C                          PLOT WILL BE PLACED.  THESE VALUES SHOULD
C                          BE IN THE RANGE 0. TO 1.  FOR EXAMPLE, IF
C                          ONE WANTS THE PLOT TO OCCUPY THE MAXIMUM
C                          PLOTTING SURFACE, SET XA=0., YA=0., XB=1.,
C                          YB=1.; IF ONE WANTS THE PLOT TO APPEAR IN
C                          THE LOWER LEFT CORNER OF THE PLOTTING
C                          SURFACE, SET XA=0., YA=0., XB=.5, YB=.5 .
C
C                          UC, UD, VC, VD, WC, AND WD DEFINE A
C                          VOLUME IN USER-COORDINATE SPACE WHICH
C                          WILL BE TRANSFORMED ONTO THE PLOTTING
C                          SURFACE DEFINED BY XA, XB, YA, YB.
C
C                          EYE IS AN ARRAY, 3 WORDS LONG, CONTAINING THE
C                          U, V, AND W COORDINATES OF THE EYE POSITION.
C                          ALL LINES IN THE PLOT ARE DRAWN AS VIEWED
C                          FROM THE EYE.  EYE IS SPECIFIED IN USER
C                          COORDINATES AND SHOULD BE OUTSIDE THE BOX
C                          DEFINED BY UC, UD, VC, VC, WC, AND WD.
C
C                        CURVE3 (U,V,W,N)
C
C                          DRAWS A CURVE THROUGH N POINTS.  THE
C                          POINTS ARE DEFINED BY THE LINEAR ARRAYS
C                          U, V, AND W WHICH ARE DIMENSIONED N OR
C                          GREATER.
C
C                        LINE3 (UA,VA,WA,UB,VB,WB)
C
C                          DRAWS A LINE CONNECTING THE COORDINATES
C                          (UA,VA,WA)  AND  (UB,VB,WB).
C
C                        FRST3 (U,V,W)
C
C                          POSITIONS THE PEN TO (U,V,W).
C
C                        VECT3 (U,V,W)
C
C                          DRAWS A LINE BETWEEN THE CURRENT PEN
C                          POSITION AND THE POINT (U,V,W).  THE
C                          CURRENT PEN POSITION BECOMES (U,V,W).
C                          NOTE THAT A CURVE CAN BE DRAWN BY USING
C                          A FRST3 CALL FOLLOWED BY A SEQUENCE OF
C                          VECT3 CALLS.
C
C                        POINT3 (U,V,W)
C
C                          PLOTS A POINT AT (U,V,W) .
C
C                        PERIM3 (MAGR1,MINR1,MAGR2,MINR2,IWHICH,VAR)
C
C                          DRAWS A PERIMETER WITH TICK MARKS.
C
C                          IWHICH DESIGNATES THE NORMAL VECTOR TO THE
C                          PERIMETER DRAWN (1=U, 2=V, 3=W).
C
C                          VAR IS THE VALUE ON THE AXIS SPECIFIED BY
C                          INWHICH WHERE THE PERIMETER IS TO BE DRAWN.
C
C                          MAGR1  AND  MAGR2  SPECIFY THE
C                          NUMBER OF MAJOR TICK MARKS TO BE DRAWN IN
C                          THE TWO COORDINATE DIRECTIONS.
C
C                          MINR1  AND  MINR2  SPECIFY THE NUMBER
C                          OF MINOR TICKS BETWEEN EACH MAJOR TICK.
C
C                          MAGR1, MAGR2, MINR1 AND MINR2
C                          ARE SPECIFIED BY THE NUMBER
C                          OF DIVISIONS(HOLES), NOT THE NUMBER OF
C                          TICKS.  SO IF MAGR1=1, THERE WOULD BE NO
C                          MAJOR DIVISIONS.
C
C                        TICK43 (MAGU,MINU,MAGV,MINV,MAGW,MINW)
C
C                          TICK43 ALLOWS PROGRAM CONTROL OF TICK
C                          MARK LENGTH IN SUBROUTINE PERIM3.
C                          MAGU, MAGV, MAGW SPECIFY THE LENGTH,
C                          IN PLOTTER ADDRESS UNITS OF MAJOR
C                          DIVISION TICK MARKS ON THE U, V, AND W
C                          AXES.  MINU, MINV, MINW SPECIFY THE LENGTH,
C                          IN PLOTTER ADDRESS UNITS OF MINOR
C                          DIVISION TICK MARKS ON THE U, V, AND
C                          W AXES.
C
C                        FENCE3 (U,V,W,N,IOREN,BOT)
C
C                          THIS ENTRY IS USED TO DRAW A LINE IN THREE-
C                          SPACE AS WELL AS A "FENCE" BETWEEN THE
C                          LINE AND A PLANE NORMAL TO ONE OF THE
C                          COORDINATE AXES.
C
C                          THE ARGUMENTS U, V, W AND N
C                          ARE THE SAME AS FOR CURVE, DESCRIBED ABOVE.
C
C                          IOREN SPECIFIES THE DIRECTION IN WHICH THE
C                          FENCE LINES ARE TO BE DRAWN (1 INDICATES
C                          PARALLEL TO THE U-AXIS, 2 INDICATES PARALLEL
C                          TO THE V-AXIS, AND 3 INDICATES PARALLEL TO
C                          TO THE W-AXIS.)
C
C                          BOT SPECIFIES WHERE THE BOTTOM OF THE FENCE
C                          IS TO BE DRAWN.
C                          IF THE FENCE LINES ARE TO BE DRAWN PARALLEL
C                          TO THE W-AXIS, AND  BOT=2., THEN THE BOTTOM
C                          OF THE FENCE WOULD BE THE PLANE  W=2.
C
C ON OUTPUT              ALL ARGUMENTS ARE UNCHANGED.
C
C NOTES                  .  FOR DRAWING CHARACTERS IN CONJUNCTION
C                           WITH THREED, USE THE COMPANION ROUTINE
C                           PWRZT.
C
C ENTRY POINTS          FENCE3, TRN32T, FRST3, VECT3, LIN3,
C                       POINT3, CURVE3, PSYM3, PERIM3, LINE3W,
C                       DRAWT, TICK43, TICK3, THREBD
C
C COMMON BLOCKS         TEMPR, SET31, PWRZ1T, TCK31, PRM31, THRINT
C
C REQUIRED LIBRARY      PWRZ AND THE SPPS
C ROUTINES
C
C HISTORY               WRITTEN AND STANDARDIZED IN NOVEMBER 1973.
C I/O                   PLOTS LINES.
C
C PRECISION             SINGLE
C
C LANGUAGE              FORTRAN
C
C ACCURACY              + OR -.5 PLOTTER ADDRESS UNITS PER CALL.
C                       THERE IS NO CUMULATIVE ERROR.
C
C PORTABILITY           ANSI FORTRAN 77
C
C
C
C
C
      SAVE
C
      COMMON /TEMPR/  RZERO
C
      DIMENSION       EYE(3)
C
      COMMON /SET31/  ISCALE     ,XMIN       ,XMAX       ,YMIN       ,
     1                YMAX       ,BIGD       ,R0         ,NLX        ,
     2                NBY        ,NRX        ,NTY
      COMMON /PWRZ1T/ UUMIN      ,UUMAX      ,VVMIN      ,VVMAX      ,
     1                WWMIN      ,WWMAX      ,DELCRT     ,EYEU       ,
     2                EYEV       ,EYEW
C
C
      AVE(A,B) = (A+B)*.5
C
C ARITHMETIC STATEMENT FUNCTION FOR SCALING
C
      SU(UTEMP) = UTEMP
      SV(VTEMP) = VTEMP
      SW(WTEMP) = WTEMP
C
C +NOAO - Blockdata threbd rewritten as run time initialization.
C
C     EXTERNAL        THREBD
      call threbd
C -NOAO
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('GRAPHX','THREED','SET3','VERSION  1')
C
C SET UP FRAME SIZE
C
      NLX = XA*1023.+1.
      NRX = XB*1023.+1.
      NBY = YA*1023.+1.
      NTY = YB*1023.+1.
C
C CONSTANTS FOR PWRZT
C
      UUMIN = ULO
      UUMAX = UHI
      VVMIN = VLO
      VVMAX = VHI
      WWMIN = WLO
      WWMAX = WHI
      EYEU = EYE(1)
      EYEV = EYE(2)
      EYEW = EYE(3)
C
C FIND CORNERS IN 2-SPACE FOR 3-SPACE BOX CONTAINING OBJECT
C
      ISCALE = 0
      ATU = AVE(SU(UUMIN),SU(UUMAX))
      ATV = AVE(SV(VVMIN),SV(VVMAX))
      ATW = AVE(SW(WWMIN),SW(WWMAX))
      BIGD = 0.
      IF (RZERO .LE. 0.) GO TO  10
C
C RELATIVE SIZE FEATURE IN USE.  THIS SECTION OF CODE IS NEVER
C EXECUTED UNLESS RZERO IS SET POSITIVE IN THE CALLING PROGRAM
C VIA COMMON BLOCK TEMPR.  RZERO  IS THE DISTANCE BETWEEN THE
C OBSERVER AND THE POINT LOOKED AT (CENTER OF THE BOX BY DEFAULT)
C WHEN THE INPUT BOX IS TO FILL THE SCREEN WHEN VIEWED FROM THE
C DIRECTION WHICH MAKES THE BOX BIGGEST.  RZERO  IS THUS TO
C BE USED TO DETERMINE THE SHAPE OF THE OBJECT.   THIS SECTION
C OF CODE IS TO BE USED WHEN IT IS DESIRED TO KEEP THE VIEWED
C OBJECT IN RELATIVE PERSPECTIVE ACROSS FRAMES--E.G. IN MAKING
C MOVIES.
C
      ALPHA = -(VVMIN-ATV)/(UUMIN-ATU)
      VVEYE = -RZERO/SQRT(1.+ALPHA*ALPHA)
      UUEYE = VVEYE*ALPHA
      VVEYE = VVEYE+ATV
      UUEYE = UUEYE+ATU
      WWEYE = ATW
      CALL TRN32T (ATU,ATV,ATW,UUEYE,VVEYE,WWEYE,1)
      CALL TRN32T (UUMIN,VVMIN,ATW,XMIN,DUMM,DUMM,2)
      CALL TRN32T (UUMAX,VVMIN,WWMIN,DUMM,YMIN,DUMM,2)
      CALL TRN32T (UUMAX,VVMAX,ATW,XMAX,DUMM,DUMM,2)
      CALL TRN32T (UUMAX,VVMIN,WWMAX,DUMM,YMAX,DUMM,2)
      BIGD = SQRT((UUMAX-UUMIN)**2+(VVMAX-VVMIN)**2+(WWMAX-WWMIN)**2)*.5
      R0 = RZERO
      GO TO  20
   10 CALL TRN32T (ATU,ATV,ATW,EYE(1),EYE(2),EYE(3),1)
      CALL TRN32T (SU(UUMIN),SV(VVMIN),SW(WWMIN),X1,Y1,DUM,2)
      CALL TRN32T (SU(UUMIN),SV(VVMIN),SW(WWMAX),X2,Y2,DUM,2)
      CALL TRN32T (SU(UUMIN),SV(VVMAX),SW(WWMIN),X3,Y3,DUM,2)
      CALL TRN32T (SU(UUMIN),SV(VVMAX),SW(WWMAX),X4,Y4,DUM,2)
      CALL TRN32T (SU(UUMAX),SV(VVMIN),SW(WWMIN),X5,Y5,DUM,2)
      CALL TRN32T (SU(UUMAX),SV(VVMIN),SW(WWMAX),X6,Y6,DUM,2)
      CALL TRN32T (SU(UUMAX),SV(VVMAX),SW(WWMIN),X7,Y7,DUM,2)
      CALL TRN32T (SU(UUMAX),SV(VVMAX),SW(WWMAX),X8,Y8,DUM,2)
      XMIN = AMIN1(X1,X2,X3,X4,X5,X6,X7,X8)
      XMAX = AMAX1(X1,X2,X3,X4,X5,X6,X7,X8)
      YMIN = AMIN1(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
      YMAX = AMAX1(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
C
C ADD RIGHT AMOUNT TO KEEP PICTURE SQUARE
C
   20 WIDTH = XMAX-XMIN
      HIGHT = YMAX-YMIN
      DIF = .5*(WIDTH-HIGHT)
      IF (DIF)  30, 50, 40
   30 XMIN = XMIN+DIF
      XMAX = XMAX-DIF
      GO TO  50
   40 YMIN = YMIN-DIF
      YMAX = YMAX+DIF
   50 ISCALE = 1
      CALL TRN32T (ATU,ATV,ATW,EYE(1),EYE(2),EYE(3),1)
      RETURN
      END
      SUBROUTINE TRN32T (U,V,W,XT,YT,ZT,IENT)
C
C THIS ROUTINE IMPLEMENTS THE 3-SPACE TO 2-SPACE TRANSFOR-
C MATION BY KUBER, SZABO AND GIULIERI, THE PERSPECTIVE
C REPRESENTATION OF FUNCTIONS OF TWO VARIABLES. J. ACM 15,
C 2, 193-204,1968.
C TRN32T ARGUMENTS
C U,V,W    ARE THE 3-SPACE COORDINATES OF THE INTERSECTION
C          OF THE LINE OF SIGHT AND THE IMAGE PLANE.  THIS
C          POINT CAN BE THOUGHT OF AS THE POINT LOOKED AT.
C XT,YT,ZT ARE THE 3-SPACE COORDINATES OF THE EYE POSITION.
C
C TRN32 ARGUMENTS
C U,V,W    ARE THE 3-SPACE COORDINATES OF A POINT TO BE
C          TRANSFORMED.
C XT,YT    THE RESULTS OF THE 3-SPACE TO 2-SPACE TRANSFOR-
C          MATION.  WHEN ISCALE=0, XT AND YT ANR IN THE SAME
C          UNITS AS U,V, AND W.  WHEN ISCALE'0, XT AND YT
C          ARE IN PLOTTER COORDINATES.
C ZT       NOT USED.
C
C
      SAVE
C
      COMMON /PWRZ1T/ UUMIN      ,UUMAX      ,VVMIN      ,VVMAX      ,
     1                WWMIN      ,WWMAX      ,DELCRT     ,EYEU       ,
     2                EYEV       ,EYEW
      COMMON /SET31/  ISCALE     ,XMIN       ,XMAX       ,YMIN       ,
     1                YMAX       ,BIGD       ,R0         ,NLX        ,
     2                NBY        ,NRX        ,NTY
C
C DECIDE IF SET OR TRANSLATE CALL
C
      IF (IENT .NE. 1) GO TO  50
C
C STORE THE PARAMETERS OF THE SET CALL
C FOR USE WITH THE TRANSLATION CALL
C
      AU = U
      AV = V
      AW = W
      EU = XT
      EV = YT
      EW = ZT
C
C
C
C
C
      DU = AU-EU
      DV = AV-EV
      DW = AW-EW
      D = SQRT(DU*DU+DV*DV+DW*DW)
      COSAL = DU/D
      COSBE = DV/D
      COSGA = DW/D
      AL = ACOS(COSAL)
      BE = ACOS(COSBE)
      GA = ACOS(COSGA)
      SINGA = SIN(GA)
C
C THE 3-SPACE POINT LOOKED AT IS TRANSFORMED INTO (0,0) OF
C THE 2-SPACE.  THE 3-SPACE W AXIS IS TRANSFORMED INTO THE
C 2-SPACE Y AXIS.  IF THE LINE OF SIGHT IS CLOSE TO PARALLEL
C TO THE 3-SPACE W AXIS, THE 3-SPACE V AXIS IS CHOSEN (IN-
C STEAD OF THE 3-SPACE W AXIS) TO BE TRANSFORMED INTO THE
C 2-SPACE Y AXIS.
C
      ASSIGN  90 TO JDONE
      IF (ISCALE)  10, 30, 10
   10 X0 = XMIN
      Y0 = YMIN
      X1 = NLX
      Y1 = NBY
      X2 = NRX-NLX
      Y2 = NTY-NBY
      X3 = X2/(XMAX-XMIN)
      Y3 = Y2/(YMAX-YMIN)
      X4 = NRX
      Y4 = NTY
      FACT = 1.
      IF (BIGD .LE. 0.) GO TO  20
      X0 = -BIGD
      Y0 = -BIGD
      X3 = X2/(2.*BIGD)
      Y3 = Y2/(2.*BIGD)
      FACT = R0/D
   20 DELCRT = X2
      ASSIGN  80 TO JDONE
   30 IF (SINGA .LT. 0.0001) GO TO  40
      R = 1./SINGA
      ASSIGN  70 TO JUMP
      RETURN
   40 SINBE = SIN(BE)
      R = 1./SINBE
      ASSIGN  60 TO JUMP
      RETURN
C
C********************  ENTRY TRN32  ************************
C     ENTRY TRN32 (U,V,W,XT,YT,ZT)
C
   50 UU = U
      VV = V
      WW = W
      Q = D/((UU-EU)*COSAL+(VV-EV)*COSBE+(WW-EW)*COSGA)
      GO TO JUMP,( 60, 70)
   60 UU = ((EW+Q*(WW-EW)-AW)*COSAL-(EU+Q*(UU-EU)-AU)*COSGA)*R
      VV = (EV+Q*(VV-EV)-AV)*R
      GO TO JDONE,( 80, 90)
   70 UU = ((EU+Q*(UU-EU)-AU)*COSBE-(EV+Q*(VV-EV)-AV)*COSAL)*R
      VV = (EW+Q*(WW-EW)-AW)*R
      GO TO JDONE,( 80, 90)
   80 XT = X1+X3*(FACT*UU-X0)
      YT = Y1+Y3*(FACT*VV-Y0)
      RETURN
   90 XT = UU
      YT = VV
      RETURN
      END
      SUBROUTINE FRST3 (U,V,W)
      SAVE
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('GRAPHX','THREED','FRST3','VERSION  1')
      XDUM = 5.
      CALL TRN32T (U,V,W,X,Y,XDUM,2)
      CALL PLOTIT  (32*IFIX(X),32*IFIX(Y),0)
      RETURN
      END
      SUBROUTINE VECT3 (U,V,W)
      SAVE
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('GRAPHX','THREED','VECT3','VERSION  1')
      CALL TRN32T (U,V,W,X,Y,ZDUM,2)
      IIX = 32*IFIX(X)
      IIY = 32*IFIX(Y)
      CALL PLOTIT  (IIX,IIY,1)
C
C     FLUSH PLOTIT BUFFER
C
      CALL PLOTIT  (IIX,IIY,0)
      RETURN
      END
      SUBROUTINE LINE3 (UA,VA,WA,UB,VB,WB)
      SAVE
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('GRAPHX','THREED','LINE3','VERSION  1')
      CALL TRN32T (UA,VA,WA,XA,YA,XDUM,2)
      CALL TRN32T (UB,VB,WB,XB,YB,XDUM,2)
      IIX = 32*IFIX(XB)
      IIY = 32*IFIX(YB)
      CALL PLOTIT (32*IFIX(XA),32*IFIX(YA),0)
      CALL PLOTIT (IIX,IIY,1)
C
C     FLUSH PLOTIT BUFFER
C
      CALL PLOTIT (IIX,IIY,0)
      RETURN
      END
      SUBROUTINE POINT3 (U,V,W)
      SAVE
      DIMENSION VWPRT(4),WNDW(4)
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('GRAPHX','THREED','POINT3','VERSION  1')
C
C INQUIRE CURRENT NORMALIZATION TRANS NUMBER
C
      CALL GQCNTN (IERR,NTORIG)
C
C SAVE NORMALIZATION TRANS 1 AND CURRENT LOG SCALING
C
      CALL GQNT (1,IERR,WNDW,VWPRT)
      CALL GETUSV ('LS',IOLLS)
C
C DEFINE NOMALIZATION TRANS TO BE USED WITH POLYMARKER
C
      CALL SET(0.0, 1.0, 0.0, 1.0, 1.0, 1024.0, 1.0, 1024.0, 1)
C
C SET MARKER TYPE TO 1
C
      CALL GSMK (1)
      CALL TRN32T (U,V,W,X,Y,ZDUM,2)
      PX = X
      PY = Y
      CALL GPM (1,PX,PY)
C
C RESTORE ORIGINAL TRANS 1 AND SELECT TRANS NUMBER NTORIG
C RESTORE LOG SCALING
C
      CALL SET(VWPRT(1),VWPRT(2),VWPRT(3),VWPRT(4),
     -         WNDW(1),WNDW(2),WNDW(3),WNDW(4),IOLLS)
      CALL GSELNT (NTORIG)
      RETURN
      END
      SUBROUTINE CURVE3 (U,V,W,N)
      SAVE
      DIMENSION       U(N)       ,V(N)       ,W(N)
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('GRAPHX','THREED','CURVE3','VERSION  1')
      CALL TRN32T (U(1),V(1),W(1),X,Y,ZDUM,2)
      CALL PLOTIT (32*IFIX(X),32*IFIX(Y),0)
      NN = N
      IF (NN .LT. 2) RETURN
      DO  10 I=2,NN
         UU = U(I)
         VV = V(I)
         WW = W(I)
         CALL TRN32T (UU,VV,WW,X,Y,ZDUM,2)
         CALL PLOTIT (32*IFIX(X),32*IFIX(Y),1)
   10 CONTINUE
C
C     FLUSH PLOTIT BUFFER
C
      CALL PLOTIT(0,0,0)
      RETURN
      END
      SUBROUTINE PSYM3 (U,V,W,ICHAR,SIZE,IDIR,ITOP,IUP)
      SAVE
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('GRAPHX','THREED','PSYM3','VERSION  1')
      IF (IUP .EQ. 2) CALL VECT3 (U,V,W)
      CALL PWRZ (U,V,W,ICHAR,1,SIZE,IDIR,ITOP,0)
      RETURN
      END
      SUBROUTINE PERIM3 (MAGR1,MINI1,MAGR2,MINI2,IWHICH,VAR)
      SAVE
      COMMON /PWRZ1T/ UUMIN      ,UUMAX      ,VVMIN      ,VVMAX      ,
     1                WWMIN      ,WWMAX      ,DELCRT     ,EYEU       ,
     2                EYEV       ,EYEW
      COMMON /PRM31/  Q          ,L
      COMMON /TCK31/  TMAGU      ,TMINU      ,TMAGV      ,TMINV      ,
     1                TMAGW      ,TMINW
C
C THRINT COMMON BLOCK IS USED FOR SETTING COLOR INTENSITY
C
      COMMON /THRINT/ ITHRMJ     ,ITHRMN     ,ITHRTX
      DIMENSION       LASF(13)
C
      TICK(T) = AMAX1(UUMAX-UUMIN,VVMAX-VVMIN,WWMAX-WWMIN)*T/1024.
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('GRAPHX','THREED','PERIM3','VERSION  1')
C
C INQUIRE LINE COLOR INDEX AND SET ASF TO INDIVIDUAL
C
      CALL GQPLCI (IERR, IPLCI)
      CALL GQASF (IERR, LASF)
      LSV3  = LASF(3)
      LASF(3) = 1
      CALL GSASF (LASF)
C
      MGR1 = MAGR1
      MN1 = MINI1-1
      MGR2 = MAGR2
      MN2 = MINI2-1
      MN1P1 = MAX0(MN1+1,1)
      MN2P1 = MAX0(MN2+1,1)
      L = MIN0(3,MAX0(1,IWHICH))
      Q = VAR
C
C PICK BOUNDS
C
      GO TO ( 10, 30, 40),L
   10 XMIN = VVMIN
      XMAX = VVMAX
      DELXL = TICK(TMAGU)
      DELXS = TICK(TMINU)
   20 YMIN = WWMIN
      YMAX = WWMAX
      DELYL = TICK(TMAGW)
      DELYS = TICK(TMINW)
      GO TO  50
   30 XMIN = UUMIN
      XMAX = UUMAX
      DELXL = TICK(TMAGU)
      DELXS = TICK(TMINU)
      GO TO  20
   40 XMIN = UUMIN
      XMAX = UUMAX
      DELXL = TICK(TMAGU)
      DELXS = TICK(TMINU)
      YMIN = VVMIN
      YMAX = VVMAX
      DELYL = TICK(TMAGV)
      DELYS = TICK(TMINV)
C
C PERIM
C
   50 CALL LINE3W (XMIN,YMIN,XMAX,YMIN)
      CALL LINE3W (XMAX,YMIN,XMAX,YMAX)
      CALL LINE3W (XMAX,YMAX,XMIN,YMAX)
      CALL LINE3W (XMIN,YMAX,XMIN,YMIN)
      IF (MGR1 .LT. 1) GO TO  90
      DX = (XMAX-XMIN)/AMAX0(MGR1*(MN1P1),1)
      DO  80 I=1,MGR1
C
C MINORS FIRST
C
         IF (MN1 .LE. 0) GO TO  70
C
C SET LINE INTENSITY TO LOW
C
         CALL GSPLCI (ITHRMN)
         DO  60 J=1,MN1
            X = XMIN+FLOAT(MN1P1*(I-1)+J)*DX
            CALL LINE3W (X,YMIN,X,YMIN+DELYS)
            CALL LINE3W (X,YMAX,X,YMAX-DELYS)
   60    CONTINUE
   70    IF (I .GE. MGR1) GO TO  90
C
C SET LINE INTENSITY TO HIGH
C
         CALL GSPLCI (ITHRMJ)
         X = XMIN+FLOAT(MN1P1*I)*DX
C
C MAJORS
C
         CALL LINE3W (X,YMIN,X,YMIN+DELYL)
         CALL LINE3W (X,YMAX,X,YMAX-DELYL)
   80 CONTINUE
   90 IF (MGR2 .LT. 1) GO TO 130
      DY = (YMAX-YMIN)/AMAX0(MGR2*(MN2P1),1)
      DO 120 J=1,MGR2
         IF (MN2 .LE. 0) GO TO 110
         DO 100 I=1,MN2
            Y = YMIN+FLOAT(MN2P1*(J-1)+I)*DY
            CALL LINE3W (XMIN,Y,XMIN+DELXS,Y)
C
C SET LINE INTENSITY TO LOW
C
            CALL GSPLCI (ITHRMN)
            CALL LINE3W (XMAX,Y,XMAX-DELXS,Y)
  100    CONTINUE
  110    IF (J .GE. MGR2) GO TO 130
C
C SET LINE INTENSITY TO HIGH
C
         CALL GSPLCI (ITHRMJ)
         Y = YMIN+FLOAT(MN2P1*J)*DY
         CALL LINE3W (XMIN,Y,XMIN+DELXL,Y)
         CALL LINE3W (XMAX,Y,XMAX-DELXL,Y)
  120 CONTINUE
C
C RESTORE ASF AND LINE INTENSITY TO ORIGINAL
C
  130 LASF(3) = LSV3
      CALL GSASF (LASF)
      CALL GSPLCI (IPLCI)
      RETURN
      END
      SUBROUTINE LINE3W (XA,YA,XB,YB)
      SAVE
      COMMON /PRM31/  Q          ,L
      GO TO ( 10, 30, 40),L
   10 UA = Q
      UB = Q
      VA = XA
      VB = XB
   20 WA = YA
      WB = YB
      GO TO  50
   30 UA = XA
      UB = XB
      VA = Q
      VB = Q
      GO TO  20
   40 UA = XA
      UB = XB
      VA = YA
      VB = YB
      WA = Q
      WB = Q
   50 CALL LINE3 (UA,VA,WA,UB,VB,WB)
      RETURN
      END
      SUBROUTINE DRAWT (IXA,IYA,IXB,IYB)
      SAVE
      CALL PLOTIT(32*IXA,32*IYA,0)
      IIX = 32*IXB
      IIY = 32*IYB
      CALL PLOTIT(IIX,IIY,1)
C
C     FLUSH PLOTIT BUFFER
C
      CALL PLOTIT(IIX,IIY,0)
      RETURN
      END
      SUBROUTINE TICK43 (MAGU,MINU,MAGV,MINV,MAGW,MINW)
      SAVE
      COMMON /TCK31/  TMAGU      ,TMINU      ,TMAGV      ,TMINV      ,
     1                TMAGW      ,TMINW
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('GRAPHX','THREED','TICK43','VERSION  1')
      TMAGU = MAGU
      TMINU = MINU
      TMAGV = MAGV
      TMINV = MINV
      TMAGW = MAGW
      TMINW = MINW
      RETURN
      END
      SUBROUTINE TICK3 (MAG,MIN)
      SAVE
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('GRAPHX','THREED','TICK3','VERSION  1')
      CALL TICK43 (MAG,MIN,MAG,MIN,MAG,MIN)
      RETURN
      END
      SUBROUTINE FENCE3 (U,V,W,N,IOR,BOT)
      SAVE
      REAL            U(N)       ,V(N)       ,W(N)
      DIMENSION       LASF(13)
C
C COMMON BLOCK THRINT IS USED FOR SETTING COLOR INTENSITY
C
      COMMON /THRINT/ ITHRMJ     ,ITHRMN     ,ITHRTX
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('GRAPHX','THREED','FENCE3','VERSION  1')
C
C INQUIRE LINE COLOR INDEX AND SET ASF TO INDIVIDUAL
C
      CALL GQPLCI (IERR, IPLCI)
      CALL GQASF (IERR, LASF)
      LSV3  = LASF(3)
      LASF(3) = 1
      CALL GSASF (LASF)
C
      M = N
      BASE = BOT
      L = MAX0(1,MIN0(3,IOR))
C
C SET LINE INTENSITY TO LOW
C
      CALL GSPLCI (ITHRMN)
      GO TO ( 10, 40, 70),L
   10 CALL FRST3 (BASE,V(1),W(1))
      DO  20 I=2,M
         VV = V(I)
         WW = W(I)
         CALL VECT3 (BASE,VV,WW)
   20 CONTINUE
      DO  30 I=1,M
         UU = U(I)
         VV = V(I)
         WW = W(I)
         CALL LINE3 (UU,VV,WW,BASE,VV,WW)
   30 CONTINUE
      GO TO 100
   40 CALL FRST3 (U(1),BASE,W(1))
      DO  50 I=2,M
         UU = U(I)
         WW = W(I)
         CALL VECT3 (UU,BASE,WW)
   50 CONTINUE
      DO  60 I=1,M
         UU = U(I)
         VV = V(I)
         WW = W(I)
         CALL LINE3 (UU,VV,WW,UU,BASE,WW)
   60 CONTINUE
      GO TO 100
   70 CALL FRST3 (U(1),V(1),BASE)
      DO  80 I=2,M
         UU = U(I)
         VV = V(I)
         CALL VECT3 (UU,VV,BASE)
   80 CONTINUE
      DO  90 I=1,M
         UU = U(I)
         VV = V(I)
         WW = W(I)
         CALL LINE3 (UU,VV,WW,UU,VV,BASE)
   90 CONTINUE
C
C SET LINE INTENSITY TO HIGH
C
  100 CALL GSPLCI (ITHRMJ)
      CALL CURVE3 (U,V,W,M)
C
C RESTORE ASF AND LINE INTENSITY TO ORIGINAL
C
      LASF(3) = LSV3
      CALL GSASF (LASF)
      CALL GSPLCI (IPLCI)
C
      RETURN
C
C REVISION HISTORY---
C
C JANUARY 1978     DELETED REFERENCES TO THE  *COSY  CARDS AND
C                  ADDED REVISION HISTORY
C FEBURARY 1979    MODIFIED CODE TO CONFORM TO FORTRAN 66 STANDARD
C JUNE 1979        UPDATED FILE TO INCLUDE BLOCK DATA PWRZBD AND
C                  CORRECT A COMMENTED OUT STATEMENT IN CURVE3.
C MARCH 1980       REMOVED THE PWRZ AND PWRITZ ENTRIES.  THESE
C                  CAPABILITIES WERE REPLACED WITH THE NEW ULIB FILE
C                  PWRZT.
C JULY 1984        CONVERTED TO FORTRAN 77 AND GKS
C-----------------------------------------------------------------------
C
      END
      SUBROUTINE PWRZ (X,Y,Z,ID,N,ISIZE,LIN3,ITOP,ICNT)
C     WRITE (6,1001)
C     WRITE (6,1002)
C     STOP
C
C1001 FORMAT (1H1//////////)
C1002 FORMAT (' *****************************************'/
C    1        ' *                                       *'/
C    2        ' *                                       *'/
C    3        ' *   THE ENTRY POINT PWRZ IS NO LONGER   *'/
C    4        ' *   SUPPORTED.  THE CAPABILITIES OF     *'/
C    5        ' *   THIS OLD ENTRY ARE NOW AVAILABLE    *'/
C    6        ' *   IN THE NEW PORTABLE VERSIONS        *'/
C    7        ' *                                       *'/
C    8        ' *        PWRZS  FOR USE WITH SRFACE     *'/
C    9        ' *        PWRZI  FOR USE WITH ISOSRF     *'/
C    +        ' *        PWRZT  FOR USE WITH THREED     *'/
C    1        ' *                                       *'/
C    2        ' *   FOR USAGE OF THESE ROUTINES, SEE    *'/
C    3        ' *   THE DOCUMENTATION FOR THE DESIRED   *'/
C    4        ' *   ROUTINE.                            *'/
C    5        ' *                                       *'/
C    6        ' *                                       *'/
C    7        ' *****************************************')
C
      END
