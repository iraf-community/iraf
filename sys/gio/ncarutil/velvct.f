      SUBROUTINE VELVCT (U,LU,V,LV,M,N,FLO,HI,NSET,LENGTH,ISPV,SPV)
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
C SUBROUTINE VELVCT (U,LU,V,LV,M,N,FLO,HI,NSET,LENGTH,ISPV,SPV)
C
C
C DIMENSION OF           U(LU,N),V(LV,N),SPV(2)
C ARGUMENTS
C
C LATEST REVISION        JULY 1984
C
C PURPOSE                VELVCT DRAWS A REPRESENTATION OF A TWO-
C                        DIMENSIONAL VELOCITY FIELD BY DRAWING ARROWS
C                        FROM EACH DATA LOCATION.  THE LENGTH OF THE
C                        ARROW IS PROPORTIONAL TO THE STRENGTH OF THE
C                        FIELD AT THAT LOCATION AND THE DIRECTION OF
C                        THE ARROW INDICATES THE DIRECTION OF THE FLOW
C                        AT THAT LOCATION.
C
C USAGE                  IF THE FOLLOWING ASSUMPTIONS ARE MET, USE
C
C                               CALL EZVEC (U,V,M,N)
C
C                          ASSUMPTIONS -
C
C                            --THE WHOLE ARRAY IS PROCESSED.
C                            --THE SCALE FACTOR IS CHOSEN INTERNALLY.
C                            --THE PERIMETER IS DRAWN.
C                            --FRAME IS CALLED AFTER PLOTTING.
C                            --THERE ARE NO SPECIAL VALUES.
C
C                        IF THESE ASSUMPTIONS ARE NOT MET, USE
C
C                               CALL VELVCT (U,LU,V,LV,M,N,FLO,HI,
C                                                 NSET,LENGTH,ISPV,SPV)
C
C ARGUMENTS
C
C ON INPUT               U,V
C
C                          THE (ORIGINS OF THE) TWO-DIMENSIONAL ARRAYS
C                          CONTAINING THE VELOCITY FIELD TO BE PLOTTED.
C                          THE VECTOR AT THE POINT (I,J) HAS MAGNITUDE
C                          SQRT(U(I,J)**2+V(I,J)**2) AND DIRECTION
C                          ATAN2(V(I,J),U(I,J)).  OTHER REPRESENTATIONS,
C                          SUCH AS (R,THETA), CAN BE PLOTTED BY
C                          CHANGING STATEMENT FUNCTIONS IN THIS ROUTINE.
C
C                        LU
C
C                          THE FIRST DIMENSION OF U IN THE CALLING
C                          PROGRAM.
C
C                        LV
C
C                          THE FIRST DIMENSION OF V IN THE CALLING
C                          PROGRAM.
C
C                        M
C
C                          THE NUMBER OF DATA VALUES TO BE PLOTTED IN
C                          THE X-DIRECTION (THE FIRST SUBSCRIPT
C                          DIRECTION).  WHEN PLOTTING THE ENTIRE ARRAY,
C                          LU = LV = M.
C
C                        N
C
C                          THE NUMBER OF DATA VALUES TO BE PLOTTED IN
C                          THE Y-DIRECTION (THE SECOND SUBSCRIPT
C                          DIRECTION).
C
C                        FLO
C
C                          THE MINIMUM VECTOR MAGNITUDE TO BE SHOWN.
C
C                        HI
C
C                          THE MAXIMUM VECTOR MAGNITUDE TO BE SHOWN. (A
C                          VALUE LESS THAN OR EQUAL TO ZERO CAUSES THE
C                          MAXIMUM VALUE OF SQRT(U**2+V**2) TO BE USED.)
C
C                        NSET
C
C                          FLAG TO CONTROL SCALING -
C
C                          IF NSET IS ZERO, VELVCT ESTABLISHES THE
C                          WINDOW AND VIEWPORT TO PROPERLY
C                          SCALE PLOTTING INSTRUCTIONS TO THE STANDARD
C                          CONFIGURATION.  PERIM IS CALLED TO DRAW A
C                          BORDER.
C
C                          IF NSET IS GREATER THAN ZERO, VELVCT ASSUMES
C                          THAT THE USER HAS ESTABLISHED THE WINDOW
C                          AND VIEWPORT IN SUCH A WAY AS TO PROPERLY
C                          SCALE THE PLOTTING INSTRUCTIONS GENERATED
C                          BY VELVCT.  PERIM IS NOT CALLED.
C
C                          IF NSET IS LESS THAN ZERO, VELVCT
C                          PLACES THE CONTOUR PLOT
C                          WITHIN THE LIMITS OF THE USER'S CURRENT
C                          WINDOW AND VIEWPORT.  PERIM IS NOT CALLED.
C
C                        LENGTH
C
C                          THE LENGTH, IN PLOTTER ADDRESS UNITS (PAUS),
C                          OF A VECTOR HAVING MAGNITUDE HI
C                          (OR, IF HI=0, THE LENGTH IN PAUS
C                          OF THE LONGEST VECTOR).  IF LENGTH=0, A
C                          VALUE IS CHOSEN SUCH THAT THE LONGEST VECTOR
C                          COULD JUST REACH TO THE TAIL OF THE NEXT
C                          VECTOR.  IF THE HORIZONTAL AND VERTICAL
C                          RESOLUTIONS OF THE PLOTTER ARE DIFFERENT,
C                          LENGTH SHOULD BE NON-ZERO AND SPECIFIED AS A
C                          HORIZONTAL DISTANCE.
C
C                        ISPV
C
C                          FLAG TO CONTROL THE SPECIAL VALUE FEATURE.
C
C                             0 MEANS THAT THE FEATURE IS NOT IN USE.
C
C                             1 MEANS THAT IF THE VALUE OF
C                               U(I,J)=SPV(1) THE VECTOR WILL NOT BE
C                               PLOTTED.
C
C                             2 MEANS THAT IF THE VALUE OF
C                               V(I,J)=SPV(2) THE VECTOR WILL NOT BE
C                               PLOTTED.
C
C                             3 MEANS THAT IF EITHER U(I,J)=SPV(1) OR
C                               V(I,J)=SPV(2) THEN THE VECTOR WILL NOT
C                               BE PLOTTED.
C
C                             4 MEANS THAT IF U(I,J)=SPV(1)
C                               AND V(I,J)=SPV(2), THE VECTOR
C                               WILL NOT BE PLOTTED.
C
C                        SPV
C
C                        AN ARRAY OF LENGTH 2 WHICH GIVES THE VALUE
C                        IN THE U ARRAY AND THE VALUE IN THE V ARRAY
C                        WHICH DENOTE MISSING VALUES.
C                        THIS ARGUMENT IS IGNORED IF ISPV=0.
C
C
C ON OUTPUT              ALL ARGUMENTS REMAIN UNCHANGED.
C
C NOTE                   THE ENDPOINTS OF EACH ARROW DRAWN ARE (FX(X,Y),
C                        FY(X,Y)) AND (MXF(X,Y,U,V,SFX,SFY,MX,MY),
C                        MYF(X,Y,U,V,SFX,SFY,MX,MY)) WHERE X=I, Y=J,
C                        U=U(I,J), V=V(I,J), AND SFX AND SFY ARE SCALE
C                        FACTORS.  HERE I IS THE X-INDEX AND J IS THE
C                        Y-INDEX.  (MX,MY) IS THE LOCATION OF THE TAIL.
C                        THUS THE ACTUAL LENGTH OF THE ARROW IS
C                        SQRT(DX**2+DY**2) AND THE DIRECTION IS
C                        ATAN2(DX,DY), WHERE DX=MX-MXF(...) AND
C                        DY=MY-MYF(...).
C
C ENTRY POINTS           VELVCT,EZVECT,DRWVEC,VELVEC,VELDAT
C
C COMMON BLOCKS          VEC1,VEC2
C
C I/O                    PLOTS THE VECTOR FIELD.
C
C PRECISION              SINGLE
C
C LANGUAGE               FORTRAN
C
C REQUIRED LIBRARY       GRIDAL AND THE SPPS
C ROUTINES
C
C HISTORY                WRITTEN AND STANDARDIZED IN NOVEMBER 1973.
C                        REVISED IN MAY, 1975, TO INCLUDE MXF AND MYF.
C                        REVISED IN MARCH, 1981, TO FIX CERTAIN ERRORS;
C                        TO USE FL2INT AND PLOTIT INSTEAD OF MXMY,
C                        FRSTPT, AND VECTOR; AND TO MAKE THE ARROWHEADS
C                        NARROWER.  CONVERTED TO FORTRAN77 AND GKS
C                        IN JULY 1984.
C
C ALGORITHM              EACH VECTOR IS EXAMINED, POSSIBLY TRANSFORMED,
C                        THEN PLOTTED.
C
C PORTABILITY            FORTRAN77
C
C ---------------------------------------------------------------------
C
C SPECIAL NOTE -
C
C USING THIS ROUTINE TO PUT VECTORS ON AN ARBITRARY BACKGROUND DRAWN BY
C SUPMAP IS A BIT TRICKY.  THE ARITHMETIC STATEMENT FUNCTIONS FX AND FY
C ARE EASY TO REPLACE.  THE PROBLEM ARISES IN REPLACING MXF AND MYF.
C THE FOLLOWING EXAMPLE MAY BE HELPFUL. (SUPMAP IS AN ENTRY POINT IN
C THE EZMAP PACKAGE.)
C
C SUPPOSE THAT WE HAVE TWO ARRAYS, CLON(36,9) AND CLAT(36,9), WHICH
C CONTAIN THE E-W AND N-S COMPONENTS OF A WIND FLOW FIELD ON THE SURFACE
C OF THE EARTH.  CLON(I,J) IS THE MAGNITUDE OF THE EASTERLY FLOW.
C CLAT(I,J) IS THE MAGNITUDE OF THE NORTHERLY FLOW AT A LONGITUDE (I-1)
C *10 DEGREES EAST OF GREENWICH AND A LATITUDE (J-1)*10 DEGREES NORTH OF
C THE EQUATOR.  SUPMAP IS TO BE USED TO DRAW A POLAR PROJECTION OF THE
C EARTH AND VELVCT IS TO BE USED TO SUPERIMPOSE VECTORS REPRESENTING THE
C FLOW FIELD ON IT.  THE FOLLOWING STEPS WOULD BE NECESSARY:
C
C     1.  CALL SUPMAP (1,90.,0.,-90.,90.,90.,90.,90.,-4,10,0,1,IER)
C         TO DRAW THE MAP.
C
C     2.  CALL VELVCT (CLON,36,CLAT,36,36,9,0.,0.,1,50,0,0.) TO PUT
C         VECTORS ON IT.  NOTICE THAT NSET HAS THE VALUE 1 TO TELL
C         VELVCT THAT SUPMAP HAS DONE THE REQUIRED SET CALL.
C
C     3.  IN ORDER TO ENSURE THAT STEP 2 WILL WORK PROPERLY, DELETE
C         THE ARITHMETIC STATEMENT FUNCTIONS FX, FY, MXF, AND MYF
C         FROM VELVCT AND INCLUDE THE FOLLOWING FUNCTIONS.
C
C     FUNCTION FX(XX,YY)
C     CALL MAPTRN (10.*(YY-1.),10.*(XX-1.),X,Y)
C     FX=X
C     RETURN
C     END
C
C     FUNCTION FY(XX,YY)
C     CALL MAPTRN (10.*(YY-1.),10.*(XX-1.),X,Y)
C     FY=Y
C     RETURN
C     END
C
C     FUNCTION MXF(XX,YY,UU,VV,SFX,SFY,MX,MY)
C     CFCT=COS(.17453292519943*(YY-1.))
C     CALL MAPTRN(10.*(YY-1.)         ,10.*(XX-1.)              ,X1,Y1)
C     CALL MAPTRN(10.*(YY-1.)+1.E-6*VV,10.*(XX-1.)+1.E-6*UU/CFCT,X2,Y2)
C     U=((X2-X1)/SQRT((X2-X1)**2+(Y2-Y1)**2))*SQRT(UU**2+VV**2)
C     MXF=MX+IFIX(SFX*U)
C     RETURN
C     END
C
C     FUNCTION MYF(XX,YY,UU,VV,SFX,SFY,MX,MY)
C     CFCT=COS(.17453292519943*(YY-1.))
C     CALL MAPTRN(10.*(YY-1.)         ,10.*(XX-1.)              ,X1,Y1)
C     CALL MAPTRN(10.*(YY-1.)+1.E-6*VV,10.*(XX-1.)+1.E-6*UU/CFCT,X2,Y2)
C     V=((Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2))*SQRT(UU**2+VV**2)
C     MYF=MY+IFIX(SFY*V)
C     RETURN
C     END
C
C THE BASIC NOTION BEHIND THE CODING OF THE MXF AND MYF FUNCTIONS IS AS
C FOLLOWS.  SINCE UU AND VV ARE THE LONGITUDINAL AND LATITUDINAL COMPONENTS,
C RESPECTIVELY, OF A VELOCITY VECTOR HAVING UNITS OF DISTANCE OVER TIME,
C 1.E-6*UU/COS(LATITUDE) AND 1.E-6*VV REPRESENT THE CHANGE IN LONGITUDE
C AND LATITUDE, RESPECTIVELY, OF A PARTICLE MOVING WITH THE FLOW FIELD
C FOR A VERY SHORT PERIOD OF TIME.  THE ROUTINE MAPTRN IS USED TO FIND
C THE POSITION OF THE PARTICLE'S PROJECTION AT THE BEGINNING AND END OF
C THAT TINY TIME SLICE AND, THEREFORE, THE DIRECTION IN WHICH TO DRAW
C THE ARROW REPRESENTING THE VELOCITY VECTOR SO THAT IT WILL BE TANGENT
C TO A PROJECTED FLOW LINE OF THE FIELD AT THAT POINT.  THE VALUES U
C AND V ARE COMPUTED SO AS TO GIVE THE ARROW THE LENGTH IMPLIED BY UU
C AND VV.  (THE CODE ENSURES THAT SQRT(U**2+V**2) IS EQUAL TO
C SQRT(UU**2+VV**2).)  THE LENGTH OF THE ARROW REPRESENTS THE MAGNITUDE
C OF THE VELOCITY VECTOR, UNAFFECTED BY PERSPECTIVE.  THE SCALING SET
C UP BY VELVCT WILL THEREFORE BE APPROPRIATE FOR THE ARROWS DRAWN.
C
C THIS METHOD IS RATHER HEURISTIC AND HAS THREE INHERENT PROBLEMS.
C FIRST, THE CONSTANT 1.E-6 MAY NEED TO BE MADE LARGER OR SMALLER,
C DEPENDING ON THE MAGNITUDE OF YOUR U/V DATA.  SECOND, THE NORTH AND
C SOUTH POLES MUST BE AVOIDED.  AT EITHER POLE, CFCT GOES TO ZERO,
C GIVING A DIVISION BY ZERO; IN A SMALL REGION NEAR THE POLE, THE
C METHOD MAY TRY TO USE MAPTRN WITH A LATITUDE OUTSIDE THE RANGE
C (-90,+90).  THIRD, THE PROJECTION MUST BE SET UP SO AS TO AVOID
C HAVING VECTOR BASEPOINTS AT THE EXACT EDGE OF THE MAP.  VECTORS
C THERE WILL BE OF THE CORRECT LENGTH, BUT THEY MAY BE DRAWN IN THE
C WRONG DIRECTION (WHEN THE PROJECTED PARTICLE TRACK DETERMINING THE
C DIRECTION CROSSES THE EDGE AND REAPPEARS ELSEWHERE ON THE MAP).
C WITH A LITTLE CARE, THE DESIRED RESULTS MAY BE OBTAINED.
C ---------------------------------------------------------------------
C
C DECLARATIONS -
C
      COMMON /VEC1/   ASH        ,EXT        ,ICTRFG     ,ILAB       ,
     +                IOFFD      ,IOFFM      ,ISX        ,ISY        ,
     +                RMN        ,RMX        ,SIDE       ,SIZE       ,
     +                XLT        ,YBT        ,ZMN        ,ZMX
C
      COMMON /VEC2/   BIG        ,INCX       ,INCY
C
C ARGUMENT DIMENSIONS.
C
      DIMENSION       U(LU,N)    ,V(LV,N)    ,SPV(2)
        CHARACTER*10    LABEL
        REAL WIND(4), VIEW(4), IAR(4)
C
C ---------------------------------------------------------------------
C
C INTERNAL PARAMETERS OF VELVCT ARE AS FOLLOWS.  THE DEFAULT VALUES OF
C THESE PARAMETERS ARE DECLARED IN THE BLOCK DATA ROUTINE VELDAT.
C
C                        NAME   DEFAULT  FUNCTION
C                        ----   -------  --------
C
C                        BIG   R1MACH(2) CONSTANT USED TO INITIALIZE
C                                        POSSIBLE SEARCH FOR HI.
C
C                        EXT     0.25    THE LENGTHS OF THE SIDES OF THE
C                                        PLOT ARE PROPORTIONAL TO M AND
C                                        N WHEN NSET IS LESS THAN OR
C                                        EQUAL TO ZERO, EXCEPT WHEN
C                                        MIN(M,N)/MAX(M,N) IS LESS THAN
C                                        EXT, IN WHICH CASE A SQUARE
C                                        GRAPH IS PLOTTED.
C
C                        ICTRFG    1     FLAG TO CONTROL THE POSITION OF
C                                        THE ARROW RELATIVE TO  A BASE
C                                        POINT AT (MX,MY).
C
C                                        ZERO - CENTER AT (MX,MY)
C
C                                        POSITIVE - TAIL AT (MX,MY)
C
C                                        NEGATIVE -  HEAD AT (MX,MY)
C
C                        ILAB      0     FLAG TO CONTROL THE DRAWING OF
C                                        LINE LABELS.
C
C                                        ZERO - DO NOT DRAW THE LABELS
C
C                                        NON-ZERO - DRAW THE LABELS
C
C                        INCX      1     X-COORDINATE STEP SIZE FOR LESS
C                                        DENSE ARRAYS.
C
C                        INCY      1     Y-COORDINATE STEP SIZE.
C
C                        IOFFD     0     FLAG TO CONTROL NORMALIZATION
C                                        OF LABEL NUMBERS.
C
C                                        ZERO - INCLUDE A DECIMAL POINT
C                                        WHEN POSSIBLE
C
C                                        NON-ZERO - NORMALIZE ALL LABEL
C                                        NUMBERS BY ASH
C
C                        IOFFM     0     FLAG TO CONTROL PLOTTING OF
C                                        THE MESSAGE BELOW THE PLOT.
C
C                                        ZERO - PLOT THE MESSAGE
C
C                                        NON-ZERO - DO NOT PLOT IT
C
C                        RMN     160.    ARROW SIZE BELOW WHICH THE
C                                        HEAD NO LONGER SHRINKS, ON A
C                                        2**15 X 2**15 GRID.
C
C                        RMX    6400.    ARROW SIZE ABOVE WHICH THE
C                                        HEAD NO LONGER GROWS LARGER,
C                                        ON A 2**15 X 2**15 GRID.
C
C                        SIDE    0.90    LENGTH OF LONGER EDGE OF PLOT.
C                                        (SEE ALSO EXT.)
C
C                        SIZE    256.    WIDTH OF THE CHARACTERS IN
C                                        VECTOR LABELS, ON A 2**15 X
C                                        2**15 GRID.
C
C                        XLT     0.05    LEFT HAND EDGE OF THE PLOT.
C                                        (0 IS THE LEFT EDGE OF THE
C                                        FRAME, 1 THE RIGHT EDGE.)
C
C                        YBT     0.05    BOTTOM EDGE OF THE PLOT (0 IS
C                                        THE BOTTOM OF THE FRAME, 1 THE
C                                        TOP OF THE FRAME.)
C
C ---------------------------------------------------------------------
C
C INTERNAL FUNCTIONS WHICH MAY BE MODIFIED FOR DATA TRANSFORMATION -
C
C                        SCALE    COMPUTES A SCALE FACTOR USED IN THE
C                                 DETERMINATION OF THE LENGTH OF THE
C                                 VECTOR TO BE DRAWN.
C
C                        DIST     COMPUTES THE LENGTH OF A VECTOR.
C
C                        FX       RETURNS THE X INDEX AS THE
C                                 X-COORDINATE OF THE VECTOR BASE.
C
C                        MXF      RETURNS THE X-COORDINATE OF THE VECTOR
C                                 HEAD.
C
C                        FY       RETURNS THE Y INDEX AS THE
C                                 Y-COORDINATE OF THE VECTOR BASE.
C
C                        MYF      RETURNS THE Y-COORDINATE OF THE VECTOR
C                                 HEAD.
C
C                        VLAB     THE VALUE FOR THE VECTOR LABEL WHEN
C                                 ILAB IS NON-ZERO.
C
      SAVE
      DIST(XX,YY) = SQRT(XX*XX+YY*YY)
      FX(XX,YY) = XX
      FY(XX,YY) = YY
      MXF(XX,YY,UU,VV,SFXX,SFYY,MXX,MYY) = MXX+IFIX(SFXX*UU)
      MYF(XX,YY,UU,VV,SFXX,SFYY,MXX,MYY) = MYY+IFIX(SFYY*VV)
      SCALEX(MM,NN,INCXX,INCYY,HAA,XX1,XX2,YY1,YY2,XX3,XX4,YY3,YY4,
     1       LENN) = LENN/HAA
      SCALEY(MM,NN,INCXX,INCYY,HAA,XX1,XX2,YY1,YY2,XX3,XX4,YY3,YY4,
     1       LENN) = SCALEX(MM,NN,INCXX,INCYY,HAA,XX1,XX2,YY1,YY2,XX3,
     2                                                 XX4,YY3,YY4,LENN)
      VLAB(UU,VV,II,JJ) = DIST(UU,VV)
C
C FORCE THE BLOCK DATA ROUTINE, WHICH SETS DEFAULT VARIABLES, TO LOAD.
C +NOAO - blockdata replaced with run time initialization.
C
C     EXTERNAL        VELDAT
      call veldat
C -NOAO
C
C ---------------------------------------------------------------------
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR.
C
      CALL Q8QST4 ('NSSL','VELVCT','VELVCT','VERSION  6')
C
C INITIALIZE AND TRANSFER SOME ARGUMENTS TO LOCAL VARIABLES.
C
      BIG = -R1MACH(2)
      MX = LU
      MY = LV
      NX = M
      NY = N
      GL = FLO
      HA = HI
      ISP = ISPV
      NC = 0
C
C COMPUTE CONSTANTS BASED ON THE ADDRESSABILITY OF THE PLOTTER.
C
      CALL GETUSV('XF',ISX)
      CALL GETUSV('YF',ISY)
      ISX = 2**(15-ISX)
      ISY = 2**(15-ISY)
      LEN = LENGTH*ISX
C
C SET UP THE SCALING OF THE PLOT.
C
        CALL GQCNTN(IERR,IOLDNT)
        CALL GQNT(IOLDNT,IERR,WIND,VIEW)
        X1 = VIEW(1)
        X2 = VIEW(2)
        Y1 = VIEW(3)
        Y2 = VIEW(4)
        X3 = WIND(1)
        X4 = WIND(2)
        Y3 = WIND(3)
        Y4 = WIND(4)
        CALL GETUSV('LS',IOLLS)
C
C     SAVE NORMALIZATION TRANSFORMATION 1
C
      CALL GQNT(1,IERR,WIND,VIEW)
C
      IF (NSET) 101,102,106
C
  101 X3 = 1.
      X4 = FLOAT(NX)
      Y3 = 1.
      Y4 = FLOAT(NY)
      GO TO 105
C
  102 X1 = XLT
      X2 = XLT+SIDE
      Y1 = YBT
      Y2 = YBT+SIDE
      X3 = 1.
      Y3 = 1.
      X4 = FLOAT(NX)
      Y4 = FLOAT(NY)
      IF (AMIN1(X4,Y4)/AMAX1(X4,Y4) .LT. EXT) GO TO 105
C
      IF (NX-NY) 103,105,104
  103 X2 = XLT+SIDE*X4/Y4
      GO TO 105
  104 Y2 = YBT+SIDE*Y4/X4
C
  105 CALL SET(X1,X2,Y1,Y2,X3,X4,Y3,Y4,1)
      IF (NSET .EQ. 0) CALL PERIM (1,0,1,0)
C
C CALCULATE A LENGTH IF NONE PROVIDED.
C
  106 IF (LEN .NE. 0) GO TO 107
      CALL FL2INT(FX(1.,1.),FY(1.,1.),MX,MY)
      CALL FL2INT(FX(FLOAT(1+INCX),FLOAT(1+INCY)),
     +            FY(FLOAT(1+INCX),FLOAT(1+INCY)),LX,LY)
      LEN = SQRT((FLOAT(MX-LX)**2+FLOAT(MY-LY)**2)/2.)
C
C SET UP SPECIAL VALUES.
C
  107 IF (ISP .EQ. 0) GO TO 108
      SPV1 = SPV(1)
      SPV2 = SPV(2)
      IF (ISP .EQ. 4) SPV2 = SPV(1)
C
C FIND THE MAXIMUM VECTOR LENGTH.
C
  108 IF (HA .GT. 0.) GO TO 118
C
      HA = BIG
      IF (ISP .EQ. 0) GO TO 115
C
      DO 114 J=1,NY,INCY
         DO 113 I=1,NX,INCX
            IF (ISP-2) 109,111,110
  109       IF (U(I,J) .EQ. SPV1) GO TO 113
            GO TO 112
  110       IF (U(I,J) .EQ. SPV1) GO TO 113
  111       IF (V(I,J) .EQ. SPV2) GO TO 113
  112       HA = AMAX1(HA,DIST(U(I,J),V(I,J)))
  113    CONTINUE
  114 CONTINUE
      GO TO 126
C
  115 DO 117 J=1,NY,INCY
         DO 116 I=1,NX,INCX
            HA = AMAX1(HA,DIST(U(I,J),V(I,J)))
  116    CONTINUE
  117 CONTINUE
C
C BRANCH IF NULL VECTOR SIZE.
C
  126 IF (HA .LE. 0.) GO TO 125
C
C COMPUTE SCALE FACTORS.
C
  118 SFX = SCALEX(M,N,INCX,INCY,HA,X1,X2,Y1,Y2,X3,X4,Y3,Y4,LEN)
      SFY = SCALEY(M,N,INCX,INCY,HA,X1,X2,Y1,Y2,X3,X4,Y3,Y4,LEN)
      IOFFDT = IOFFD
      IF (GL.NE.0.0 .AND. (ABS(GL).LT.0.1 .OR. ABS(GL).GE.1.E5))
     1    IOFFDT = 1
      IF (HA.NE.0.0 .AND. (ABS(HA).LT.0.1 .OR. ABS(HA).GE.1.E5))
     1    IOFFDT = 1
      ASH = 1.0
      IF (IOFFDT .NE. 0)
     1    ASH = 10.**(3-IFIX(ALOG10(AMAX1(ABS(GL),ABS(HA)))-500.)-500)
      IZFLG = 0
C
C COMPUTE ZMN AND ZMX, WHICH ARE USED IN DRWVEC.
C
      ZMN = LEN*(GL/HA)
      ZMX = FLOAT(LEN)+.01
C
C DRAW THE VECTORS.
C
      DO 123 J=1,NY,INCY
         DO 122 I=1,NX,INCX
            UI = U(I,J)
            VI = V(I,J)
            IF (ISP-1) 121,119,120
  119       IF (UI-SPV1) 121,122,121
  120       IF (VI .EQ. SPV2) GO TO 122
            IF (ISP .GE. 3) GO TO 119
  121       X = I
            Y = J
            CALL FL2INT(FX(X,Y),FY(X,Y),MX,MY)
            LX = MAX0(1,MXF(X,Y,UI,VI,SFX,SFY,MX,MY))
            LY = MAX0(1,MYF(X,Y,UI,VI,SFX,SFY,MX,MY))
            IZFLG = 1
            IF (ILAB .NE. 0) CALL ENCD(VLAB(UI,VI,I,J),ASH,LABEL,NC,
     +                                                           IOFFDT)
            CALL DRWVEC (MX,MY,LX,LY,LABEL,NC)
  122    CONTINUE
  123 CONTINUE
C
      IF (IZFLG .EQ. 0) GO TO 125
C
      IF (IOFFM .NE. 0) GO TO 200
C +NOAO - FTN internal write replaced with call to encode
C     WRITE(LABEL,'(E10.3)')HA
      call encode (10, '(e10.3)', label, ha)
C -NOAO
C
C     TURN OFF CLIPPING SO ARROW CAN BE DRAWN
C
      CALL GQCLIP(IER,ICLP,IAR)
      CALL GSCLIP(0)
      CALL DRWVEC (28768,608,28768+LEN,608,LABEL,10)
C
C     RESTORE CLIPPING
C
      CALL GSCLIP(ICLP)
        IX = 1+(28768+LEN/2)/ISX
        IY = 1+(608-(5*ISX*MAX0(256/ISX,8))/4)/ISY
        CALL GQCNTN(IER,ICN)
        CALL GSELNT(0)
        XC = CPUX(IX)
        YC = CPUY(IY)
      CALL WTSTR (XC,YC,
     +                         'MAXIMUM VECTOR',MAX0(256/ISX,8),0,0)
      CALL GSELNT(ICN)
C
C DONE.
C
      GOTO 200
C
C ZERO-FIELD ACTION.
C
  125 IX = 1+16384/ISX
        IY = 1+16384/ISY
        CALL GQCNTN(IER,ICN)
        CALL GSELNT(0)
        XC = CPUX(IX)
        YC = CPUY(IY)
      CALL WTSTR (XC,YC,
     +                             'ZERO FIELD',MAX0(960/ISX,8),0,0)
        CALL GSELNT(ICN)
C
C RESTORE TRANS 1 AND LOG SCALING AND ORIGINAL TRANS NUMBER
C
  200 CONTINUE
      IF (NSET .LE. 0) THEN
        CALL SET(VIEW(1),VIEW(2),VIEW(3),VIEW(4),
     -           WIND(1),WIND(2),WIND(3),WIND(4),IOLLS)
      ENDIF
      CALL GSELNT(IOLDNT)
      RETURN
      END
      SUBROUTINE EZVEC (U,V,M,N)
C
C THIS SUBROUTINE IS FOR THE USER WHO WANTS A QUICK-AND-DIRTY VECTOR
C PLOT WITH DEFAULT VALUES FOR MOST OF THE ARGUMENTS.
C
        SAVE
C
      DIMENSION       U(M,N)     ,V(M,N)     ,SPVAL(2)
C
      DATA FLO,HI,NSET,LENGTH,ISPV,SPVAL(1),SPVAL(2) /
     +      0.,0.,   0,     0,   0,      0.,      0. /
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR.
C
      CALL Q8QST4 ('CRAYLIB','VELVCT','EZVEC','VERSION  6')
C
      CALL VELVCT (U,M,V,M,M,N,FLO,HI,NSET,LENGTH,ISPV,SPVAL)
C +NOAO - call to frame is suppressed.
C     CALL FRAME
C -NOAO
      RETURN
      END
      SUBROUTINE DRWVEC (M1,M2,M3,M4,LABEL,NC)
C
C THIS ROUTINE IS CALLED TO DRAW A SINGLE ARROW.  IT HAS ARGUMENTS AS
C FOLLOWS -
C
C     (M1,M2)  -  COORDINATE OF ARROW BASE, ON A 2**15 X 2**15 GRID.
C     (M3,M4)  -  COORDINATE OF ARROW HEAD, ON A 2**15 X 2**15 GRID.
C     LABEL    -  CHARACTER LABEL TO BE PUT ABOVE ARROW.
C     NC       -  NUMBER OF CHARACTERS IN LABEL.
C
        SAVE
C
C
      COMMON /VEC1/   ASH        ,EXT        ,ICTRFG     ,ILAB       ,
     +                IOFFD      ,IOFFM      ,ISX        ,ISY        ,
     +                RMN        ,RMX        ,SIDE       ,SIZE       ,
     +                XLT        ,YBT        ,ZMN        ,ZMX
        CHARACTER*10 LABEL
C
C SOME LOCAL PARAMETERS ARE THE FOLLOWING -
C
C     CL     -  ARROW HEAD LENGTH SCALE FACTOR - EACH SIDE OF THE ARROW
C               HEAD IS THIS LONG RELATIVE TO THE LENGTH OF THE ARROW
C     ST,CT  -  SIN AND COS OF THE ARROW HEAD ANGLE
C     PI     -  THE CONSTANT PI
C     TWOPI  -  TWO TIMES PI
C     OHOPI  -  ONE HALF OF PI
C     FHOPI  -  FIVE HALVES OF PI
C
      DATA    CL / .25 /
      DATA    ST / .382683432365090 /
      DATA    CT / .923879532511287 /
      DATA    PI / 3.14159265358979 /
      DATA TWOPI / 6.28318530717959 /
      DATA OHOPI / 1.57079632679489 /
      DATA FHOPI / 7.85398163397448 /
C
      DIST(X,Y) = SQRT(X*X+Y*Y)
C
C TRANSFER ARGUMENTS TO LOCAL VARIABLES AND COMPUTE THE VECTOR LENGTH.
C
      N1 = M1
      N2 = M2
      N3 = M3
      N4 = M4
      DX = N3-N1
      DY = N4-N2
      R = DIST(DX,DY)
C
C SORT OUT POSSIBLE CASES, DEPENDING ON VECTOR LENGTH.
C
      IF (R .LE. ZMN) RETURN
C
      IF (R .LE. ZMX) GO TO 101
C
C PLOT A POINT FOR VECTORS WHICH ARE TOO LONG.
C
      CALL PLOTIT (N1,N2,0)
      CALL PLOTIT (N1,N2,1)
      CALL PLOTIT (N1,N2,0)
      RETURN
C
C ADJUST THE COORDINATES OF THE VECTOR ENDPOINTS AS IMPLIED BY THE
C CENTERING OPTION.
C
  101 IF (ICTRFG) 102,103,104
C
  102 N3 = N1
      N4 = N2
      N1 = FLOAT(N1)-DX
      N2 = FLOAT(N2)-DY
      GO TO 104
C
  103 N1 = FLOAT(N1)-.5*DX
      N2 = FLOAT(N2)-.5*DY
      N3 = FLOAT(N3)-.5*DX
      N4 = FLOAT(N4)-.5*DY
C
C DETERMINE THE COORDINATES OF THE POINTS USED TO DRAW THE ARROWHEAD.
C
  104 C1 = CL
C
C SHORT ARROWS HAVE HEADS OF A FIXED MINIMUM SIZE.
C
      IF (R .LT. RMN) C1 = RMN*CL/R
C
C LONG ARROWS HAVE HEADS OF A FIXED MAXIMUM SIZE.
C
      IF (R .GT. RMX) C1 = RMX*CL/R
C
C COMPUTE THE COORDINATES OF THE HEAD.
C
      N5 = FLOAT(N3)-C1*(CT*DX-ST*DY)
      N6 = FLOAT(N4)-C1*(CT*DY+ST*DX)
      N7 = FLOAT(N3)-C1*(CT*DX+ST*DY)
      N8 = FLOAT(N4)-C1*(CT*DY-ST*DX)
C
C PLOT THE ARROW.
C
      CALL PLOTIT (N1,N2,0)
      CALL PLOTIT (N3,N4,1)
      CALL PLOTIT (N5,N6,0)
      CALL PLOTIT (N3,N4,1)
      CALL PLOTIT (N7,N8,1)
      CALL PLOTIT (0,0,0)
C
C IF REQUESTED, PUT THE VECTOR MAGNITUDE ABOVE THE ARROW.
C
      IF (NC .EQ. 0) RETURN
      PHI = ATAN2(DY,DX)
      IF (AMOD(PHI+FHOPI,TWOPI) .GT. PI) PHI = PHI+PI
      IX = 1+IFIX(.5*FLOAT(N1+N3)+1.25*
     +            FLOAT(ISX*MAX0(IFIX(SIZE)/ISX,8))*COS(PHI+OHOPI))/ISX
      IY = 1+IFIX(.5*FLOAT(N2+N4)+1.25*
     +            FLOAT(ISX*MAX0(IFIX(SIZE)/ISX,8))*SIN(PHI+OHOPI))/ISY
        CALL GQCNTN(IER,ICN)
        CALL GSELNT(0)
        XC = CPUX(IX)
        YC = CPUY(IY)
      CALL WTSTR(XC,YC,
     +           LABEL,MAX0(IFIX(SIZE)/ISX,8),
     +                                     IFIX(57.2957795130823*PHI),0)
        CALL GSELNT(ICN)
      RETURN
      END
      SUBROUTINE VELVEC (U,LU,V,LV,M,N,FLO,HI,NSET,ISPV,SPV)
C
C THIS ROUTINE SUPPORTS USERS OF THE OLD VERSION OF THIS PACKAGE.
C
      DIMENSION       U(LU,N)    ,V(LV,N)    ,SPV(2)
C
        SAVE
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR.
C
      CALL Q8QST4 ('CRAYLIB','VELVCT','VELVEC','VERSION  4')
      CALL VELVCT (U,LU,V,LV,M,N,FLO,HI,NSET,0,ISPV,SPV)
      RETURN
      END
C
C REVISION HISTORY ----------------------------------------------------
C
C FEBRUARY, 1979   ADDED REVISION HISTORY
C                  MODIFIED CODE TO CONFORM TO FORTRAN 66 STANDARD
C
C JULY, 1979       FIXED HI VECTOR TRAP AND MESSAGE INDICATING
C                  MAXIMUM VECTOR PLOTTED.
C
C DECEMBER, 1979   CHANGED THE STATISTICS CALL FROM CRAYLIB TO NSSL
C
C MARCH, 1981      FIXED SOME FRINGE-CASE ERRORS, CHANGED THE CODE TO
C                  USE FL2INTT AND PLOTIT INSTEAD OF MXMY, FRSTPT, AND
C                  VECTOR, AND MADE THE ARROWHEADS NARROWER (45 DEGREES
C                  APART, RATHER THAN 60 DEGREES APART)
C
C FEBRUARY, 1984   PROVIDED A DIMENSION STATEMENT FOR A VARIABLE INTO
C                  WHICH A TEN-CHARACTER STRING WAS BEING ENCODED.  ON
C                  THE CRAY, WHEN THE ENCODE WAS DONE, A WORD FOLLOWING
C                  THE VARIABLE WAS CLOBBERED, BUT THIS APPARENTLY MADE
C                  NO DIFFERENCE.  ON AT LEAST ONE OTHER MACHINE, THE
C                  CODE BLEW UP.  (ERROR REPORTED BY GREG WOODS)
C
C JULY, 1984       CONVERTED TO FORTRAN77 AND GKS.
C
C ---------------------------------------------------------------------
