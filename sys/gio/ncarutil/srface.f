      SUBROUTINE SRFACE (X,Y,Z,M,MX,NX,NY,S,STEREO)
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
C DIMENSION OF           X(NX),Y(NY),Z(MX,NY),M(2,NX,NY),S(6)
C ARGUMENTS
C
C LATEST REVISION        MARCH 1984
C
C PURPOSE                SRFACE DRAWS A PERSPECTIVE PICTURE OF A
C                        FUNCTION OF TWO VARIABLES WITH HIDDEN LINES
C                        REMOVED.  THE FUNCTION IS APPROXIMATED BY A
C                        TWO-DIMENSIONAL ARRAY OF HEIGHTS.
C
C USAGE                  IF THE FOLLOWING ASSUMPTIONS ARE MET, USE
C                                 CALL EZSRFC (Z,M,N,ANGH,ANGV,WORK)
C
C                          ASSUMPTIONS:
C                                  .THE ENTIRE ARRAY IS TO BE DRAWN,
C                                  .THE DATA IS EQUALLY SPACED (IN THE
C                               X-Y PLANE),
C                                  .NO STEREO PAIRS,
C                                  .SCALING     IS CHOSEN INTERNALLY.
C
C                        IF THESE ASSUMPTIONS ARE NOT MET USE
C                                 CALL SRFACE (X,Y,Z,M,MX,NX,NY,S,
C                                                STEREO)
C
C ARGUMENTS
C
C ON INPUT               Z
C FOR EZSRFC                     THE M BY N ARRAY TO BE DRAWN.
C
C                        M
C                          THE FIRST DIMENSION OF Z.
C
C                        N
C                          THE SECOND DIMENSION OF Z.
C
C                        ANGH
C                          ANGLE IN DEGREES IN THE X-Y PLANE TO THE
C                          LINE OF SIGHT (COUNTER-CLOCK WISE FROM
C                          THE PLUS-X AXIS).
C
C                        ANGV
C                          ANGLE IN DEGREES FROM THE X-Y PLANE TO
C                          THE LINE OF SIGHT (POSITIVE ANGLES ARE
C                          ABOVE THE MIDDLE Z, NEGATIVE BELOW).
C
C                        WORK
C                          A SCRATCH STORAGE DIMENSIONED AT LEAST
C                          2*M*N+M+N.
C
C ON OUTPUT              Z, M, N, ANGH, ANGV ARE UNCHANGED.  WORK
C FOR EZSRFC             HAS BEEN WRITTEN IN.
C
C
C ARGUMENTS
C
C ON INPUT               X
C FOR SRFACE                     A LINEAR ARRAY NX LONG CONTAINING THE X
C                          COORDINATES OF THE POINTS IN THE SURFACE
C                          APPROXIMATION.  SEE NOTE, BELOW.
C
C                        Y
C                          THE LINEAR ARRAY NY LONG CONTAINING THE
C                          Y COORDINATES OF THE POINTS IN THE
C                          SURFACE APPROXIMATION.  SEE NOTE, BELOW.
C
C                        Z
C                          AN ARRAY MX BY NY CONTAINING THE SURFACE
C                          TO BE DRAWN IN NX BY NY CELLS.
C                          Z(I,J) = F(X(I),Y(J)).  SEE NOTE, BELOW.
C
C                        M
C                          SCRATCH ARRAY AT LEAST 2*NX*NY WORDS
C                          LONG.
C
C                        MX
C                          FIRST DIMENSION OF Z.
C
C                        NX
C                          NUMBER OF POINTS IN THE X DIRECTION
C                          IN Z.  WHEN PLOTTING AN ENTIRE ARRAY,
C                          MX=NX.  SEE APPENDIX 1 OF THE GRAPHICS
C                          CHAPTER FOR AN EXPLANATION OF USING THIS
C                          ARGUMENT LIST TO PROCESS ANY PART OF AN
C                          ARRAY.
C
C                        NY
C                          NUMBER OF POINTS IN THE Y DIRECTION IN Z.
C
C                        S
C                          S DEFINES THE LINE OF SIGHT.  THE VIEWER'S
C                          EYE IS AT (S(1), S(2), S(3)) AND THE
C                          POINT LOOKED AT IS AT (S(4), S(5), S(6)).
C                          THE EYE SHOULD BE OUTSIDE THE BLOCK WITH
C                          OPPOSITE CORNERS (X(1), Y(1), ZMIN) AND
C                          (X(NX), Y(NY), ZMAX) AND THE POINT LOOKED
C                          AT SHOULD BE INSIDE IT.      FOR     A NICE
C                          PERSPECTIVE EFFECT, THE DISTANCE BETWEEN
C                          THE EYE AND THE POINT LOOKED AT SHOULD BE
C                          5 TO 10 TIMES THE SIZE OF THE BLOCK.  SEE
C                          NOTE, BELOW.
C
C                        STEREO
C                          FLAG TO INDICATE IF STEREO PAIRS ARE TO
C                          BE DRAWN.  0.0 MEANS NO STEREO PAIR (ONE
C                          PICTURE).  NON-ZERO MEANS PUT OUT TWO
C                          PICTURES.  THE VALUE OF STEREO IS THE
C                          RELATIVE ANGLE BETWEEN THE EYES.  A VALUE
C                          OF 1.0 PRODUCES STANDARD SEPARATION.
C                          NEGATIVE STEREO REVERSES THE LEFT AND
C                          RIGHT FIGURES.
C
C ON OUTPUT              X, Y, Z, MX, NX, NY, S, STEREO ARE
C FOR SRFACE             UNCHANGED.  M HAS BEEN WRITTEN IN.
C
C NOTES                  . THE RANGE OF Z COMPARED WITH THE RANGE
C                          OF X AND Y DETERMINES THE SHAPE OF THE
C                          PICTURE.  THEY ARE ASSUMED TO BE IN THE
C                          SAME UNITS AND NOT WILDLY DIFFERENT IN
C                          MAGNITUDE.  S IS ASSUMED TO BE IN THE
C                          SAME UNITS AS X, Y, AND Z.
C                        . PICTURE SIZE CAN BE MADE RELATIVE TO
C                          DISTANCE.  SEE COMMENTS IN SETR.
C                        . TRN32S CAN BE USED TO TRANSLATE FROM 3
C                          SPACE TO 2 SPACE.  SEE COMMENTS THERE.
C                        . DATA WITH EXTREME DISCONTINUITIES MAY
C                          CAUSE VISIBILITY ERRORS.  IF THIS PROBLEM
C                          OCCURS, USE A DISTANT EYE POSITION
C                          AWAY FROM THE +Z AXIS.
C                        . THE DEFAULT LINE COLOR IS SET TO
C                          COLOR INDEX 1.  IF THE USER WISHES TO
C                          CHANGE THE LINE COLOR, HE CAN DO SO BY
C                          DEFINING COLOR INDEX 1 BEFORE CALLING
C                          SRFACE, OR BY PUTTING THE COMMON BLOCK
C                          SRFINT IN HIS CALLING PROGRAM AND
C                          DEFINING AND USING COLOR INDEX ISRFMJ
C                          (DEFAULTED TO 1 IN BLOCKDATA.)
C
C ENTRY POINTS           SRFACE, SRFGK, EZSRFC, SETR, DRAWS, TRN32S,
C                        CLSET, CTCELL, SRFABD
C
C COMMON BLOCKS          PWRZ1S, SRFBLK, SRFINT, SRFIP1
C
C I/O                    PLOTS
C
C PRECISION              SINGLE
C
C LANGUAGE               FORTRAN
C
C HISTORY                CONVERTED TO FORTRAN 77 AND GKS IN MARCH 1984.
C
C                        PREPARED FOR SIGGRAPH, AUGUST 1976.
C
C                        STANDARDIZED IN JANUARY 1973.
C
C                        WRITTEN IN DECEMBER 1971.      REPLACED K.S.+G.
C                        ALGORITHM CALLED SOLIDS AT NCAR.
C
C
C ALGORITHM              HIGHEST SO FAR IS VISIBLE FROM ABOVE.  (SEE
C                        REFERENCE.)
C
C REFERENCE              WRIGHT, T.J., A TWO SPACE SOLUTION TO THE
C                        HIDDEN LINE PROBLEM FOR PLOTTING A FUNCTION
C                        OF TWO VARIABLES.      IEEE TRANS.     COMP.,
C                        PP 28-33, JANUARY 1973.
C
C ACCURACY               IF THE ENDS OF A LINE SEGMENT ARE VISIBLE,
C                        THE MIDDLE IS ASSUMED VISIBLE.
C
C TIMING                 PROPORTIONAL TO NX*NY.
C
C
C INTERNAL PARAMETERS    NAME   DEFAULT  FUNCTION
C                        ----   -------  --------
C                        IFR        1    -1  CALL FRAME FIRST.
C                                         0  DO NOT CALL FRAME.
C                                        +1  CALL FRAME WHEN DONE.
c +NOAO:  The value of ifr has been changed from its default of +1 to 0.
c -NOAO
C
C                        ISTP       0    STEREO TYPE IF STEREO
C                                        NON-ZERO.
C                                        -1  ALTERNATING FRAMES,
C                                                  SLIGHTLY OFFSET (FOR
C                                                  MOVIES.  IROTS = 0).
C                                               0  BLANK FRAME BETWEEN
C                                                  FOR STEREO   SLIDE.
C                                                  IROTS = 1).
C                                        +1  BOTH ON SAME FRAME.
C                                                  (LEFT PICTURE TO LEFT
C                                                  SIDE.  IROTS = 0).
C
C                        IROTS      0     0  +Z IN VERTICAL PLOTTING
C                                                  DIRECTION (CINE MODE).
C                                        +1  +Z IN HORIZONTAL
C                                                  PLOTTING DIRECTION
C                                                  (COMIC MODE).
C
C                        IDRX       1    +1  DRAW LINES OF CONSTANT
C                                                  X.
C                                               0  DO   NOT.
C
C                        IDRY       1    +1  DRAW LINES OF CONSTANT
C                                                  Y.
C                                               0  DO   NOT.
C
C                        IDRZ       0    +1  DRAW LINES OF CONSTANT
C                                                  Z (CONTOUR   LINES).
C                                               0  DO   NOT.
C
C                        IUPPER         0        +1  DRAW UPPER SIDE OF
C                                                  SURFACE.
C                                        0  DRAW BOTH SIDES.
C                                        -1  DRAW LOWER SIDE.
C
C                        ISKIRT         0        +1  DRAW A SKIRT AROUND THE
C                                                  SURFACE.
C                                                  BOTTOM = HSKIRT.
C                                               0  DO   NOT.
C
C                        NCLA       6    APPROXIMATE NUMBER OF
C                                        LEVELS OF CONSTANT Z THAT
C                                        ARE DRAWN IF LEVELS ARE NOT
C                                        SPECIFIED.  40 LEVELS
C                                        MAXIMUM.
C
C                        THETA    .02    ANGLE IN RADIANS BETWEEN
C                                        EYES FOR STEREO PAIRS.
C
C                        HSKIRT    0.    HEIGHT OF SKIRT
C                                                  (IF ISKIRT   = 1).
C
C                        CHI       0.    HIGHEST LEVEL OF CONSTANT
C                                        Z.
C
C                        CLO       0.    LOWEST LEVEL OF CONSTANT Z.
C
C                        CINC      0.    INCREMENT BETWEEN LEVELS.
C
C                          [IF CHI, CLO, OR CINC IS ZERO, A NICE
C                               VALUE IS GENERATED AUTOMATICALLY.]
C
C                        IOFFP     0     FLAG TO CONTROL USE OF SPECIAL
C                                        VALUE FEATURE.  DO NOT HAVE
C                                        BOTH IOFFP=1 AND ISKIRT=1.
C                                               0 FEATURE NOT   IN USE
C                                        +1  FEATURE IN USE.  NO LINES
C                                                  DRAWN TO DATA POINTS IN Z
C                                                  THAT ARE EQUAL TO SPVAL.
C
C                        SPVAL     0.    SPECIAL VALUE USED TO MARK UN-
C                                        KNOWN DATA WHEN IOFFP=1.
C
C
C
      DIMENSION       X(NX)      ,Y(NY)    ,Z(MX,NY), M(2,NX,NY),
     1                S(6)
      DIMENSION       WIN1(4)    ,VP1(4)           ,LASF(13)
      COMMON /SRFINT/ ISRFMJ     ,ISRFMN          ,ISRFTX
c +NOAO: common block added 4NOV85 to allow user control of viewport
      common /noaovp/ vpx1, vpx2, vpy1, vpy2
c -NOAO
c +NOAO: Blockdata srfabd rewritten as run time initialization
c     EXTERNAL        SRFABD
      call srfabd
c -NOAO
      CALL Q8QST4 ('GRAPHX','SRFACE','SRFACE','VERSION 01')
C
C     THIS DRIVER SAVES THE CURRENT NORMALIZATION TRANSFORMATION
C     INFORMATION, DEFINES THE NORMALIZATION TRANSFORMATION
C     APPROPRIATE FOR SRFGK, CALLS SRFGK, AND RESTORES THE ORIGINAL
C     NORMALIZATION TRANSFORMATION.
C
C     GET CURRENT NORMALIZATION TRANSFORMATION NUMBER
C
      CALL GQCNTN (IER,NTORIG)
C
C     STORE WINDOW AND VIEWPORT OF NORMALIZATION TRANSFORMATION 1
C
      CALL GQNT (NTORIG,IER,WIN1,VP1)
      CALL GETUSV('LS',IOLLS)
C
C     SET WINDOW AND VIEWPORT FOR SRFGK
C
c     CALL SET(0.,1.,0.,1.,1.,1024.,1.,1024.,1)
c +NOAO: viewport limits now stored in common block noaovp
      CALL SET(vpx1, vpx2, vpy1, vpy2, 1.0, 1024., 1.0, 1024., 1)
c -NOAO
C
C     SET LINE COLOR TO INDIVIDUAL (SAVE CURRENT SETTING)
C
      CALL GQASF (IER,LASF)
      LASFSV  = LASF(3)
      LASF(3) = 1
      CALL GSASF(LASF)
C
C     SET LINE COLOR INDEX TO COMMON VARIABLE ISRFMJ (SAVE
C     CURRENT SETTING)
C
      CALL GQPLCI (IER,LCISV)
      CALL GSPLCI (ISRFMJ)
C
C     DRAW PLOT
C
      CALL SRFGK (X,Y,Z,M,MX,NX,NY,S,STEREO)
C
C     RESTORE INITIAL LINE COLOR SETTINGS
C
      LASF(3) = LASFSV
      CALL GSASF(LASF)
      CALL GSPLCI (LCISV)
C
C     RESTORE ORIGINAL NORMALIZATION TRANSFORMATION
C
      CALL SET(VP1(1),VP1(2),VP1(3),VP1(4),WIN1(1),WIN1(2),
     -         WIN1(3),WIN1(4),IOLLS)
      CALL GSELNT (NTORIG)
C
      RETURN
      END
      SUBROUTINE SRFGK (X,Y,Z,M,MX,NX,NY,S,STEREO)
C
      DIMENSION       X(NX)      ,Y(NY)   ,Z(MX,NY)  ,M(2,NX,NY) ,
     1                S(6)
      DIMENSION       MXS(2)     ,MXF(2) ,MXJ(2) ,MYS(2),
     1                MYF(2)     ,MYJ(2)
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL,
     1                LL         ,FACT       ,IROT       ,NDRZ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO,
     4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
      COMMON /PWRZ1S/ XXMIN      ,XXMAX      ,YYMIN      ,YYMAX,
     1                ZZMIN      ,ZZMAX      ,DELCRT     ,EYEX,
     2                EYEY       ,EYEZ
      COMMON /SRFIP1/ IFR        ,ISTP       ,IROTS      ,IDRX ,
     1                IDRY       ,IDRZ       ,IUPPER     ,ISKIRT,
     2                NCLA       ,THETA      ,HSKIRT     ,CHI,
     3                CLO        ,CINC       ,ISPVAL
c +NOAO:
      common /noaovp/ vpx1, vpx2, vpy1, vpy2
c -NOAO
C
      DATA JF, IF, LY, LX, ICNST /1, 1, 2, 2, 0/
      CALL Q8QST4 ('GRAPHX','SRFACE','SRFGK','VERSION 01')
      BIGEST = R1MACH(2)
      MMXX = MX
      NNXX = NX
      NNYY = NY
      STER = STEREO
      NXP1 = NNXX+1
      NYP1 = NNYY+1
      NLA = NCLA
      NSPVAL = ISPVAL
      NDRZ = IDRZ
      IF (IDRZ .NE. 0)
     1    CALL CLSET (Z,MMXX,NNXX,NNYY,CHI,CLO,CINC,NLA,40,CL,NCL,
     2                ICNST,IOFFP,SPVAL,BIGEST)
      IF (IDRZ .NE. 0) NDRZ = 1-ICNST
      STHETA = SIN(STER*THETA)
      CTHETA = COS(STER*THETA)
      RX = S(1)-S(4)
      RY = S(2)-S(5)
      RZ = S(3)-S(6)
      D1 = SQRT(RX*RX+RY*RY+RZ*RZ)
      D2 = SQRT(RX*RX+RY*RY)
      DX = 0.
      DY = 0.
      IF (STEREO .EQ. 0.) GO TO  20
      D1 = D1*STEREO*THETA
      IF (D2 .GT. 0.) GO TO  10
      DX = D1
      GO TO  20
   10 AGL = ATAN2(RX,-RY)
      DX = D1*COS(AGL)
      DY = D1*SIN(AGL)
   20 IROT = IROTS
      NPIC = 1
      IF (STER .NE. 0.) NPIC = 2
      FACT = 1.
      IF (NRSWT .NE. 0) FACT = RZERO/D1
      IF (ISTP.EQ.0 .AND. STER.NE.0.) IROT = 1
      DO 570 IPIC=1,NPIC
         NUPPER = IUPPER
         IF (IFR .LT. 0) CALL FRAME
C
C SET UP MAPING FROM FLOATING POINT 3-SPACE TO CRT SPACE.
C
         SIGN1 = IPIC*2-3
         EYEX = S(1)+SIGN1*DX
         POIX = S(4)+SIGN1*DX
         EYEY = S(2)+SIGN1*DY
         POIY = S(5)+SIGN1*DY
         EYEZ = S(3)
         POIZ = S(6)
         LL = 0
         XEYE = EYEX
         YEYE = EYEY
         ZEYE = EYEZ
         CALL TRN32S (POIX,POIY,POIZ,XEYE,YEYE,ZEYE,0)
         LL = IPIC+2*ISTP+3
         IF (STER .EQ. 0.) LL = 1
         IF (NRSWT .NE. 0) GO TO 100
         XXMIN = X(1)
         XXMAX = X(NNXX)
         YYMIN = Y(1)
         YYMAX = Y(NNYY)
         UMIN = BIGEST
         VMIN = BIGEST
         ZZMIN = BIGEST
         UMAX = -UMIN
         VMAX = -VMIN
         ZZMAX = -ZZMIN
         DO  40 J=1,NNYY
            DO  30 I=1,NNXX
                ZZ = Z(I,J)
                IF (IOFFP.EQ.1 .AND. ZZ.EQ.SPVAL) GO TO 30
                ZZMAX = AMAX1(ZZMAX,ZZ)
                ZZMIN = AMIN1(ZZMIN,ZZ)
                CALL TRN32S (X(I),Y(J),Z(I,J),UT,VT,DUMMY,1)
                UMAX = AMAX1(UMAX,UT)
                UMIN = AMIN1(UMIN,UT)
                VMAX = AMAX1(VMAX,VT)
                VMIN = AMIN1(VMIN,VT)
   30             CONTINUE
   40    CONTINUE
         IF (ISKIRT .NE. 1) GO TO  70
         NXSTP = NNXX-1
         NYSTP = NNYY-1
         DO  60 J=1,NNYY,NYSTP
            DO  50 I=1,NNXX,NXSTP
                CALL TRN32S (X(I),Y(J),HSKIRT,UT,VT,DUMMY,1)
                UMAX = AMAX1(UMAX,UT)
                UMIN = AMIN1(UMIN,UT)
                VMAX = AMAX1(VMAX,VT)
                VMIN = AMIN1(VMIN,VT)
   50             CONTINUE
   60    CONTINUE
   70    CONTINUE
         WIDTH = UMAX-UMIN
         HIGHT = VMAX-VMIN
         DIF = .5*(WIDTH-HIGHT)
         IF (DIF)  80,100, 90
   80    UMIN = UMIN+DIF
         UMAX = UMAX-DIF
         GO TO 100
   90    VMIN = VMIN-DIF
         VMAX = VMAX+DIF
  100    XEYE = EYEX
         YEYE = EYEY
         ZEYE = EYEZ
         CALL TRN32S (POIX,POIY,POIZ,XEYE,YEYE,ZEYE,0)
         DO 120 J=1,NNYY
            DO 110 I=1,NNXX
                CALL TRN32S (X(I),Y(J),Z(I,J),UT,VT,DUMMY,1)
                M(1,I,J)        = UT
                M(2,I,J)        = VT
  110             CONTINUE
  120    CONTINUE
C
C INITIALIZE UPPER AND LOWER VISIBILITY ARRAYS
C
         DO 130 K=1,1024
            LIMU(K) = 0
            LIML(K) = 1024
  130    CONTINUE
C
C FIND ORDER TO DRAW LINES
C
         NXPASS = 1
         IF (S(1) .GE. X(NNXX)) GO TO 160
         IF (S(1) .LE. X(1)) GO TO 170
         DO 140 I=2,NNXX
            LX = I
            IF (S(1) .LE. X(I)) GO TO 150
  140    CONTINUE
  150    MXS(1) = LX-1
         MXJ(1) = -1
         MXF(1) = 1
         MXS(2) = LX
         MXJ(2) = 1
         MXF(2) = NNXX
         NXPASS = 2
         GO TO 180
  160    MXS(1) = NNXX
         MXJ(1) = -1
         MXF(1) = 1
         GO TO 180
  170    MXS(1) = 1
         MXJ(1) = 1
         MXF(1) = NNXX
  180    NYPASS = 1
         IF (S(2) .GE. Y(NNYY)) GO TO 210
         IF (S(2) .LE. Y(1)) GO TO 220
         DO 190 J=2,NNYY
            LY = J
            IF (S(2) .LE. Y(J)) GO TO 200
  190    CONTINUE
  200    MYS(1) = LY-1
         MYJ(1) = -1
         MYF(1) = 1
         MYS(2) = LY
         MYJ(2) = 1
         MYF(2) = NNYY
         NYPASS = 2
         GO TO 230
  210    MYS(1) = NNYY
         MYJ(1) = -1
         MYF(1) = 1
         GO TO 230
  220    MYS(1) = 1
         MYJ(1) = 1
         MYF(1) = NNYY
C
C PUT ON SKIRT ON FRONT SIDE IF WANTED
C
  230    IF (NXPASS.EQ.2 .AND. NYPASS.EQ.2) GO TO 490
         IF (ISKIRT .EQ. 0) GO TO 290
         IN = MXS(1)
         IF = MXF(1)
         JN = MYS(1)
         JF = MYF(1)
         IF (NYPASS .NE. 1) GO TO 260
         CALL TRN32S (X(1),Y(JN),HSKIRT,UX1,VX1,DUMMY,1)
         CALL TRN32S (X(NNXX),Y(JN),HSKIRT,UX2,VX2,DUMMY,1)
         QU = (UX2-UX1)/(X(NNXX)-X(1))
         QV = (VX2-VX1)/(X(NNXX)-X(1))
         YNOW = Y(JN)
         DO 240 I=1,NNXX
            CALL TRN32S (X(I),YNOW,HSKIRT,RU,RV,DUMMY,1)
            CALL DRAWS (IFIX(RU),IFIX(RV),M(1,I,JN),M(2,I,JN),1,0)
  240    CONTINUE
         CALL DRAWS (IFIX(UX1),IFIX(VX1),IFIX(UX2),IFIX(VX2),1,1)
         IF (IDRY .NE. 0) GO TO 260
         DO 250 I=2,NNXX
            CALL DRAWS (M(1,I-1,JN),M(2,I-1,JN),M(1,I,JN),M(2,I,JN),1,1)
  250    CONTINUE
  260    IF (NXPASS .NE. 1) GO TO 290
         CALL TRN32S (X(IN),Y(1),HSKIRT,UY1,VY1,DUMMY,1)
         CALL TRN32S (X(IN),Y(NNYY),HSKIRT,UY2,VY2,DUMMY,1)
         QU = (UY2-UY1)/(Y(NNYY)-Y(1))
         QV = (VY2-VY1)/(Y(NNYY)-Y(1))
         XNOW = X(IN)
         DO 270 J=1,NNYY
            CALL TRN32S (XNOW,Y(J),HSKIRT,RU,RV,DUMMY,1)
            CALL DRAWS (IFIX(RU),IFIX(RV),M(1,IN,J),M(2,IN,J),1,0)
  270    CONTINUE
         CALL DRAWS (IFIX(UY1),IFIX(VY1),IFIX(UY2),IFIX(VY2),1,1)
         IF (IDRX .NE. 0) GO TO 290
         DO 280 J=2,NNYY
            CALL DRAWS (M(1,IN,J-1),M(2,IN,J-1),M(1,IN,J),M(2,IN,J),1,1)
  280    CONTINUE
C
C PICK PROPER ALGORITHM
C
  290    LI = MXJ(1)
         MI = MXS(1)-LI
         NI = IABS(MI-MXF(1))
         LJ = MYJ(1)
         MJ = MYS(1)-LJ
         NJ = IABS(MJ-MYF(1))
C
C WHEN LINE OF SIGHT IS NEARER TO PARALLEL TO THE X AXIS,
C HAVE J LOOP OUTER-MOST, OTHERWISE HAVE I LOOP OUTER-MOST.
C
         IF (ABS(RX) .LE. ABS(RY)) GO TO 360
         IF (ISKIRT.NE.0 .OR. NYPASS.NE.1) GO TO 310
         I = MXS(1)
         DO 300 J=2,NNYY
            CALL DRAWS (M(1,I,J-1),M(2,I,J-1),M(1,I,J),M(2,I,J),0,1)
  300    CONTINUE
  310    DO 350 II=1,NNXX
            I = MI+II*LI
            IPLI = I+LI
            IF (NYPASS .EQ. 1) GO TO 320
            K = MYS(1)
            L = MYS(2)
            IF (IDRX .NE. 0)
     1          CALL DRAWS (M(1,I,K),M(2,I,K),M(1,I,L),M(2,I,L),1,1)
            IF (NDRZ.NE.0 .AND. II.NE.NI)
     1          CALL CTCELL (Z,MMXX,NNXX,NNYY,M,MIN0(I,I+LI),K)
  320             DO 340 JPASS=1,NYPASS
                LJ = MYJ(JPASS)
                MJ = MYS(JPASS)-LJ
                NJ = IABS(MJ-MYF(JPASS))
                DO 330 JJ=1,NJ
                  J = MJ+JJ*LJ
                  JPLJ = J+LJ
                  IF (IDRX.NE.0 .AND. JJ.NE.NJ)
     1                CALL DRAWS (M(1,I,J),M(2,I,J),M(1,I,JPLJ),
     2                            M(2,I,JPLJ),1,1)
                  IF (I.NE.MXF(1) .AND. IDRY.NE.0)
     1                CALL DRAWS (M(1,IPLI,J),M(2,IPLI,J),M(1,I,J),
     2                            M(2,I,J),1,1)
                  IF (NDRZ.NE.0 .AND. JJ.NE.NJ .AND. II.NE.NNXX)
     1                CALL CTCELL (Z,MMXX,NNXX,NNYY,M,MIN0(I,I+LI),
     2                             MIN0(J,J+LJ))
  330                CONTINUE
  340             CONTINUE
  350    CONTINUE
         GO TO 430
  360    IF (ISKIRT.NE.0 .OR. NXPASS.NE.1) GO TO 380
         J = MYS(1)
         DO 370 I=2,NNXX
            CALL DRAWS (M(1,I-1,J),M(2,I-1,J),M(1,I,J),M(2,I,J),0,1)
  370    CONTINUE
  380    DO 420 JJ=1,NNYY
            J = MJ+JJ*LJ
            JPLJ = J+LJ
            IF (NXPASS .EQ. 1) GO TO 390
            K = MXS(1)
            L = MXS(2)
            IF (IDRY .NE. 0)
     1          CALL DRAWS (M(1,K,J),M(2,K,J),M(1,L,J),M(2,L,J),1,1)
            IF (NDRZ.NE.0 .AND. JJ.NE.NJ)
     1          CALL CTCELL (Z,MMXX,NNXX,NNYY,M,K,MIN0(J,J+LJ))
  390             DO 410 IPASS=1,NXPASS
                LI = MXJ(IPASS)
                MI = MXS(IPASS)-LI
                NI = IABS(MI-MXF(IPASS))
                DO 400 II=1,NI
                  I = MI+II*LI
                  IPLI = I+LI
                  IF (IDRY.NE.0 .AND. II.NE.NI)
     1                CALL DRAWS (M(1,I,J),M(2,I,J),M(1,IPLI,J),
     2                            M(2,IPLI,J),1,1)
                  IF (J.NE.MYF(1) .AND. IDRX.NE.0)
     1                CALL DRAWS (M(1,I,JPLJ),M(2,I,JPLJ),M(1,I,J),
     2                            M(2,I,J),1,1)
                  IF (NDRZ.NE.0 .AND. II.NE.NI .AND. JJ.NE.NNYY)
     1                CALL CTCELL (Z,MMXX,NNXX,NNYY,M,MIN0(I,I+LI),
     2                             MIN0(J,J+LJ))
  400                CONTINUE
  410             CONTINUE
  420    CONTINUE
  430    IF (ISKIRT .EQ. 0) GO TO 520
C
C FIX UP IF SKIRT IS USED WITH LINES ONE WAY.
C
         IF (IDRX .NE. 0) GO TO 460
         DO 450 IPASS=1,NXPASS
            IF (NXPASS .EQ. 2) IF = 1+(IPASS-1)*(NNXX-1)
            DO 440 J=2,NNYY
                CALL DRAWS (M(1,IF,J-1),M(2,IF,J-1),M(1,IF,J),M(2,IF,J),
     1                     1,0)
  440             CONTINUE
  450    CONTINUE
  460    IF (IDRY .NE. 0) GO TO 520
         DO 480 JPASS=1,NYPASS
            IF (NYPASS .EQ. 2) JF = 1+(JPASS-1)*(NNYY-1)
            DO 470 I=2,NNXX
                CALL DRAWS (M(1,I-1,JF),M(2,I-1,JF),M(1,I,JF),M(2,I,JF),
     1                     1,0)
  470             CONTINUE
  480    CONTINUE
         GO TO 520
C
C ALL VISIBLE IF VIEWED FROM DIRECTLY ABOVE OR BELOW.
C
  490    IF (NUPPER.GT.0 .AND. S(3).LT.S(6)) GO TO 520
         IF (NUPPER.LT.0 .AND. S(3).GT.S(6)) GO TO 520
         NUPPER = 1
         IF (S(3) .LT. S(6)) NUPPER = -1
         DO 510 I=1,NNXX
            DO 500 J=1,NNYY
                IF (IDRX.NE.0 .AND. J.NE.NNYY)
     1             CALL DRAWS (M(1,I,J),M(2,I,J),M(1,I,J+1),M(2,I,J+1),
     2                          1,0)
                IF (IDRY.NE.0 .AND. I.NE.NNXX)
     1             CALL DRAWS (M(1,I,J),M(2,I,J),M(1,I+1,J),M(2,I+1,J),
     2                          1,0)
                IF (IDRZ.NE.0 .AND. I.NE.NNXX .AND. J.NE.NNYY)
     1             CALL CTCELL (Z,MMXX,NNXX,NNYY,M,I,J)
  500             CONTINUE
  510    CONTINUE
  520    IF (STER .EQ. 0.) GO TO 560
         IF (ISTP) 540,530,550
  530    CALL FRAME
  540    CALL FRAME
         GO TO 570
  550    IF (IPIC .NE. 2) GO TO 570
  560    IF (IFR .GT. 0) CALL FRAME
  570 CONTINUE
      RETURN
      END
      SUBROUTINE EZSRFC (Z,M,N,ANGH,ANGV,WORK)
      DIMENSION       Z(M,N)     ,WORK(1)
C
C                         WORK(2*M*N+M+N)
C
C PERSPECTIVE PICTURE OF A SURFACE STORED IN A TWO DIMENSIONAL ARRAY
C VIA A VERY SHORT ARGUMENT LIST.
C
C ASSUMPTIONS--
C     THE ENTIRE ARRAY IS TO BE DRAWN,
C     THE DATA IS EQUALLY SPACED (IN THE X-Y PLANE),
C     NO STEREO PAIRS.
C IF THESE ASSUMPTIONS ARE NOT MET USE SRFACE.
C
C ARGUMENTS--
C     Z    THE 2 DIMENSIONAL ARRAY TO BE DRAWN.
C     M    THE FIRST DIMENSION OF Z.
C     N    THE SECOND DIMENSION OF Z.
C     ANGH ANGLE IN DEGREES IN THE X-Y PLANE TO THE LINE OF SIGHT
C          (COUNTER-CLOCK WISE FROM THE PLUS-X AXIS).
C     ANGV ANGLE IN DEGREES FROM THE X-Y PLANE TO THE LINE OF SIGHT
C          (POSITIVE ANGLES ARE ABOVE THE MIDDLE Z, NEGATIVE BELOW).
C     WORK A SCRATCH STORAGE DIMENSIONED AT LEAST 2*M*N+M+N.
C
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL,
     1                LL         ,FACT       ,IROT       ,NDRZ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO,
     4                NOFFP      ,NSPVAL     ,SPV        ,BIGEST
      DIMENSION       S(6)
      DATA S(4),S(5),S(6)/0.0,0.0,0.0/
C
C  FACT1 IS THE PERSPECTIVE RATIO AND IS DEFINED TO BE THE RATIO
C         MAXIMUM(LENGTH,WIDTH)/HEIGHT
C
C  FACT2 IS THE RATIO   (LENGTH OF LINE OF SIGHT)/MAXIMUM(LENGTH,WIDTH)
C
      DATA FACT1,FACT2/2.0,5.0/
      BIGEST = R1MACH(2)
C
C FIND RANGE OF Z
C
      MX = M
      NY = N
      ANG1 = ANGH*3.14159265358979/180.
      ANG2 = ANGV*3.14159265358979/180.
      FLO = BIGEST
      HI = -FLO
      DO  20 J=1,NY
         DO  10 I=1,MX
            IF (NOFFP.EQ.1 .AND. Z(I,J).EQ.SPV) GO TO   10
            HI = AMAX1(Z(I,J),HI)
            FLO = AMIN1(Z(I,J),FLO)
   10    CONTINUE
   20 CONTINUE
C
C SET UP LINEAR X AND Y ARRAYS FOR SRFACE
C
      DELTA = (HI-FLO)/(AMAX0(MX,NY)-1.)*FACT1
      XMIN = -(FLOAT(MX/2)*DELTA+FLOAT(MOD(MX+1,2))*DELTA)
      YMIN = -(FLOAT(NY/2)*DELTA+FLOAT(MOD(NY+1,2))*DELTA)
      DO  30 I=1,MX
         WORK(I) = XMIN+FLOAT(I-1)*DELTA
   30 CONTINUE
      DO  40 J=1,NY
         K = MX+J
         WORK(K) = YMIN+FLOAT(J-1)*DELTA
   40 CONTINUE
C
C SET UP EYE POSITION
C
      FACTE = (HI-FLO)*FACT1*FACT2
      CANG2 = COS(ANG2)
      S(1) = FACTE*CANG2*COS(ANG1)
      S(2) = FACTE*CANG2*SIN(ANG1)
      S(3) = FACTE*SIN(ANG2)+(FLO+HI)*.5
C
C READY
C
      CALL SRFACE (WORK(1),WORK(MX+1),Z,WORK(K+1),MX,MX,NY,S,0.)
      RETURN
      END
      SUBROUTINE SETR (XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,R0)
C
C THIS ROUTINE ESTABLISHES CERTAIN CONSTANTS SO THAT SRFACE
C PRODUCES A PICTURE WHOSE SIZE CHANGES WITH RESPECT TO THE
C VIEWERS DISTANCE FROM THE OBJECT.  IT CAN ALSO BE USED
C WHEN MAKING A MOVIE OF AN OBJECT EVOLVING IN TIME TO KEEP
C IT POSITIONED PROPERLY ON THE SCREEN, SAVING COMPUTER TIME
C IN THE BARGIN.  CALL IT WITH R0 NEGATIVE TO TURN OFF THIS
C FEATURE.
C PARAMETERS
C XMIN,XMAX - RANGE OF X ARRAY THAT WILL BE PASSED TO SRFACE.
C YMIN,YMAX - SAME IDEA, BUT FOR Y.
C ZMIN,ZMAX - SAME IDEA, BUT FOR Z.  IF A MOVIE IS BEING
C               MADE OF AN EVOLVING Z ARRAY, ZMIN       AND ZMAX
C               SHOULD CONTAIN RANGE OF THE UNION       OF ALL THE Z
C               ARRAYS.  THEY NEED NOT BE       EXACT.
C R0             - DISTANCE BETWEEN OBSERVER    AND POINT LOOKED AT
C               WHEN THE PICTURE IS TO FILL THE SCREEN WHEN
C               VIEWED FROM THE DIRECTION       WHICH MAKES THE PIC-
C               TURE BIGGEST.  IF       R0 IS NOT POSITIVE, THEN THE
C               RELATIVE SIZE FEATURE IS TURNED OFF, AND SUB-
C               SEQUENT PICTURES WILL FILL THE SCREEN.
C
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL,
     1                LL         ,FACT       ,IROT       ,NDRZ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO,
     4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
      COMMON /PWRZ1S/ XXMIN      ,XXMAX      ,YYMIN      ,YYMAX,
     1                ZZMIN      ,ZZMAX      ,DELCRT     ,EYEX,
     2                EYEY       ,EYEZ
C
C
      CALL Q8QST4 ('GRAPHX','SRFACE','SETR','VERSION 01')
      IF (R0)  10, 10, 20
   10 NRSWT = 0
      RETURN
   20 NRSWT = 1
      XXMIN = XMIN
      XXMAX = XMAX
      YYMIN = YMIN
      YYMAX = YMAX
      ZZMIN = ZMIN
      ZZMAX = ZMAX
      RZERO = R0
      LL = 0
      XAT = (XXMAX+XXMIN)*.5
      YAT = (YYMAX+YYMIN)*.5
      ZAT = (ZZMAX+ZZMIN)*.5
      ALPHA = -(YYMIN-YAT)/(XXMIN-XAT)
      YEYE = -RZERO/SQRT(1.+ALPHA*ALPHA)
      XEYE = YEYE*ALPHA
      YEYE = YEYE+YAT
      XEYE = XEYE+XAT
      ZEYE = ZAT
      CALL TRN32S (XAT,YAT,ZAT,XEYE,YEYE,ZEYE,0)
      XMN = XXMIN
      XMX = XXMAX
      YMN = YYMIN
      YMX = YYMAX
      ZMN = ZZMIN
      ZMX = ZZMAX
      CALL TRN32S (XMN,YMN,ZAT,UMN,DUMMY,DUMMIE,1)
      CALL TRN32S (XMX,YMN,ZMN,DUMMY,VMN,DUMMIE,1)
      CALL TRN32S (XMX,YMX,ZAT,UMX,DUMMY,DUMMIE,1)
      CALL TRN32S (XMX,YMN,ZMX,DUMMY,VMX,DUMMIE,1)
      UMIN = UMN
      UMAX = UMX
      VMIN = VMN
      VMAX = VMX
      BIGD = SQRT((XXMAX-XXMIN)**2+(YYMAX-YYMIN)**2+(ZZMAX-ZZMIN)**2)*.5
      RETURN
      END
      SUBROUTINE DRAWS (MX1,MY1,MX2,MY2,IDRAW,IMARK)
C
C THIS ROUTINE DRAWS THE VISIBLE PART OF THE LINE CONNECTING
C (MX1,MY1) AND (MX2,MY2).  IF IDRAW .NE. 0, THE LINE IS DRAWN.
C IF IMARK .NE. 0, THE VISIBILITY ARRAY IS MARKED.
C
      LOGICAL               VIS1         ,VIS2
      DIMENSION       PXS(2)     ,PYS(2)
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL,
     1                LL         ,FACT       ,IROT       ,NDRZ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO,
     4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
      DATA STEEP/5./
      DATA MX, MY /0, 0/
C
c +NOAO: Blockdata srfabd rewritten as run time initialization
c     EXTERNAL        SRFABD
      call srfabd
c -NOAO
C MAKE LINE LEFT TO RIGHT.
C
      MMX1 = MX1
      MMY1 = MY1
      MMX2 = MX2
      MMY2 = MY2
      IF (MMX1.EQ.NSPVAL .OR. MMX2.EQ.NSPVAL) RETURN
      IF (MMX1 .GT. MMX2) GO TO  10
      NX1 = MMX1
      NY1 = MMY1
      NX2 = MMX2
      NY2 = MMY2
      GO TO  20
   10 NX1 = MMX2
      NY1 = MMY2
      NX2 = MMX1
      NY2 = MMY1
   20 IF (NUPPER .LT. 0) GO TO 180
C
C CHECK UPPER VISIBILITY.
C
      VIS1 = NY1 .GE. (LIMU(NX1)-1)
      VIS2 = NY2 .GE. (LIMU(NX2)-1)
C
C VIS1 AND VIS2 TRUE MEANS VISIBLE.
C
      IF (VIS1 .AND. VIS2) GO TO 120
C
C VIS1 AND VIS2 FALSE MEANS INVISIBLE.
C
      IF (.NOT.(VIS1 .OR. VIS2)) GO TO 180
C
C FIND CHANGE POINT.
C
      IF (NX1 .EQ. NX2) GO TO 110
      DY = FLOAT(NY2-NY1)/FLOAT(NX2-NX1)
      NX1P1 = NX1+1
      FNY1 = NY1
      IF (VIS1) GO TO  60
      DO  30 K=NX1P1,NX2
         MX = K
         MY = FNY1+FLOAT(K-NX1)*DY
         IF (MY .GT. LIMU(K)) GO TO  40
   30 CONTINUE
   40 IF (ABS(DY) .GE. STEEP) GO TO  90
   50 NX1 = MX
      NY1 = MY
      GO TO 120
   60 DO  70 K=NX1P1,NX2
         MX = K
         MY = FNY1+FLOAT(K-NX1)*DY
         IF (MY .LT. LIMU(K)) GO TO  80
   70 CONTINUE
   80 IF (ABS(DY) .GE. STEEP) GO TO 100
      NX2 = MX-1
      NY2 = MY
      GO TO 120
   90 IF (LIMU(MX) .EQ. 0) GO TO  50
      NX1 = MX
      NY1 = LIMU(NX1)
      GO TO 120
  100 NX2 = MX-1
      NY2 = LIMU(NX2)
      GO TO 120
  110 IF (VIS1) NY2 = MIN0(LIMU(NX1),LIMU(NX2))
      IF (VIS2) NY1 = MIN0(LIMU(NX1),LIMU(NX2))
  120 IF (IDRAW .EQ. 0) GO TO 150
C
C DRAW VISIBLE PART OF LINE.
C
      IF (IROT) 130,140,130
  130 CONTINUE
      PXS(1) = FLOAT(NY1)
      PXS(2) = FLOAT(NY2)
      PYS(1) = FLOAT(1024-NX1)
      PYS(2) = FLOAT(1024-NX2)
      CALL GPL (2,PXS,PYS)
      GO TO 150
  140 CONTINUE
      PXS(1) = FLOAT(NX1)
      PXS(2) = FLOAT(NX2)
      PYS(1) = FLOAT(NY1)
      PYS(2) = FLOAT(NY2)
      CALL GPL (2,PXS,PYS)
  150 IF (IMARK .EQ. 0) GO TO 180
      IF (NX1 .EQ. NX2) GO TO 170
      DY = FLOAT(NY2-NY1)/FLOAT(NX2-NX1)
      FNY1 = NY1
      DO 160 K=NX1,NX2
         LTEMP = FNY1+FLOAT(K-NX1)*DY
	 IF (LTEMP .GT. LIMU(K)) LIMU(K) = LTEMP
  160 CONTINUE
      GO TO 180
  170 LTEMP = MAX0(NY1,NY2)
      IF (LTEMP .GT. LIMU(NX1)) LIMU(NX1) = LTEMP
  180 IF (NUPPER) 190,190,370
C
C SAME IDEA AS ABOVE, BUT FOR LOWER SIDE.
C
  190 IF (MMX1 .GT. MMX2) GO TO 200
      NX1 = MMX1
      NY1 = MMY1
      NX2 = MMX2
      NY2 = MMY2
      GO TO 210
  200 NX1 = MMX2
      NY1 = MMY2
      NX2 = MMX1
      NY2 = MMY1
  210 VIS1 = NY1 .LE. (LIML(NX1)+1)
      VIS2 = NY2 .LE. (LIML(NX2)+1)
      IF (VIS1 .AND. VIS2) GO TO 310
      IF (.NOT.(VIS1 .OR. VIS2)) GO TO 370
      IF (NX1 .EQ. NX2) GO TO 300
      DY = FLOAT(NY2-NY1)/FLOAT(NX2-NX1)
      NX1P1 = NX1+1
      FNY1 = NY1
      IF (VIS1) GO TO 250
      DO 220 K=NX1P1,NX2
         MX = K
         MY = FNY1+FLOAT(K-NX1)*DY
         IF (MY .LT. LIML(K)) GO TO 230
  220 CONTINUE
  230 IF (ABS(DY) .GE. STEEP) GO TO 280
  240 NX1 = MX
      NY1 = MY
      GO TO 310
  250 DO 260 K=NX1P1,NX2
         MX = K
         MY = FNY1+FLOAT(K-NX1)*DY
         IF (MY .GT. LIML(K)) GO TO 270
  260 CONTINUE
  270 IF (ABS(DY) .GE. STEEP) GO TO 290
      NX2 = MX-1
      NY2 = MY
      GO TO 310
  280 IF (LIML(MX) .EQ. 1024) GO TO 240
      NX1 = MX
      NY1 = LIML(NX1)
      GO TO 310
  290 NX2 = MX-1
      NY2 = LIML(NX2)
      GO TO 310
  300 IF (VIS1) NY2 = MAX0(LIML(NX1),LIML(NX2))
      IF (VIS2) NY1 = MAX0(LIML(NX1),LIML(NX2))
  310 IF (IDRAW .EQ. 0) GO TO 340
      IF (IROT) 320,330,320
  320 CONTINUE
      PXS(1) = FLOAT(NY1)
      PXS(2) = FLOAT(NY2)
      PYS(1) = FLOAT(1024-NX1)
      PYS(2) = FLOAT(1024-NX2)
      CALL GPL (2,PXS,PYS)
      GO TO 340
  330 CONTINUE
      PXS(1) = FLOAT(NX1)
      PXS(2) = FLOAT(NX2)
      PYS(1) = FLOAT(NY1)
      PYS(2) = FLOAT(NY2)
      CALL GPL (2,PXS,PYS)
  340 IF (IMARK .EQ. 0) GO TO 370
      IF (NX1 .EQ. NX2) GO TO 360
      DY = FLOAT(NY2-NY1)/FLOAT(NX2-NX1)
      FNY1 = NY1
      DO 350 K=NX1,NX2
         LTEMP = FNY1+FLOAT(K-NX1)*DY
	 IF (LTEMP .LT. LIML(K)) LIML(K) = LTEMP
  350 CONTINUE
      RETURN
  360 LTEMP = MIN0(NY1,NY2)
      IF (LTEMP .LT. LIML(NX1)) LIML(NX1) = LTEMP
  370 RETURN
      END
      SUBROUTINE TRN32S (X,Y,Z,XT,YT,ZT,IFLAG)
C
C THIS ROUTINE IMPLEMENTS THE 3-SPACE TO 2-SPACE TRANSFOR-
C MATION BY KUBER, SZABO AND GIULIERI, THE PERSPECTIVE
C REPRESENTATION OF FUNCTIONS OF TWO VARIABLES. J. ACM 15,
C 2, 193-204,1968.
C IFLAG=0 ARGUMENTS
C X,Y,Z    ARE THE 3-SPACE COORDINATES OF THE INTERSECTION
C          OF THE LINE OF SIGHT AND THE IMAGE PLANE.    THIS
C          POINT CAN BE THOUGHT OF AS THE POINT LOOKED AT.
C XT,YT,ZT ARE THE 3-SPACE COORDINATES OF THE EYE POSITION.
C
C IFLAG=1 ARGUMENTS
C X,Y,Z    ARE THE 3-SPACE COORDINATES OF A POINT TO BE
C          TRANSFORMED.
C XT,YT    THE RESULTS OF THE 3-SPACE TO 2-SPACE TRANSFOR-
C          MATION.
C          USE IFIX(XT) AND IFIX(YT) IN GPL CALLS.
C ZT            NOT USED.
C IF LL (IN COMMON) =0 XT AND YT ARE IN THE SAME SCALE AS X, Y, AND Z.
C
      COMMON /PWRZ1S/ XXMIN      ,XXMAX      ,YYMIN  ,YYMAX,
     1                ZZMIN      ,ZZMAX      ,DELCRT ,EYEX,
     2                EYEY       ,EYEZ
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL,
     1                LL         ,FACT       ,IROT       ,NDRZ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO,
     4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
      DIMENSION       NLU(7)     ,NRU(7)     ,NBV(7)     ,NTV(7)
C
C SAVE INSERTED BY BEN DOMENICO 9/8/85 BECAUSE OF ASSUMPTION THAT
C   JUMP, JUMP2, AND JUMP3 ARE PRESERVED BETWEEN CALLS.
C   THERE MAY BE OTHER SUCH ASSUMPTIONS AS WELL.
C
      SAVE
C
C PICTURE CORNER COORDINATES FOR LL=1
C
      DATA NLU(1),NRU(1),NBV(1),NTV(1)/  10,1014,  10,1014/
C
C PICTURE CORNER COORDINATES FOR LL=2
C
      DATA NLU(2),NRU(2),NBV(2),NTV(2)/  10, 924,  50, 964/
C
C PICTURE CORNER COORDINATES FOR LL=3
C
      DATA NLU(3),NRU(3),NBV(3),NTV(3)/ 100,1014,  50, 964/
C
C PICTURE CORNER COORDINATES FOR LL=4
C
      DATA NLU(4),NRU(4),NBV(4),NTV(4)/  10,1014,  10,1014/
C
C PICTURE CORNER COORDINATES FOR LL=5
C
      DATA NLU(5),NRU(5),NBV(5),NTV(5)/  10,1014,  10,1014/
C
C PICTURE CORNER COORDINATES FOR LL=6
C
      DATA NLU(6),NRU(6),NBV(6),NTV(6)/  10, 512, 256, 758/
C
C PICTURE CORNER COORDINATES FOR LL=7
C
      DATA NLU(7),NRU(7),NBV(7),NTV(7)/ 512,1014, 256, 758/
C
C STORE THE PARAMETERS OF THE SET32 CALL FOR USE WHEN
C TRN32 IS CALLED.
C
      IF (IFLAG)  40, 10, 40
   10 CONTINUE
      ASSIGN  60 TO JUMP3
      IF (IOFFP .EQ. 1) ASSIGN  50 TO JUMP3
      AX = X
      AY = Y
      AZ = Z
      EX = XT
      EY = YT
      EZ = ZT
C
C AS MUCH COMPUTATION AS POSSIBLE IS DONE DURING EXECUTION
C THIS ROUTINE WHEN IFLAG=0 BECAUSE CALLS IN THAT MODE ARE INFREQUENT.
C
      DX = AX-EX
      DY = AY-EY
      DZ = AZ-EZ
      D = SQRT(DX*DX+DY*DY+DZ*DZ)
      COSAL = DX/D
      COSBE = DY/D
      COSGA = DZ/D
      SINGA = SQRT(1.-COSGA*COSGA)
      ASSIGN 120 TO JUMP2
      IF (LL .EQ. 0) GO TO  20
      ASSIGN 100 TO JUMP2
      DELCRT = NRU(LL)-NLU(LL)
      U0 = UMIN
      V0 = VMIN
      U1 = NLU(LL)
      V1 = NBV(LL)
      U2 = NRU(LL)-NLU(LL)
      V2 = NTV(LL)-NBV(LL)
      U3 = U2/(UMAX-UMIN)
      V3 = V2/(VMAX-VMIN)
      U4 = NRU(LL)
      V4 = NTV(LL)
      IF (NRSWT .EQ. 0) GO TO  20
      U0 = -BIGD
      V0 = -BIGD
      U3 = U2/(2.*BIGD)
      V3 = V2/(2.*BIGD)
C
C THE 3-SPACE POINT LOOKED AT IS TRANSFORMED INTO (0,0) OF
C THE 2-SPACE.  THE 3-SPACE Z AXIS IS TRANSFORMED INTO THE
C 2-SPACE Y AXIS.  IF THE LINE OF SIGHT IS CLOSE TO PARALLEL
C TO THE 3-SPACE Z AXIS, THE 3-SPACE Y AXIS IS CHOSEN (IN-
C STEAD OF THE 3-SPACE Z AXIS) TO BE TRANSFORMED INTO THE
C 2-SPACE Y AXIS.
C
   20 IF (SINGA .LT. 0.0001) GO TO  30
      R = 1./SINGA
      ASSIGN  70 TO JUMP
      RETURN
   30 SINBE = SQRT(1.-COSBE*COSBE)
      R = 1./SINBE
      ASSIGN  80 TO JUMP
      RETURN
   40 CONTINUE
      XX = X
      YY = Y
      ZZ = Z
      GO TO JUMP3,( 50, 60)
   50 IF (ZZ .EQ. SPVAL) GO TO 110
   60 Q = D/((XX-EX)*COSAL+(YY-EY)*COSBE+(ZZ-EZ)*COSGA)
      GO TO JUMP,( 70, 80)
   70 XX = ((EX+Q*(XX-EX)-AX)*COSBE-(EY+Q*(YY-EY)-AY)*COSAL)*R
      YY = (EZ+Q*(ZZ-EZ)-AZ)*R
      GO TO  90
   80 XX = ((EZ+Q*(ZZ-EZ)-AZ)*COSAL-(EX+Q*(XX-EX)-AX)*COSGA)*R
      YY = (EY+Q*(YY-EY)-AY)*R
   90 GO TO JUMP2,(100,120)
c + NOAO: Clipping is done at the gio level and is unnecessary here.  The
c following statements were preventing labels from being positioned properly
c at the edges of the surface plot, even when the viewport had been reset.
  100 xx = u1 + u3 * (fact * xx - u0)
      yy = v1 + v3 * (fact * yy - v0)
c 100 XX = AMIN1(U4,AMAX1(U1,U1+U3*(FACT*XX-U0)))
c     YY = AMIN1(V4,AMAX1(V1,V1+V3*(FACT*YY-V0)))
c -NOAO
      GO TO 120
  110 XX = NSPVAL
      YY = NSPVAL
C
  120 XT = XX
      YT = YY
      RETURN
      END
      SUBROUTINE CLSET (Z,MX,NX,NY,CHI,CLO,CINC,NLA,NLM,CL,NCL,ICNST,
     1                  IOFFP,SPVAL,BIGEST)
      DIMENSION       Z(MX,NY)   ,CL(NLM)
      DATA KK /0/
C
C CLSET PUTS THE VALUS OF THE CONTOUR LEVELS IN CL
C
      ICNST = 0
      GLO = CLO
      HA = CHI
      FANC = CINC
      CRAT = NLA
      IF (HA-GLO)  10, 20, 50
   10 GLO = HA
      HA = CLO
      GO TO  50
   20 GLO = BIGEST
      HA = -GLO
      DO  40 J=1,NY
         DO  30 I=1,NX
            IF (IOFFP.EQ.1 .AND. Z(I,J).EQ.SPVAL) GO TO  30
            GLO = AMIN1(Z(I,J),GLO)
            HA = AMAX1(Z(I,J),HA)
   30    CONTINUE
   40 CONTINUE
   50 IF (FANC)  60, 70, 90
   60 CRAT = -FANC
   70 FANC = (HA-GLO)/CRAT
      IF (FANC) 140,140, 80
   80 P = 10.**(IFIX(ALOG10(FANC)+500.)-500)
      FANC = AINT(FANC/P)*P
   90 IF (CHI-CLO) 110,100,110
  100 GLO = AINT(GLO/FANC)*FANC
      HA = AINT(HA/FANC)*FANC
  110 DO 120 K=1,NLM
         CC = GLO+FLOAT(K-1)*FANC
         IF (CC .GT. HA) GO TO 130
         KK = K
         CL(K) = CC
  120 CONTINUE
  130 NCL = KK
      RETURN
  140 ICNST = 1
      RETURN
      END
      SUBROUTINE CTCELL (Z,MX,NX,NY,M,I0,J0)
C
C CTCELL COMPUTES LINES OF CONSTANT Z (CONTOUR LINES) IN ONE
C CELL OF THE ARRAY Z FOR THE SRFACE PACKAGE.
C Z,MX,NX,NY ARE THE SAME AS IN SRFACE.
C M              BY     THE TIME CTCELL IS FIRST CALLED, M CONTAINS
C            THE TWO-SPACE PLOTTER LOCATION OF EACH Z POINT.
C            U(Z(I,J))=M(1,I,J).  V(Z(I,J))=M(2,I,J)
C I0,J0      THE CELL Z(I0,J0) TO Z(I0+1,J0+1) IS THE ONE TO
C            BE CONTOURED.
C
      DIMENSION       Z(MX,NY)   ,M(2,NX,NY)
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL,
     1                LL         ,FACT       ,IROT       ,NDRZ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO,
     4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
      DATA IDUB/0/
      R(HO,HU) = (HO-CV)/(HO-HU)
      I1 = I0
      I1P1 = I1+1
      J1 = J0
      J1P1 = J1+1
      H1 = Z(I1,J1)
      H2 = Z(I1,J1P1)
      H3 = Z(I1P1,J1P1)
      H4 = Z(I1P1,J1)
      IF (IOFFP .NE. 1) GO TO  10
      IF (H1.EQ.SPVAL .OR. H2.EQ.SPVAL .OR. H3.EQ.SPVAL .OR.
     1    H4.EQ.SPVAL) RETURN
   10 IF (AMIN1(H1,H2,H3,H4) .GT. CL(NCL)) RETURN
      DO 110 K=1,NCL
C
C FOR EACH CONTOUR LEVEL, DESIDE WHICH OF THE 16 BASIC SIT-
C UATIONS EXISTS, THEN INTERPOLATE IN TWO-SPACE TO FIND THE
C END POINTS OF THE CONTOUR LINE SEGMENT WITHIN THIS CELL.
C
         CV = CL(K)
         K1 = (IFIX(SIGN(1.,H1-CV))+1)/2
         K2 = (IFIX(SIGN(1.,H2-CV))+1)/2
         K3 = (IFIX(SIGN(1.,H3-CV))+1)/2
         K4 = (IFIX(SIGN(1.,H4-CV))+1)/2
         JUMP = 1+K1+K2*2+K3*4+K4*8
         GO TO (120, 30, 50, 60, 70, 20, 80, 90, 90, 80,
     1           40, 70, 60, 50, 30,110),JUMP
   20    IDUB = 1
   30    RA = R(H1,H2)
         MUA = FLOAT(M(1,I1,J1))+RA*FLOAT(M(1,I1,J1P1)-M(1,I1,J1))
         MVA = FLOAT(M(2,I1,J1))+RA*FLOAT(M(2,I1,J1P1)-M(2,I1,J1))
         RB = R(H1,H4)
         MUB = FLOAT(M(1,I1,J1))+RB*FLOAT(M(1,I1P1,J1)-M(1,I1,J1))
         MVB = FLOAT(M(2,I1,J1))+RB*FLOAT(M(2,I1P1,J1)-M(2,I1,J1))
         GO TO 100
   40    IDUB = -1
   50    RA = R(H2,H1)
         MUA = FLOAT(M(1,I1,J1P1))+RA*FLOAT(M(1,I1,J1)-M(1,I1,J1P1))
         MVA = FLOAT(M(2,I1,J1P1))+RA*FLOAT(M(2,I1,J1)-M(2,I1,J1P1))
         RB = R(H2,H3)
         MUB = FLOAT(M(1,I1,J1P1))+RB*FLOAT(M(1,I1P1,J1P1)-M(1,I1,J1P1))
         MVB = FLOAT(M(2,I1,J1P1))+RB*FLOAT(M(2,I1P1,J1P1)-M(2,I1,J1P1))
         GO TO 100
   60    RA = R(H2,H3)
         MUA = FLOAT(M(1,I1,J1P1))+RA*FLOAT(M(1,I1P1,J1P1)-M(1,I1,J1P1))
         MVA = FLOAT(M(2,I1,J1P1))+RA*FLOAT(M(2,I1P1,J1P1)-M(2,I1,J1P1))
         RB = R(H1,H4)
         MUB = FLOAT(M(1,I1,J1))+RB*FLOAT(M(1,I1P1,J1)-M(1,I1,J1))
         MVB = FLOAT(M(2,I1,J1))+RB*FLOAT(M(2,I1P1,J1)-M(2,I1,J1))
         GO TO 100
   70    RA = R(H3,H2)
         MUA = FLOAT(M(1,I1P1,J1P1))+
     1         RA*FLOAT(M(1,I1,J1P1)-M(1,I1P1,J1P1))
         MVA = FLOAT(M(2,I1P1,J1P1))+
     1         RA*FLOAT(M(2,I1,J1P1)-M(2,I1P1,J1P1))
         RB = R(H3,H4)
         MUB = FLOAT(M(1,I1P1,J1P1))+
     1         RB*FLOAT(M(1,I1P1,J1)-M(1,I1P1,J1P1))
         MVB = FLOAT(M(2,I1P1,J1P1))+
     1         RB*FLOAT(M(2,I1P1,J1)-M(2,I1P1,J1P1))
         IDUB = 0
         GO TO 100
   80    RA = R(H2,H1)
         MUA = FLOAT(M(1,I1,J1P1))+RA*FLOAT(M(1,I1,J1)-M(1,I1,J1P1))
         MVA = FLOAT(M(2,I1,J1P1))+RA*FLOAT(M(2,I1,J1)-M(2,I1,J1P1))
         RB = R(H3,H4)
         MUB = FLOAT(M(1,I1P1,J1P1))+
     1         RB*FLOAT(M(1,I1P1,J1)-M(1,I1P1,J1P1))
         MVB = FLOAT(M(2,I1P1,J1P1))+
     1         RB*FLOAT(M(2,I1P1,J1)-M(2,I1P1,J1P1))
         GO TO 100
   90    RA = R(H4,H1)
         MUA = FLOAT(M(1,I1P1,J1))+RA*FLOAT(M(1,I1,J1)-M(1,I1P1,J1))
         MVA = FLOAT(M(2,I1P1,J1))+RA*FLOAT(M(2,I1,J1)-M(2,I1P1,J1))
         RB = R(H4,H3)
         MUB = FLOAT(M(1,I1P1,J1))+RB*FLOAT(M(1,I1P1,J1P1)-M(1,I1P1,J1))
         MVB = FLOAT(M(2,I1P1,J1))+RB*FLOAT(M(2,I1P1,J1P1)-M(2,I1P1,J1))
         IDUB = 0
  100    CALL DRAWS (MUA,MVA,MUB,MVB,1,0)
         IF (IDUB)  90,110, 70
  110 CONTINUE
  120 RETURN
      END
