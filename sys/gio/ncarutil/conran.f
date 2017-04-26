      SUBROUTINE CONRAN (XD,YD,ZD,NDP,WK,IWK,SCRARR)
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
C
C
C    SUBROUTINE CONRAN(XD,YD,ZD,NDP,WK,IWK,SCRARR)
C    STANDARD AND SMOOTH VERSIONS OF CONRAN
C
C          DIMENSION OF  XD(NDP),YD(NDP),ZD(NDP),WK(13*NDP)
C             ARGUMENTS  IWK((27+NCP)*NDP),SCRARR(RESOLUTION**2)
C                        WHERE NCP = 4 AND RESOLUTION = 40 BY
C                        DEFAULT.
C
C       LATEST REVISION  JULY 1984
C
C              OVERVIEW  CONRAN  PERFORMS  CONTOURING  OF  IRREGULARLY
C                        DISTRIBUTED  DATA.   IT IS THE STANDARD AND
C                        SMOOTH MEMBERS OF THE CONRAN FAMILY. THIS
C                        VERSION  WILL  PLOT CONTOURS; SMOOTH THEM USING
C                        SPLINES  UNDER  TENSION (IF THE PACKAGE DASHSMTH
C                        IS LOADED); PLOT A PERIMETER OR GRID; TITLE THE
C                        PLOT; PRINT A MESSAGE GIVING THE CONTOUR INTERVALS
C                        BELOW THE MAP; PLOT THE INPUT DATA ON THE MAP;
C                        AND LABEL THE CONTOUR LINES.
C
C               PURPOSE  CONRAN  PLOTS  CONTOUR  LINES  USING  RANDOM,
C                        SPARSE  OR  IRREGULAR DATA SETS.  THE DATA IS
C                        TRIANGULATED AND THEN CONTOURED.   CONTOURING
C                        IS  PERFORMED USING INTERPOLATION OF THE TRI-
C                        ANGULATED DATA.  THERE ARE TWO METHODS OF
C                        INTERPOLATION:  C1 SURFACES AND LINEAR.
C
C                 USAGE  CALL CONRAN(XD,YD,ZD,NDP,WK,IWK,SCRARR)
C                        AN OPTION SETTING ROUTINE  CAN  ALSO  BE  IN-
C                        VOKED,  SEE  WRITEUP  BELOW.   FRAME  MUST BE
C                        CALLED BY THE USER.
C
C                        IF DIFFERENT COLORS (OR INTENSITIES) ARE TO BE
C                        USED FOR NORMAL INTENSITY, LOW INTENSITY OR
C                        TEXT OUTPUT, THEN THE VALUES IN COMMON BLOCK
C                        RANINT SHOULD BE CHANGED:
C
C                        IRANMJ  COLOR INDEX FOR NORMAL (MAJOR) INTENSITY
C                                LINES.
C                        IRANMN  COLOR INDEX FOR LOW INTENSITY LINES
C                        IRANTX  COLOR INDEX FOR TEXT (LABELS)
C
C
C             ARGUMENTS
C
C              ON INPUT  XD
C                            ARRAY OF DIMENSION NDP CONTAINING THE  X-
C                            COORDINATES OF THE DATA POINTS.
C
C                        YD
C                            ARRAY OF DIMENSION NDP CONTAINING THE  Y-
C                            COORDINATES OF THE DATA POINTS.
C
C                        ZD
C                            ARRAY OF  DIMENSION  NDP  CONTAINING  THE
C                            DATA VALUES AT THE POINTS.
C
C                        NDP
C                            NUMBER OF  DATA  POINTS  (MUST  BE  4  OR
C                            GREATER) TO BE CONTOURED.
C
C                        WK
C                            REAL WORK ARRAY  OF  DIMENSION  AT  LEAST
C                            13*NDP
C
C                        IWK
C                            INTEGER WORK ARRAY.  WHEN USING C1 SURFACES
C                            THE ARRAY MUST BE AT LEAST IWK((27+NCP)*NDP).
C                            WHEN USING LINEAR INTERPOLATION THE ARRAY
C                            MUST BE AT LEAST IWK((27+4)*NDP).
C
C                        SCRARR
C                            REAL WORK ARRAY  OF  DIMENSION  AT  LEAST
C                            (RESOLUTION**2)   WHERE   RESOLUTION   IS
C                            DESCRIBED IN THE SSZ OPTION BELOW.  RESO-
C                            LUTION IS 40 BY DEFAULT.
C
C             ON OUTPUT  ALL ARGUMENTS  REMAIN  UNCHANGED  EXCEPT  THE
C                        SCRATCH ARRAYS IWK, WK, AND SCRARR WHICH HAVE
C                        BEEN WRITTEN INTO.  IF MAKING  MULTIPLE  RUNS
C                        ON  THE SAME TRIANGULATION IWK AND WK MUST BE
C                        SAVED AND RETURNED TO THE NEXT INVOCATION  OF
C                        CONRAN.
C
C          ENTRY POINTS  CONRAN, CONDET, CONINT, CONCAL, CONLOC, CONTNG,
C                        CONDRW, CONCLS, CONSTP, CONBDN, CONTLK
C                        CONPDV, CONOP1, CONOP2, CONOP3, CONOP4,
C                        CONXCH, CONREO, CONCOM, CONCLD, CONPMM,
C                        CONGEN, CONLOD, CONECD, CONOUT, CONOT2,
C                        CONSLD, CONLCM, CONLIN, CONDSD, CONSSD
C
C         COMMON BLOCKS  CONRA1, CONRA2, CONRA3, CONRA4, CONRA5, CONRA6,
C                        CONRA7, CONRA8, CONRA9, CONR10, CONR11, CONR12,
C                        CONR13, CONR14, CONR15, CONR16, CONR17, RANINT
C                        INTPR FROM THE DASH PACKAGE
C
C                   I/O  PLOTS THE CONTOUR MAP AND, VIA  THE ERPRT77
C                        PACKAGE,  OUTPUTS  MESSAGES  TO  THE MESSAGE
C                        OUTPUT  UNIT; AT  NCAR  THIS  UNIT  IS  THE
C                        PRINTER.  THE OPTION VALUES ARE ALL LISTED ON
C                        STANDARD ERPRT77 OUTPUT UNIT; AT NCAR THIS
C                        UNIT IS THE PRINTER.
C
C             PRECISION  SINGLE
C
C      REQUIRED LIBRARY  STANDARD VERSION: DASHCHAR, WHICH AT NCAR IS
C              ROUTINES  LOADED BY DEFAULT.
C                        SMOOTH VERSION: DASHSMTH WHICH MUST BE
C                        REQUESTED AT NCAR.
C                        BOTH VERSIONS REQUIRE CONCOM, CONTERP, GRIDAL
C                        THE ERPRT77 PACKAGE, AND THE SPPS.
C
C              LANGUAGE  FORTRAN77
C
C               HISTORY
C
C             ALGORITHM  THE SPARSE DATA IS TRIANGULATED AND A VIRTUAL
C                        GRID  IS  LAID  OVER  THE  TRIANGULATED AREA.
C                        EACH VIRTUAL GRID POINT RECEIVES AN  INTERPO-
C                        LATED  VALUE.   THE  GRID IS SCANNED ONCE FOR
C                        EACH CONTOUR LEVEL AND ALL CONTOURS  AT  THAT
C                        LEVEL ARE PLOTTED.
C                        THERE ARE TWO METHODS OF INTERPOLATION. THE
C                        FIRST IS A SMOOTH DATA INTERPOLATION
C                        SCHEME BASED  ON  LAWSON'S  C1
C                        SURFACE  INTERPOLATION  ALGORITHM,  WHICH HAS
C                        BEEN REFINED  BY  HIROSHA  AKIMA.   PARTS  OF
C                        AKIMA'S  ALGORITHM  ARE USED IN THIS PACKAGE.
C                        SEE THE "REFERENCE" SECTION BELOW.
C                        THE SECOND IS A LINEAR INTERPOLATION SCHEME.
C                        WHEN DATA IS SPARSE IT IS USUALLY BETTER TO
C                        USE THE C1 INTERPOLATION.  IF YOU HAVE DENSE
C                        DATA (OVER 100 POINTS) THEN THE LINEAR
C                        INTERPOLATION WILL GIVE THE BETTER RESULTS.
C
C           PORTABILITY  ANSI FORTRAN
C
C
C             OPERATION  CALL CONRAN (XD,YD,ZD,NDP,WK,IWK,SCRARR)
C
C                        FRAME MUST BE CALLED BY THE USER.
C
C                        CONRAN HAS MANY OPTIONS, EACH OF WHICH MAY
C                        BE CHANGED BY CALLING ONE OF THE FOUR
C                        SUBROUTINES CONOP1, CONOP2, CONOP3, OR
C                        CONOP4.  THE NUMBER OF ARGUMENTS TO EACH
C                        CONOP ROUTINE IS THE SAME AS THE FINAL
C                        SUFFIX CHARACTER IN THE ROUTINE'S NAME.
C
C                        THE CONOP ROUTINES ARE CALLED BEFORE CONRAN
C                        IS CALLED, AND VALUES SET BY THESE CALLS
C                        CONTINUE TO BE IN EFFECT UNTIL THEY ARE
C                        CHANGED BY ANOTHER CALL TO A CONOP ROUTINE.
C
C                        ALL THE CONOP ROUTINES HAVE AS THEIR FIRST
C                        ARGUMENT A CHARACTER STRING TO IDENTIFY THE
C                        OPTION BEING CHANGED.  THIS IS THE ONLY
C                        ARGUMENT TO CONOP1.  CONOP2 HAS AN INTEGER
C                        SECOND ARGUMENT.  CONOP3 HAS A REAL ARRAY (OR
C                        CONSTANT) AS ITS SECOND ARGUMENT AND AN
C                        INTEGER (USUALLY THE DIMENSION OF THE
C                        ARRAY) AS ITS THIRD ARGUMENT.  CONOP4 HAS A
C                        CHARACTER STRING AS ITS SECOND ARGUMENT AND
C                        INTEGERS FOR THE THIRD AND FOURTH ARGUMENTS.
C
C                        ONLY THE FIRST TWO CHARACTERS ON EACH SIDE OF
C                        THE EQUAL SIGN ARE SCANNED.  THEREFORE ONLY 2
C                        CHARACTERS  FOR  EACH  OPTION ARE REQUIRED ON
C                        INPUT TO CONOP (I.E.  'SCA=PRI'  AND  'SC=PR'
C                        ARE EQUIVALENT.)
C
C                        REMEMBER, THERE MUST BE AT LEAST 4 DATA POINTS.
C                        THIS IS EQUAL TO  THE  DEFAULT  NUMBER OF
C                        DATA POINTS TO BE USED FOR ESTIMATION OF PAR-
C                        TIAL DERIVATIVES AT  EACH  DATA  POINT.
C                        THE ESTIMATED PARTIAL DERIVATIVES  ARE
C                        USED  FOR THE CONSTRUCTION OF THE INTERPOLAT-
C                        ING POLYNOMIAL'S COEFFICIENTS.
C
C                        LISTED BELOW ARE OPTIONS WHICH CAN ENHANCE
C                        YOUR PLOT.  AN EXAMPLE OF AN APPROPRIATE
C                        CONOP CALL IS GIVEN FOR EACH OPTION.  A
C                        COMPLETE LIST OF DEFAULT SETTINGS FOLLOWS
C                        THE LAST OPTION.
C
C               OPTIONS
C
C                   CHL  THIS FLAG DETERMINES HOW  THE  HIGH  AND  LOW
C                        CONTOUR VALUES ARE SET.  THESE CONTOUR VALUES
C                        MAY BE SET BY THE PROGRAM OR BY THE USER.  IF
C                        CHL=OFF,  THE PROGRAM EXAMINES THE USER'S IN-
C                        PUT DATA AND DETERMINES BOTH THE HIGH AND LOW
C                        VALUES.  IF CHL=ON, THE USER MUST SPECIFY THE
C                        DESIRED HIGH (HI) AND LOW (FLO) VALUES.
C                        THE DEFAULT IS CHL=OFF.
C
C                        IF PROGRAM SET:   CALL CONOP3('CHL=OFF',0.,0)
C
C                        IF USER SET:  CALL CONOP3('CHL=ON',ARRAY,2)
C                                WHERE ARRAY(1)=HI, ARRAY(2)=FLO
C
C                        NOTE: THE VALUES SUPPLIED FOR CONTOUR  INCRE-
C                        MENT  AND CONTOUR HIGH AND LOW VALUES ASSUMES
C                        THE UNSCALED DATA VALUES.  SEE THE SDC  FLAG,
C                        BELOW.
C
C                        EXAMPLE:  CALL   CONOP3('CHL=ON',ARRAY,2)
C                                  WHERE  ARRAY(1)=5020. (THE DESIRED
C                                  HIGH CONTOUR VALUE) AND ARRAY(2)=
C                                  2000 (THE DESIRED LOW CONTOUR VALUE).
C                                  THESE ARE FLOATING POINT NUMBERS.
C
C                   CIL  THIS FLAG DETERMINES HOW THE  CONTOUR  INCRE-
C                        MENT  (CINC) IS SET.  THE INCREMENT IS EITHER
C                        CALCULATED BY THE PROGRAM (CIL=OFF) USING THE
C                        RANGE  OF HIGH AND LOW VALUES FROM THE USER'S
C                        INPUT DATA, OR SET BY THE USER (CIL=ON). THE
C                        DEFAULT IS CIL=OFF.
C
C                        IF PROGRAM SET:   CALL CONOP3('CIL=OFF',0.,0)
C
C                        IF USER SET:      CALL CONOP3('CIL=ON',CINC,1)
C
C                        NOTE: BY DEFAULT,  THE  PROGRAM  WILL EXAMINE
C                        THE USER'S INPUT DATA AND DETERMINE THE CONTOUR
C                        INTERVAL (CINC)  AT SOME APPROPRIATE RANGE BETWEEN
C                        THE LEVEL OF HIGH AND LOW VALUES SUPPLIED, USUALLY
C                        GENERATING BETWEEN 15 AND 20 CONTOUR LEVELS.
C                        ELS.
C
C                        EXAMPLE:          CALL CONOP3('CIL=ON',15.,1)
C                                          WHERE  15.  REPRESENTS  THE
C                                          CONTOUR  INCREMENT  DESIRED
C                                          BY THE USER.
C
C                   CON  THIS FLAG DETERMINES HOW THE  CONTOUR  LEVELS
C                        ARE  SET.   IF  CON=ON, THE USER MUST SPECIFY
C                        THE ARRAY OF CONTOUR VALUES AND THE NUMBER OF
C                        CONTOUR LEVELS.  A  MAXIMUM OF 30 CONTOUR (NCL)
C                        LEVELS ARE PERMITTED.  IF CON=OFF,  DEFAULT
C                        VALUES  ARE  USED.  IN THIS CASE, THE PROGRAM
C                        WILL CALCULATE THE VALUES FOR THE  ARRAY  AND
C                        NCL USING INPUT DATA.  THE DEFAULT IS OFF.
C
C                        IF PROGRAM SET:   CALL CONOP3('CON=OFF',0.,0)
C
C                        IF USER SET:   CALL CONOP3('CON=ON',ARRAY,NCL)
C
C                        NOTE: THE ARRAY (ARRAY) CONTAINS THE  CONTOUR
C                        LEVELS  (FLOATING  POINT ONLY) AND NCL IS THE
C                        NUMBER OF LEVELS.  THE MAXIMUM NUMBER OF CON-
C                        TOUR LEVELS ALLOWED IS 30.  WHEN ASSIGNING
C                        THE ARRAY OF CONTOUR VALUES, THE VALUES MUST
C                        BE ORDERED FROM SMALLEST TO LARGEST.
C
C                        EXAMPLE:
C                         DATA RLIST(1),...,RLIST(5)/1.,2.,3.,10.,12./
C
C                                  CALL CONOP3('CON=ON',RLIST,5) WHERE
C                                  'RLIST' CONTAINS THE USER SPECIFIED
C                                  CONTOUR LEVELS, AND 5  IS  THE
C                                  NUMBER  OF  USER  SPECIFIED CONTOUR
C                                  LEVELS (NCL).
C
C                        WARNING ON CONTOUR OPTIONS:
C                        IT IS ILLEGAL TO USE THE CON OPTION WHEN
C                        EITHER  CIL  OR  CHL  ARE ACTIVATED.  IF
C                        THIS IS DONE, THE OPTION CALL THAT DETECTED
C                        THE ERROR WILL NOT BE EXECUTED.
C
C                   DAS  THIS  FLAG  DETERMINES  WHICH  CONTOURS   ARE
C                        REPRESENTED  BY  DASHED LINES.  THE USER SETS
C                        THE DASHED LINE PATTERN.  THE USER MAY SPECI-
C                        FY  THAT  DASHED  LINES  BE USED FOR CONTOURS
C                        WHOSE  VALUE  IS  LESS  THAN,  EQUAL  TO,  OR
C                        GREATER THAN THE DASH PATTERN BREAKPOINT (SEE
C                        THE DBP OPTION BELOW), WHICH IS ZERO BY
C                        DEFAULT.  IF DAS=OFF (THE DEFAULT VALUE), ALL
C                        SOLID LINES ARE USED.
C
C                        ALL SOLID LINES: CALL CONOP4('DAS=OFF',' ',0,0)
C
C                        IF GREATER:      CALL CONOP4('DAS=GTR',PAT,0,0)
C
C                        IF EQUAL:        CALL CONOP4('DAS=EQU',PAT,0,0)
C
C                        IF LESS:         CALL CONOP4('DAS=LSS',PAT,0,0)
C
C                        IF ALL SAME:     CALL CONOP4('DAS=ALL',PAT,0,0)
C
C                        NOTE: PAT MUST BE A TEN CHARACTER
C                        STRING WITH A DOLLAR SIGN ($) FOR SOLID AND A
C                        SINGLE QUOTE (') FOR BLANK.  RECALL THAT IN
C                        FORTRAN 77, IN A QUOTED STRING A SINGLE QUOTE
C                        IS REPRESENTED BY TWO SINGLE QUOTES ('').
C
C                        EXAMPLE:
C                          CALL CONOP4('DAS=GTR','$$$$$''$$$$',0,0)
C
C                   DBP  THIS FLAG DETERMINES  HOW  THE  DASH  PATTERN
C                        BREAK POINT (BP) IS SET.  IF DBP=ON, BP  MUST
C                        BE SET BY THE  USER  BY  SPECIFYING  BP.   IF
C                        DBP=OFF  THE  PROGRAM  WILL SET BP TO THE
C                        DEFAULT VALUE WHICH IS ZERO.
C
C                        IF PROGRAM SET:   CALL CONOP3('DBP=OFF',0.,0)
C
C                        IF USER SET:      CALL CONOP3('DBP=ON',BP,1)
C
C                        NOTE: BP IS A FLOATING POINT NUMBER WHERE THE
C                        BREAK  FOR  GTR AND LSS CONTOUR DASH PATTERNS
C                        ARE DEFINED.  BP IS ASSUMED TO BE GIVEN RELA-
C                        TIVE TO THE UNTRANSFORMED CONTOURS.
C
C                        EXAMPLE:          CALL CONOP3('DBP=ON',5.,1)
C                                          WHERE 5. IS THE USER SPECI-
C                                          FIED BREAK POINT.
C
C                   DEF  RESET FLAGS TO  DEFAULT  VALUES.   ACTIVATING
C                        THIS  OPTION  SETS  ALL  FLAGS TO THE DEFAULT
C                        VALUE.  DEF HAS NO 'ON' OF 'OFF' STATES.
C
C                        TO ACTIVATE:   CALL CONOP1('DEF')
C
C                   EXT  FLAG TO SET EXTRAPOLATION. NORMALLY ALL
C                        CONRAN VERSIONS WILL  ONLY PLOT THE BOUNDARIES
C                        OF THE CONVEX HULL DEFINED BY THE USER'S DATA.
C                        TO HAVE THE CONTOURS FILL THE RECTANGULAR
C                        AREA OF THE FRAME, SET THE EXT SWITCH ON.
C                        THE DEFAULT IS OFF.
C
C                        TO TURN ON:    CALL CONOP1('EXT=ON')
C
C                        TO TURN OFF:   CALL CONOP1('EXT=OFF')
C
C                   FMT  FLAG FOR THE FORMAT OF THE PLOTTED INPUT DATA
C                        VALUES.   IF  FMT=OFF, THE DEFAULT VALUES FOR
C                        FT, L, AND IF ARE USED.  THE  DEFAULT  VALUES
C                        ARE:
C
C                        FT = '(G10.3)'
C                        L  = 7  CHARACTERS INCLUDING THE PARENTHESES
C                        IF = 10 CHARACTERS PRINTED IN THE OUTPUT
C                             FIELD BY THE FORMAT
C
C                        IF FMT=ON, THE USER MUST SPECIFY  VALUES  FOR
C                        FT,  L,  AND  IF.   ALL USER SPECIFIED VALUES
C                        MUST BE GIVEN IN THE CORRECT FORMAT.
C
C                        IF PROGRAM SET:  CALL CONOP4('FMT=OFF',' ',0,0)
C
C                        IF USER SET:  CALL CONOP4('FMT=ON',FT,L,IF)
C
C                        NOTE: FT IS A CHARACTER STRING CONTAINING THE
C                        FORMAT.   THE  FORMAT  MUST  BE  ENCLOSED  IN
C                        PARENTHESES.  ANY  FORMAT, UP TO 10 CHARACTERS
C                        WHICH IS  ALLOWED  AT  YOUR INSTALLATION WILL BE
C                        ACCEPTED.  L IS THE NUMBER OF  CHARACTERS  IN
C                        FT.  IF IS THE LENGTH OF THE FIELD CREATED BY
C                        THE FORMAT.
C
C                        EXAMPLE:  CALL CONOP4('FMT=ON','(G30.2)',7,30)
C
C                        WARNING: CONRAN WILL NOT  TEST  FOR  A  VALID
C                        FORMAT.  THE FORMAT IS ONLY ALLOWED TO BE
C                        10 CHARACTERS LONG.
C
C                   GRI  FLAG TO DISPLAY THE GRID. GRI IS OFF BY DEFAULT.
C
C                        TO TURN ON:    CALL CONOP1('GRI=ON')
C
C                        TO TURN OFF:   CALL CONOP1('GRI=OFF')
C
C                        NOTE: IF GRI IS ON, THE VIRTUAL GRID WILL
C                        BE SUPERIMPOSED OVER THE CONTOUR PLOT.
C                        THE X AND Y TICK INTERVALS WILL BE DISPLAYED
C                        UNDER THE MAP ONLY IF PER=ON.  (SEE PER)
C
C                   INT  FLAG TO DETERMINE THE INTENSITIES OF THE CON-
C                        TOUR  LINES  AND OTHER PARTS OF THE PLOT.  IF
C                        INT=OFF,  ALL INTENSITIES ARE SET TO THE DEFAULT
C                        VALUES.  IF INT=ALL, ALL INTENSITIES ARE SET
C                        TO THE GIVEN VALUE, IVAL.  IF INT IS SET TO
C                        ONE OF THE OTHER POSSIBLE OPTIONS (MAJ, MIN,
C                        LAB OR DAT), THE INTENSITY LEVEL FOR THAT
C                        OPTION IS SET TO THE GIVEN VALUE, IVAL.
C
C                        IF PROGRAM SET:   CALL CONOP2('INT=OFF',0)
C
C                        ALL THE SAME:     CALL  CONOP2('INT=ALL',IVAL)
C
C                        MAJOR LINES:      CALL CONOP2('INT=MAJ',IVAL)
C
C                        MINOR LINES:      CALL CONOP2('INT=MIN',IVAL)
C
C                        TITLE AND MESSAGE:
C                                          CALL  CONOP2('INT=LAB',IVAL)
C
C                        DATA VALUES:      CALL  CONOP2('INT=DAT',IVAL)
C
C                        NOTE: 'INT=DAT' RELATES TO THE PLOTTED DATA
C                        VALUES AND THE PLOTTED MAXIMUMS AND MINIMUMS.
C
C                        NOTE: IVAL IS THE INTENSITY DESIRED.  FOR  AN
C                        EXPLANATION  OF THE OPTION VALUE SETTINGS SEE
C                        THE OPTN ROUTINE  IN  THE  NCAR  SYSTEM  PLOT
C                        PACKAGE DOCUMENTATION.  BRIEFLY, IVAL  VALUES
C                        RANGE FROM 0 TO 255 OR THE CHARACTER  STRINGS
C                        'LO'  AND  'HI'.   THE DEFAULT IS 'HI' EXCEPT
C                        FOR INT=MIN WHICH IS SET TO 'LO'.
C
C                        EXAMPLE:           CALL CONOP2('INT=ALL',110)
C
C                   ITP  SET THE INTERPOLATION SCHEME.
C                        THERE ARE TWO SCHEMES--C1 SURFACES AND LINEAR.
C                        THE C1 METHOD TAKES LONGER BUT WILL GIVE THE
C                        BEST RESULTS WHEN THE DATA IS SPARSE (LESS
C                        THAN 100 POINTS).  THE LINEAR METHOD WILL
C                        PRODUCE A BETTER PLOT WHEN THERE IS A DENSE
C                        DATA SET.  THE DEFAULT IS C1 SURFACE.
C
C                        FOR C1 SURFACE   CALL CONOP1('ITP=C1')
C
C                        FOR LINEAR       CALL CONOP1('ITP=LIN')
C
C                   LAB  THIS FLAG CAN BE SET TO EITHER LABEL THE CON-
C                        TOURS (LAB=ON) OR NOT (LAB=OFF).  THE DEFAULT
C                        VALUE IS LAB=ON.
C
C                        TO TURN ON:    CALL CONOP1('LAB=ON')
C
C                        TO TURN OFF:   CALL CONOP1('LAB=OFF')
C
C                   LOT  FLAG TO LIST OPTIONS ON THE PRINTER.  THE DE-
C                        FAULT  VALUE  IS  SET  TO OFF, AND NO OPTIONS
C                        WILL BE DISPLAYED.
C
C                        TO TURN ON:    CALL CONOP1('LOT=ON')
C
C                        TO TURN OFF:   CALL CONOP1('LOT=OFF')
C
C                        NOTE: IF  USERS  WANT  TO  PRINT  THE  OPTION
C                        VALUES, THEY SHOULD TURN THIS OPTION ON.  THE
C                        OPTION VALUES WILL BE SENT  TO  THE  STANDARD
C                        OUTPUT  UNIT  AS  DEFINED BY THE SUPPORT
C                        ROUTINE I1MACH.
C
C                   LSZ  THIS FLAG  DETERMINES  THE  LABEL  SIZE.   IF
C                        LSZ=OFF,  THE  DEFAULT ISZLSZ VALUE WILL BE
C                        USED.  IF LSZ=ON,  THE  USER  SHOULD  SPECIFY
C                        ISZLSZ.   THE  DEFAULT VALUE IS 9 PLOTTER
C                        ADDRESS UNITS.
C
C                        IF PROGRAM SET:   CALL CONOP2('LSZ=OFF',0)
C
C                        IF USER SET:   CALL CONOP2('LSZ=ON',ISZLSZ)
C
C                        NOTE: ISZLSZ  IS  THE  REQUESTED  CHARACTER
C                        SIZE IN PLOTTER ADDRESS UNITS.
C
C                        EXAMPLE:          CALL CONOP2('LSZ=ON',4)
C                                          WHERE 4 IS THE USER DESIRED
C                                          INTEGER PLOTTER ADDRESS
C                                          UNITS.
C
C                   MES  FLAG TO PLOT A MESSAGE. THE DEFAULT IS ON.
C
C                        TO TURN ON:    CALL CONOP1('MES=ON')
C
C                        TO TURN OFF:   CALL CONOP1('MES=OFF')
C
C                        NOTE: IF MES=ON, A MESSAGE IS  PRINTED   BELOW
C                        THE  PLOT GIVING CONTOUR INTERVALS AND EXECU-
C                        TION TIME IN SECONDS.  IF PER OR GRI ARE  ON,
C                        THE  MESSAGE  ALSO  CONTAINS THE X AND Y TICK
C                        INTERVALS.
C
C                   NCP  FLAG TO INDICATE THE NUMBER OF DATA POINTS
C                        USED FOR THE PARTIAL DERIVATIVE
C                        ESTIMATION.   IF NCP=OFF, NUM IS SET TO
C                        4, WHICH IS THE DEFAULT  VALUE.   IF  NCP=ON,
C                        THE  USER  MUST  SPECIFY  NUM GREATER THAN OR
C                        EQUAL TO 2.
C
C                        IF PROGRAM SET:   CALL CONOP2('NCP=OFF',0)
C
C                        IF USER SET:      CALL CONOP2('NCP=ON',NUM)
C
C                        NOTE: NUM = NUMBER OF DATA  POINTS  USED  FOR
C                        ESTIMATION.   CHANGING THIS VALUE EFFECTS THE
C                        CONTOURS PRODUCED AND THE SIZE OF INPUT ARRAY
C                        IWK.
C
C                        EXAMPLE:          CALL CONOP2('NCP=ON',3)
C
C                   PDV  FLAG TO PLOT THE INPUT DATA VALUES.  THE
C                        DEFAULT VALUE IS PDV=OFF.
C
C                        TO TURN ON:    CALL CONOP1('PDV=ON')
C
C                        TO TURN OFF:   CALL CONOP1('PDV=OFF')
C
C                        NOTE: IF PDV=ON, THE INPUT  DATA  VALUES  ARE
C                        PLOTTED  RELATIVE  TO  THEIR  LOCATION ON THE
C                        CONTOUR MAP.  IF YOU ONLY WISH TO SEE THE
C                        LOCATIONS  AND  NOT THE VALUES, SET PDV=ON AND
C                        CHANGE FMT TO PRODUCE AN ASTERISK (*) SUCH AS
C                        (I1).
C
C                   PER  FLAG TO SET THE PERIMETER.  THE DEFAULT VALUE
C                        IS  PER=ON,  WHICH  CAUSES  A PERIMETER TO BE
C                        DRAWN AROUND THE CONTOUR PLOT.
C
C                        TO TURN ON:    CALL CONOP1('PER=ON')
C
C                        TO TURN OFF:   CALL CONOP1('PER=OFF')
C
C                        NOTE: IF MES IS ON, THE X AND Y TICK INTERVALS
C                        WILL  BE GIVEN.  THESE ARE THE INTERVALS IN USER
C                        COORDINATES THAT EACH TICK MARK REPRESENTS.
C
C                   PMM  FLAG TO PLOT RELATIVE MINIMUMS AND MAXIMUMS.
C                        THIS FLAG IS OFF BY DEFAULT.
C
C                        TO TURN OFF:      CALL CONOP1('PMM=OFF')
C
C                        TO TURN ON:       CALL CONOP1('PMM=ON')
C
C                   PSL  FLAG WHICH SETS THE PLOT SHIELD OPTION.
C                        THE OUTLINE OF THE SHIELD WILL BE DRAWN ON
C                        THE SAME FRAME AS THE CONTOUR PLOT.
C                        BY DEFAULT THIS OPTION IS OFF.
C                        (SEE SLD OPTION).
C
C                        DRAW THE SHIELD:  CALL CONOP1('PSL=ON')
C
C                        DON'T DRAW IT:    CALL CONOP1('PSL=OFF')
C
C                   REP  FLAG INDICATING THE USE OF THE SAME DATA IN
C                        A NEW EXECUTION.  THE DEFAULT VALUE IS OFF.
C
C                        TO TURN ON:    CALL CONOP1('REP=ON')
C
C                        TO TURN OFF:   CALL CONOP1('REP=OFF')
C
C                        NOTE: IF REP=ON, THE SAME X-Y DATA AND TRIANGU-
C                        LATION  ARE  TO  BE  USED BUT IT IS ASSUMED
C                        THE USER HAS CHANGED CONTOUR VALUES OR RESOLUTION
C                        FOR THIS RUN.  SCRATCH ARRAYS WK AND IWK MUST
C                        REMAIN UNCHANGED.
C
C                   SCA  FLAG FOR SCALING OF THE PLOT ON A FRAME.
C                        THIS FLAG IS ON BY DEFAULT.
C
C                        USER SCALING:     CALL CONOP1('SCA=OFF')
C
C                        PROGRAM SCALING:  CALL CONOP1('SCA=ON')
C
C                        PRIOR WINDOW:     CALL CONOP1('SCA=PRI')
C
C                        NOTE:  WITH  SCA=OFF,  PLOTTING  INSTRUCTIONS
C                        WILL BE ISSUED USING THE USER'S INPUT COORDI-
C                        NATES, UNLESS THEY ARE TRANSFORMED VIA FX AND
C                        FY  TRANSFORMATIONS.   USERS WILL FIND AN
C                        EXTENDED DISCUSSION IN THE "INTERFACING WITH
C                        OTHER GRAPHICS ROUTINES" SECTION BELOW.  THE SCA
C                        OPTION  ASSUMES  THAT ALL INPUT DATA FALLS INTO
C                        THE CURRENT WINDOW SETTING.  WITH SCA=ON, THE
C                        ENTRY POINT WILL ESTABLISH A VIEWPORT SO THAT
C                        THE USER'S PLOT WILL FIT INTO THE  CENTER  90
C                        PERCENT OF THE FRAME.  WHEN SCA=PRI, THE
C                        PROGRAM  MAPS THE USER'S PLOT INSTRUCTIONS  INTO
C                        THE  PORTION OF THE FRAME DEFINED BY THE
C                        CURRENT NORMALIZATION TRANSFORMATION.  SCA=OFF
C                        SHOULD BE USED TO INTERFACE WITH EZMAP.
C
C                   SDC  FLAG TO DETERMINE HOW TO SCALE  THE  DATA  ON
C                        THE CONTOURS.  IF SDC=OFF, THE FLOATING POINT
C                        VALUE IS GIVEN BY SCALE.  IF SDC=ON, THE USER
C                        MAY SPECIFY SCALE.  THE DEFAULT VALUE FOR SCALE
C                        IS 1.
C
C                        IF PROGRAM SET:   CALL CONOP3('SDC=OFF',0.,0)
C
C                        IF USER SET:      CALL CONOP3('SDC=ON',SCALE,1)
C
C                        NOTE: THE DATA PLOTTED ON CONTOUR  LINES  AND
C                        THE  DATA  PLOTTED  FOR RELATIVE MINIMUMS AND
C                        MAXIMUMS WILL BE SCALED BY THE FLOATING POINT
C                        VALUE  GIVEN  BY SCALE.  TYPICAL SCALE VALUES
C                        ARE 10., 100., 1000., ETC.  THE ORIGINAL DATA
C                        VALUES ARE MULTIPLIED BY SCALE.  SCALE MUST BE
C                        A FLOATING POINT NUMBER AND IS DISPLAYED IN THE
C                        MESSAGE  (SEE  MES).
C
C                        EXAMPLE:          CALL CONOP2('SDC=ON',100.,1)
C
C                   SLD  ACTIVATE OR DEACTIVATE THE SHIELDING OPTION.
C                        WHEN THIS OPTION IS ACTIVATED,  ONLY THOSE
C                        CONTOURS WITHIN THE SHIELD ARE DRAWN. THE SHIELD
C                        IS A POLYGON SPECIFIED BY THE USER WHICH MUST
C                        BE GIVEN IN THE SAME COORDINATE RANGE AS THE
C                        THE DATA. IT MUST DEFINE ONLY ONE POLYGON.
C
C                        TO ACTIVATE THE SHIELD:
C                               CALL CONOP3('SLD=ON',ARRAY,ICSD)
C
C                        TO DEACTIVATE THE SHIELD:
C                               CALL CONOP3('SLD=OFF',0.,0)
C
C                        NOTE:  ARRAY IS A REAL ARRAY ICSD ELEMENTS LONG.
C                        THE FIRST ICSD/2 ELEMENTS ARE X COORDINATES AND
C                        THE SECOND ICSD/2 ELEMENTS ARE Y COORDINATES.
C                        ICSD IS THE LENGTH OF ENTIRE ARRAY, THE
C                        NUMBER OF (X + Y) SHIELD COORDS. THE POLYGON
C                        MUST BE CLOSED, THAT IS THE FIRST AND LAST
C                        POINTS DESCRIBING IT MUST BE THE SAME.
C
C                        EXAMPLE:       DIMENSION SHLD
C                                       DATA SHLD/ 7.,10.,10.,7.,7.,
C                                      1           7.,7.,10.,10.,7./
C                                       CALL CONOP3 (6HSLD=ON,SHLD,10)
C
C
C                   SML  FLAG TO DETERMINE THE  SIZE  OF  MINIMUM  AND
C                        MAXIMUM CONTOUR LABELS.  IF SML=OFF, THE
C                        ISZSML DEFAULT VALUE OF  15 IS  USED.
C                        IF SML=ON, THE USER MUST SPECIFY ISZSML.
C
C                        IF PROGRAM SET:   CALL CONOP2('SML=OFF',0)
C
C                        IF USER SET:  CALL CONOP2('SML=ON',ISZSML)
C
C                        NOTE: ISZSML IS AN INTEGER NUMBER WHICH  IS
C                        THE SIZE OF LABELS IN PLOTTER ADDRESS UNITS
C                        AS DEFINED IN THE SPPS ENTRY WTSTR.
C
C                        EXAMPLE:      CALL CONOP2('SML=ON',12)
C
C                   SPD  FLAG FOR THE SIZE OF THE PLOTTED  INPUT  DATA
C                        VALUES.  IF SPD=OFF, THE VALUE OF ISZSPD IS
C                        8, WHICH IS THE DEFAULT.  IF SPD=ON, THE USER
C                        MUST SPECIFY ISZSPD.
C
C                        IF PROGRAM SET:   CALL CONOP2('SPD=OFF',0)
C
C                        IF USER SET:   CALL CONOP2('SPD=ON',ISZSPD)
C
C                        NOTE: ISZSPD IS AN INTEGER NUMBER GIVING THE
C                        SIZE TO PLOT THE DATA VALUES IN PLOTTER ADDRESS
C                        UNITS AS DEFINED IN THE SPPS ENTRY WTSTR.    .
C
C                        EXAMPLE:          CALL CONOP2('SPD=ON',6)
C
C                   SSZ  FLAG TO DETERMINE THE RESOLUTION  (NUMBER  OF
C                        STEPS  IN  EACH  DIRECTION).   IF SSZ=ON, THE
C                        USER SETS ISTEP, OR, IF SSZ=OFF, THE  PROGRAM
C                        WILL  AUTOMATICALLY  SET ISTEP AT THE DEFAULT
C                        VALUE OF 40.
C
C                        IF PROGRAM SET:   CALL CONOP2('SSZ=OFF',0)
C
C                        IF USER SET:      CALL CONOP2('SSZ=ON',ISTEP)
C
C                        NOTE: ISTEP IS AN INTEGER SPECIFYING THE DENSITY
C                        OF THE VIRTUAL GRID. IN MOST CASES, THE DEFAULT
C                        VALUE OF 40 PRODUCES PLEASING CONTOURS.   FOR
C                        COARSER   BUT  QUICKER  CONTOURS,  LOWER  THE
C                        VALUE.  FOR SMOOTHER CONTOURS AT
C                        THE EXPENSE OF TAKING LONGER  TIME,  RAISE
C                        THE  VALUE.   NOTE:   FOR  STEP SIZES GREATER
C                        THAN 200 IN CONRAN, THE ARRAYS  PV  IN  COMMON
C                        CONRA1  AND ITLOC IN COMMON CONRA9,  MUST BE
C                        EXPANDED TO ABOUT 10 MORE THAN ISTEP.
C                        SEE CONRA1 AND CONRA9 COMMENTS BELOW FOR MORE
C                        INFORMATION.
C
C                        EXAMPLE:          CALL    CONOP2('SSZ=ON',25)
C                                          THIS  ISTEP VALUE WILL PRO-
C                                          DUCE A COARSE CONTOUR.
C
C                   STL  FLAG TO DETERMINE  THE  SIZE  OF  THE  TITLE.
C                        ISZSTL  MAY BE SET BY THE USER (STL=ON), OR
C                        THE PROGRAM WILL SET IT TO THE  DEFAULT  SIZE
C                        OF 16 PLOTTER ADDRESS UNITS (STL=OFF).
C
C                        IF PROGRAM SET:   CALL CONOP2('STL=OFF',0)
C
C                        IF USER SET:  CALL  CONOP2('STL=ON',ISZSTL)
C
C                        NOTE: WHEN 30 OR 40 CHARACTERS ARE  USED  FOR
C                        THE TITLE, THE DEFAULT SIZE OF 16 PLOTTER
C                        ADDRESS UNITS WORKS WELL.  FOR LONGER TITLES,
C                        A SMALLER TITLE SIZE IS REQUIRED.
C
C                        EXAMPLE:          CALL CONOP2('STL=ON',13)
C
C                   TEN  FLAG TO DETERMINE THE TENSION FACTOR  APPLIED
C                        WHEN  SMOOTHING  CONTOUR LINES.  THE USER MAY
C                        SET TENS OR ALLOW  THE  PROGRAM  TO  SET  THE
C                        VALUE.   IF  USER SET, TENS MUST HAVE A VALUE
C                        GREATER THAN ZERO AND LESS THAN OR  EQUAL  TO
C                        30.  THE DEFAULT VALUE IS 2.5.
C
C                        IF PROGRAM SET:   CALL CONOP3('TEN=OFF',0.,0)
C
C                        IF USER SET:      CALL CONOP3('TEN=ON',TENS,1)
C
C                        NOTE: TENS IS NOT AVAILABLE IN THE STANDARD
C                        VERSION OF CONRAN.
C                        SMOOTHING OF CONTOUR  LINES  IS  ACCOMPLISHED
C                        WITH  SPLINES  UNDER  TENSION.  TO ADJUST THE
C                        AMOUNT OF SMOOTHING APPLIED, ADJUST THE  TEN-
C                        SION  FACTOR.   SETTING TENS VERY LARGE
C                        (I.E. 30.), EFFECTIVELY SHUTS OFF SMOOTHING.
C
C                        EXAMPLE:          CALL CONOP3('TEN=ON',14.,1)
C
C                   TFR  FLAG TO ADVANCE THE FRAME BEFORE TRIANGULATION.
C                        THE DEFAULT VALUE IS TFR=ON, WHICH MEANS THAT
C                        THE CONTOURS AND THE TRIANGLES WILL BE PLOTTED
C                        ON SEPARATE FRAMES.
C
C                        IF PROGRAM SET:   CALL CONOP1('TFR=ON')
C
C                        TO TURN OFF:      CALL CONOP1('TFR=OFF')
C
C                        NOTE: TRIANGLES ARE PLOTTED  AFTER  THE  CON-
C                        TOURING  IS COMPLETED.  TO SEE THE TRIANGLES
C                        OVER  THE  CONTOURS,  TURN THIS SWITCH OFF.
C
C                   TLE  FLAG TO PLACE A TITLE AT THE TOP OF THE PLOT.
C                        IF  TLE=ON,  THE USER MUST SPECIFY CHARS AND
C                        INUM.  CHARS  IS THE CHARACTER STRING CONTAINING
C                        THE TITLE.  INUM IS THE NUMBER OF CHARACTERS
C                        IN CHARS.  THE DEFAULT VALUE IS  OFF.
C
C                        TO TURN ON: CALL CONOP4('TLE=ON',CHARS,INUM,0)
C
C                        TO TURN OFF:   CALL CONOP4('TLE=OFF',' ',0,0)
C
C                        NOTE: IF LONGER THAN 64-CHARACTER TITLES ARE
C                        DESIRED, THE CHARACTER VARIABLE ISTRNG FOUND
C                        IN CONRA7 MUST BE INCREASED APPROPRIATELY.
C
C                        EXAMPLE: CALL CONOP4('TLE=ON','VECTOR REVIEW'
C                                       ,13,0)
C
C                   TOP  FLAG TO PLOT ONLY THE TRIANGLES.
C
C                        TO TURN OFF:   CALL CONOP1('TOP=OFF')
C
C                        TO TURN ON:    CALL CONOP1('TOP=ON')
C
C                        NOTE: THE USER MAY WISH TO OVERLAY THE TRIAN-
C                        GLES ON SOME OTHER PLOT.  'TOP=ON' WILL
C                        ALLOW  THAT.   THIS  OPTION  WHEN   ACTIVATED
C                        (TOP=ON),  WILL  SET TRI=ON, AND TFR=OFF.  IF
C                        THE USER WANTS TFR=ON, IT SHOULD BE SET AFTER
C                        TOP IS SET.  IF THE USER SETS TOP=OFF IT WILL
C                        SET TRI=OFF AND TFR=ON. IF THE USER WANTS TRI
C                        OR  TFR  DIFFERENT, SET THEM AFTER THE
C                        TOP CALL.
C
C                   TRI  FLAG TO PLOT THE TRIANGULATION.  THE DEFAULT IS
C                        OFF AND THEREFORE THE TRIANGLES ARE NOT DRAWN.
C
C                        TO TURN ON:    CALL CONOP1('TRI=ON')
C
C                        TO TURN OFF:   CALL CONOP1('TRI=OFF')
C
C                        NOTE: PLOTTING THE TRIANGLES WILL INDICATE TO
C                        THE  USER WHERE GOOD AND BAD POINTS OF INTER-
C                        POLATION ARE OCCURRING IN  THE  CONTOUR  MAP.
C                        EQUILATERAL  TRIANGLES ARE OPTIMAL FOR INTER-
C                        POLATION.  QUALITY DEGRADES AS TRIANGLES
C                        APPROACH  A  LONG AND NARROW SHAPE.  THE CONVEX
C                        HULL OF THE  TRIANGULATION  IS  ALSO  A  POOR
C                        POINT OF INTERPOLATION.
C
C        OPTION DEFAULT  BELOW ARE  LISTED  THE  DEFAULT
C                VALUES  VALUES  FOR  THE VARIOUS OPTIONS GIVEN ABOVE.
C                        UNLESS THE USER  SPECIFIES  OTHERWISE,  THESE
C                        VALUES WILL BE USED IN EXECUTION OF THE VARI-
C                        OUS OPTIONS.
C
C                           CHL=OFF  LOT=OFF  SLD=OFF
C                           CIL=OFF  LSZ=OFF  SML=OFF
C                           CON=OFF  MES=ON   SPD=OFF
C                           DAS=OFF  NCP=OFF  SPT=OFF
C                           DBP=OFF  PDV=OFF  SSZ=OFF
C                           EXT=OFF  PER=ON   STL=OFF
C                           FMT=OFF  PMM=OFF  TEN=OFF
C                           GRI=OFF  REP=OFF  TFR=ON
C                           ITP=C1   SCA=ON   TOP=OFF
C                           LAB=ON   SDC=OFF  TRI=OFF
C
C    DEFAULT VALUES FOR  THE OPTION DEFAULT  VALUES  GIVEN  ABOVE,  IF
C        USER SPECIFIED  USED, WILL SET DEFAULT VALUES FOR THE FOLLOW-
C            PARAMETERS  ING PARAMETERS:
C
C                        PARAMETER    DEFAULT
C                        ---------    -------
C
C                        ARRAY        UP TO 30 CONTOUR LEVELS ALLOWED.
C                                     VALUES  ARE COMPUTED BY THE
C                                     PROGRAM, BASED ON INPUT.
C
C                        BP           0.
C
C                        CINC         COMPUTED BY THE PROGRAM BASED ON THE
C                                     RANGE OF HI AND LO VALUES OF THE
C                                     INPUT DATA.
C
C                        FLO          COMPUTED BY THE PROGRAM BASED ON THE
C                                     LOWEST UNSCALED INPUT DATA.
C
C                        FT           (G10.3)  PARENTHESES MUST BE
C                                     INCLUDED.
C
C                        HI           COMPUTED BY THE PROGRAM BASED ON THE
C                                     HIGHEST UNSCALED INPUT DATA.
C
C                        CHARS        NO TITLE
C
C                        IF           10 CHARACTERS
C
C                        INUM         NO TITLE
C
C                        IPAT         '$$$$$$$$$$'  (THIS IS A 10 CHARACTER
C                                     STRING.)
C
C                        ISZLSZ       9 PLOTTER ADDRESS UNITS
C
C                        ISZSML       15 PLOTTER ADDRESS UNITS
C
C                        ISZSPD       8 PLOTTER ADDRESS UNITS
C
C                        ISZSTL       16 PLOTTER ADDRESS UNITS
C
C                        ISTEP        40
C
C                        IVAL         'HI' FOR ALL EXCEPT  MINOR  CON-
C                                     TOUR   LINES   WHICH  ARE  'LO'.
C
C                        L            7  CHARACTERS  (INCLUDING   BOTH
C                                     PARENTHESES)
C
C                        NCL          COMPUTED BY THE PROGRAM BASED ON
C                                     INPUT DATA.  UP TO 30 CONTOUR
C                                     LEVELS ARE PERMITTED.
C
C                        NUM          4 DATA POINTS
C
C                        SCALE        1. (NO SCALING PERFORMED)
C
C                        TENS         2.5
C
C                        ICSD         0 (NO SHIELD)
C
C         OPTIONS WHICH  THE SHAPE OF THE CONTOURS MAY BE MODIFIED BY
C            EFFECT THE  CHANGING NCP  AND SSZ.  NCP CONTROLS THE
C              CONTOURS  NUMBER OF DATA POINTS TO BE USED IN THE
C                        INTERPOLATION.  INCREASING NCP CAUSES MORE
C                        OF THE  SURROUNDING  DATA  TO INFLUENCE THE
C                        POINT OF INTERPOLATION.  SOME  DATASETS CAUSE
C                        DIFFICULTY  WHEN  TRYING  TO PRODUCE MEANINGFUL
C                        CONTOURS (TRIANGLES WHICH ARE LONG AND  NARROW).
C                        BY MODIFYING NCP A USER CAN FINE-TUNE A
C                        PLOT.   INCREASING  ISTEP, THE DENSITY OF THE
C                        VIRTUAL GRID, WILL  SMOOTH OUT THE CONTOUR
C                        LINES AND PICK UP  MORE  DETAIL  (NEW  CONTOURS
C                        WILL APPEAR AS ISTEP INCREASES AND OLD ONES WILL
C                        SOMETIMES BREAK INTO MORE  DISTINCT UNITS).
C                        ISTEP IS CHANGED BY THE SSD OPTION.
C
C                  NOTE  IF NCP.GT.25, ARRAYS DSQ0 AND IPC0 IN CONDET
C                        MUST  BE ADJUSTED ACCORDINGLY.  ALSO NCPSZ IN
C                        CONBDN (25 BY DEFAULT), MUST BE INCREASED TO
C                        NCP.   THE  DEFAULT  VALUE OF NCP, WHICH IS 4,
C                        PRODUCES PLEASING  PICTURES  IN  MOST  CASES.
C                        HOWEVER, FINE-TUNING OF THE INTERPOLATION CAN
C                        BE OBTAINED BY INCREASING THE  SIZE  OF  NCP,
C                        WITH  A CORRESPONDING LINEAR INCREASE IN WORK
C                        SPACE.
C
C                        THE INTERPOLATION METHOD USED WILL ALSO CAUSE
C                        DIFFERENT LOOKING CONTOURS.  THE C1 METHOD
C                        IS RECOMMENDED WHEN THE DATA IS SPARSE.  IT
C                        WILL SMOOTH THE DATA AND ADD TRENDS (FALSE
C                        HILLS AND VALLEYS).  THE LINEAR METHOD IS
C                        RECOMMENDED WHEN DATA IS DENSE (GT 50 TO 100)
C                        IT WILL NOT SMOOTH THE DATA OR ADD TRENDS.
C
C      INTERFACING WITH  NORMALLY THE SCALING FACTOR WILL BE SET TO OFF.
C        OTHER GRAPHICS  IN MOST CASES MAPPING CAN BE PERFORMED BEFORE
C              ROUTINES  CALLING THE CONRAN ENTRY POINT, THUS SAVING THE
C                        USER FROM  MODIFYING THE FILE.  IF REASONABLE
C                        RESULTS CANNOT BE OBTAINED, THE STATEMENT
C                        FUNCTIONS, FX AND FY, WILL HAVE TO BE REPLACED.
C                        THE ROUTINES HAVING THESE STATEMENT FUNCTIONS
C                        ARE:
C
C                           CONDRW, CONPDV, CONTLK, CONPMS, CONGEN
C
C            REFERENCES  AKIMA, HIROSHA
C                           A METHOD OF BIVARIATE INTERPOLATION AND
C                           SMOOTH SURFACE FITTING FOR IRREGULARLY
C                           DISTRIBUTED DATA POINTS.
C                           ACM TRANSACTIONS ON MATHEMATICAL SOFTWARE
C                           VOL 4, NO. 2, JUNE 1978, PAGES 148-159
C                        LAWSON, C.L.
C                           SOFTWARE FOR C1 SURFACE INTERPOLATION
C                           JPL PUBLICATION 77-30
C                           AUGUST 15, 1977
C
C     CONRAN ERROR  ERROR  ROUTINE               MESSAGE
C       MESSAGES
C                    1      CONRAN         INPUT PARAMETER NDP LT NCP
C                    2      CONRAN         NCP GT MAX SIZE OR LT 2
C                    3      CONTNG         ALL COLINEAR DATA POINTS
C                    4      CONTNG         IDENTICAL INPUT DATA POINTS
C                                          FOUND
C                    5      CONOP          UNDEFINED OPTION
C                    6      CONCLS         CONSTANT INPUT FIELD
C                    7      CONOP          INCORRECT CONOP CALL USED
C                    8      CONOP          ILLEGAL USE OF CON OPTION
C                                          WITH CIL OR CHL OPTIONS
C                    9      CONOP          NUMBER OF CONTOUR LEVELS
C                                          EXCEEDS 30
C                    10     CONDRW         CONTOUR STORAGE EXHAUSTED
C                                          THIS ERROR IS TRAPPED AND
C                                          NULLIFIED BY CONRAN.  IT
C                                          SERVES TO SIGNAL THE USER
C                                          THAT A CONTOUR LEVEL MAY NOT
C                                          BE COMPLETE.
C                    11     CONSTP         ASPECT RATIO OF X AND Y
C                                          GREATER THAN 5 TO 1.
C                                          (THIS ERROR MAY CAUSE A POOR
C                                          QUALITY PLOT.  USUALLY THIS
C                                          CAN BE FIXED BY MULTIPLYING
C                                          X OR Y BY A CONSTANT FACTOR.
C                                          IF THIS SOLUTION IS
C                                          UNACCEPTABLE THEN INCREASING
C                                          SSZ TO A VERY LARGE VALUE
C                                          MAY HELP.  NOTE:  THIS CAN BE
C                                          EXPENSIVE.)
C
C                     THE ERRORS LISTED ABOVE ARE DEFINED AS RECOVERABLE
C                     ERRORS SHOULD THE USER WISH TO USE THEM IN THAT
C                     FASHION.  THE DOCUMENTATION ON THE ERPRT77 PACKAGE
C                     EXPLAINS HOW TO RECOVER FROM AN ERROR.
C
C NOTE:  THE COMMON BLOCKS LISTED INCLUDE ALL THE COMMON USED BY
C        THE ENTIRE CONRAN FAMILY.  NOT ALL MEMBERS WILL USE ALL
C        THE COMMON VARIABLES.
C
C   CONRA1
C       CL-ARRAY OF CONTOUR LEVELS
C       NCL-NUMBER OF CONTOUR LEVELS
C       OLDZ-Z VALUE OF LEFT NEIGHBOR TO CURRENT LOCATION
C       PV-ARRAY OF PREVIOUS ROW VALUES
C       HI-LARGEST CONTOUR PLOTTED
C       FLO-LOWEST CONTOUR PLOTTED
C       FINC-INCREMENT LEVEL BETWEEN EQUALLY SPACED CONTOURS
C   CONRA2
C       REPEAT-FLAG TO TRIANGULATE AND DRAW OR JUST DRAW
C       EXTRAP-PLOT DATA OUTSIDE OF CONVEX DATA HULL
C       PER-PUT PERIMETER AROUND PLOT
C       MESS-FLAG TO INDICATE MESSAGE OUTPUT
C       ISCALE-SCALING SWITCH
C       LOOK-PLOT TRIANGLES FLAG
C       PLDVLS-PLOT THE DATA VALUES FLAG
C       GRD-PLOT GRID FLAG
C       CON-USER SET OR PROGRAM SET CONTOURS FLAG
C       CINC-USER OR PROGRAM SET INCREMENT FLAG
C       CHILO-USER OR PROGRAM SET HI LOW CONTOURS
C       LABON-FLAG TO CONTROL LABELING OF CONTOURS
C       PMIMX-FLAG TO CONTROL THE PLOTTING OF MIN'S
C             AND MAX'S
C       SCALE-THE SCALE FACTOR FOR CONTOUR LINE VALUES
C             AND MIN, MAX PLOTTED VALUES
C       FRADV-ADVANCE FRAME BEFORE PLOTTING TRIANGULATION
C       EXTRI-ONLY PLOT TRIANGULATION
C       BPSIZ-BREAKPOINT SIZE FOR DASHPATTERNS
C       LISTOP-LIST OPTIONS ON UNIT6 FLAG
C   CONRA3
C       IRED-ERPRT77 RECOVERABLE ERROR FLAG
C   CONRA4
C       NCP-NUMBER OF DATA POINTS USED AT EACH POINT FOR
C           POLYNOMIAL CONSTRUCTION.
C       NCPSZ-MAX SIZE ALLOWED FOR NCP
C   CONRA5
C       NIT-FLAG TO INDICATE STATUS OF SEARCH DATA BASE
C       ITIPV-LAST TRIANGLE INTERPOLATION OCCURRED IN
C  CONRA6
C       XST-X COORDINATE START POINT FOR CONTOURING
C       YST-Y COORDINATE START POINT FOR CONTOURING
C       XED-X COORDINATE END POINT FOR CONTOURING
C       YED-Y COORDINATE END POINT FOR CONTOURING
C       STPSZ-STEP SIZE FOR X,Y CHANGE WHEN CONTOURING
C       IGRAD-NUMBER OF GRADUATIONS FOR CONTOURING (STEP SIZE)
C       IG-RESET VALUE FOR IGRAD
C       XRG-X RANGE OF COORDINATES
C       YRG-Y RANGE OF COORDINATES
C       BORD-PERCENT OF FRAME USED FOR CONTOUR PLOT
C       PXST-X PLOTTER START ADDRESS FOR CONTOURS
C       PYST-Y PLOTTER START ADDRESS FOR CONTOURS
C       PXED-X PLOTTER END ADDRESS FOR CONTOURS
C       PYED-Y PLOTTER END ADDRESS FOR CONTOURS
C       ITICK-NUMBER OF TICK MARKS FOR GRIDS AND PERIMETERS
C CONRA7
C       TITLE-SWITCH TO INDICATE IF TITLE OPTION ON OR OFF
C       ISTRNG-CHARACTER STRING CONTAINING THE TITLE
C       ICNT-CHARACTER COUNT OF ISTRNG
C       ITLSIZ-SIZE OF TITLE IN PWRIT UNITS
C CONRA8
C       IHIGH-DEFAULT INTENSITY SETTING
C       INMAJ-CONTOUR LEVEL INTENSITY FOR MAJOR LINES
C       INMIN-CONTOUR LEVEL INTENSITY FOR MINOR LINES
C       INLAB-TITLE AND MESSAGE INTENSITY
C       INDAT-DATA VALUE INTENSITY
C       FORM-THE FORMAT FOR PLOTTING THE DATA VALUES
C       LEN-THE NUMBER OF CHARACTERS IN THE FORMAT
C       IFMT-SIZE OF THE FORMAT FIELD
C       LEND-DEFAULT FORMAT LENGTH
C       IFMTD-DEFAULT FORMAT FIELD SIZE
C       ISIZEP-SIZE OF THE PLOTTED DATA VALUES
C  CONRA9
C       X-ARRAY OF X COORDINATES OF CONTOURS DRAWN AT CURRENT CONTOUR
C          LEVEL
C       Y-ARRAY OF Y COORDINATES OF CONTOURS DRAWN AT CURRENT CONTOUR
C          LEVEL
C       NP-COUNT IN X AND Y
C       MXXY-SIZE OF X AND Y
C       TR-TOP RIGHT CORNER VALUE OF CURRENT CELL
C       BR-BOTTOM RIGHT CORNER VALUE OF CURRENT CELL
C       TL-TOP LEFT CORNER VALUE OF CURRENT CELL
C       BL-BOTTOM LEFT CORNER VALUE OF CURRENT CELL
C       CONV-CURRENT CONTOUR VALUE
C       XN-X POSITION WHERE CONTOUR IS BEING DRAWN
C       YN-Y POSITION WHERE CONTOUR IS BEING DRAWN
C       ITLL-TRIANGLE WHERE TOP LEFT CORNER OF CURRENT CELL LIES
C       IBLL-TRIANGLE OF BOTTOM LEFT CORNER
C       ITRL-TRIANGLE OF TOP RIGHT CORNER
C       IBRL-TRIANGLE OF BOTTOM RIGHT CORNER
C       XC-X COORDINATE OF CURRENT CELL
C       YC-Y COORDINATE OF CURRENT CELL
C       ITLOC-IN CONJUNCTION WITH PV STORES THE TRIANGLE WHERE PV
C             VALUE CAME FROM
C CONR10
C       NT-NUMBER OF TRIANGLES GENERATED
C       NL-NUMBER OF LINE SEGMENTS
C       NTNL-NT+NL
C       JWIPT-POINTER INTO IWK WHERE WHERE TRIANGLE POINT NUMBERS
C             ARE STORED
C       JWIWL-IN IWK THE LOCATION OF A SCRATCH SPACE
C       JWIWP-IN IWK THE LOCATION OF A SCRATCH SPACE
C       JWIPL-IN IWK THE LOCATION OF END POINTS FOR BORDER LINE
C             SEGMENTS
C       IPR-IN WK THE LOCATION OF THE PARTIAL DERIVATIVES AT EACH
C           DATA POINT
C       ITPV-THE TRIANGLE WHERE THE PREVIOUS VALUE CAME FROM
C CONR11
C       NREP-NUMBER OF REPETITIONS OF DASH PATTERN BEFORE A LABEL
C       NCRT-NUMBER OF CRT UNITS FOR A DASH MARK OR BLANK
C       ISIZEL-SIZE OF CONTOUR LINE LABELS
C       NDASH-ARRAY CONTAINING THE NEGATIVE VALUED CONTOUR DASH
C             PATTERN
C       MINGAP-NUMBER OF UNLABELED LINES BETWEEN EACH LABELED ONE
C       IDASH-POSITIVE VALUED CONTOUR DASH PATTERN
C       ISIZEM-SIZE OF PLOTTED MINIMUMS AND MAXIMUMS
C       EDASH-EQUAL VALUED CONTOUR DASH PATTERN
C       TENS-DEFAULT TENSION SETTING FOR SMOOTHING
C CONR12
C       IXMAX,IYMAX-MAXIMUM X AND Y COORDINATES RELATIVE TO THE
C                 SCRATCH ARRAY, SCRARR
C       XMAX,YMAX-MAXIMUM X AND Y COORDINATES RELATIVE TO USERS
C                 COORDINATE SPACE
C CONR13
C       XVS-ARRAY OF THE X COORDINATES FOR SHIELDING
C       YVS-ARRAY OF THE Y COORDINATES FOR SHIELDING
C       IXVST-POINTER TO THE USERS X ARRAY FOR SHIELDING
C       IYVST-POINTER TO THE USERS Y ARRAY FOR SHIELDING
C       ICOUNT-COUNT OF THE SHIELD ELEMENTS
C       SPVAL-SPECIAL VALUE USED TO HALT CONTOURING AT THE SHIELD
C               BOUNDARY
C       SHIELD-LOGICAL FLAG TO SIGNAL STATUS OF SHIELDING
C       SLDPLT-LOGICAL FLAG TO INDICATE STATUS OF SHIELD PLOTTING
C CONR14
C       LINEAR-C1 LINEAR INTERPOLATING FLAG
C CONR15
C       ISTRNG-TITLE OF THE PLOT
C CONR16
C       FORM-FORMAT USED FOR DATA
C CONR17
C       NDASH-DASH PATTERN USED FOR CONTOUR LINES LESS THAN BP
C       IDASH-DASH PATTERN USED FOR CONTOUR LINES GREATER THAN BP
C       EDASH-DASH PATTERN USED FOR CONTOUR LINES EQUAL TO THE BP
C RANINT
C       IRANMJ-COLOR INDEX FOR NORMAL (MAJOR) INTENSITY LINES
C       IRANMN-COLOR INDEX FOR LOW INTENSITY LINES
C       IRANMJ-COLOR INDEX FOR TEXT (LABELS)
C
C +NOAO - Blockdata data conbdn rewritten as run time initialization
C         Variable LNGTHS not used.
C
C     EXTERNAL        CONBDN
C     DIMENSION       LNGTHS(4), HOLD(4)
      DIMENSION       HOLD(4)
C - NOAO
        CHARACTER*110   IWORK
        CHARACTER*13    ENCSCR,  ENSCRY
        CHARACTER*1     ICHAR
        CHARACTER*500   DPAT
        REAL            WIND(4),   VIEW(4),  NWIND(4), NVIEW(4)
      DIMENSION       XD(*)      ,YD(*)      ,ZD(*)      ,WK(*)      ,
     1                IWK(*)     ,SCRARR(*)
C
C
      COMMON /CONRA1/ CL(30)     ,NCL        ,OLDZ       ,PV(210)    ,
     1                FINC       ,HI         ,FLO
      COMMON /CONRA2/ REPEAT     ,EXTRAP     ,PER        ,MESS       ,
     1                ISCALE     ,LOOK       ,PLDVLS     ,GRD        ,
     2                CINC       ,CHILO      ,CON        ,LABON      ,
     3                PMIMX      ,SCALE      ,FRADV      ,EXTRI      ,
     4                BPSIZ      ,LISTOP
      COMMON /CONRA3/ IREC
      COMMON /CONRA4/ NCP        ,NCPSZ
      COMMON /CONRA5/ NIT        ,ITIPV
      COMMON /CONRA6/ XST        ,YST        ,XED        ,YED        ,
     1                STPSZ      ,IGRAD      ,IG         ,XRG        ,
     2                YRG        ,BORD       ,PXST       ,PYST       ,
     3                PXED       ,PYED       ,ITICK
      COMMON /CONRA7/ TITLE      ,ICNT   ,ITLSIZ
      COMMON /CONRA8/ IHIGH      ,INMAJ      ,INLAB      ,INDAT      ,
     1              LEN      ,IFMT       ,LEND       ,
     2                IFMTD      ,ISIZEP     ,INMIN
      COMMON /CONRA9/ ICOORD(500),NP         ,MXXY       ,TR         ,
     1                BR         ,TL         ,BL         ,CONV       ,
     2                XN         ,YN         ,ITLL       ,IBLL       ,
     3                ITRL       ,IBRL       ,XC         ,YC         ,
     4                ITLOC(210) ,JX         ,JY         ,ILOC       ,
     5                ISHFCT     ,XO         ,YO         ,IOC        ,NC
      COMMON /CONR10/ NT         ,NL         ,NTNL       ,JWIPT      ,
     1                JWIWL      ,JWIWP      ,JWIPL      ,IPR        ,
     2                ITPV
      COMMON /CONR11/ NREP       ,NCRT       ,ISIZEL     ,
     1                MINGAP     ,ISIZEM         ,
     2                TENS
      COMMON /CONR12/ IXMAX      ,IYMAX      ,XMAX       ,YMAX
      LOGICAL         REPEAT     ,EXTRAP     ,PER        ,MESS       ,
     1                LOOK       ,PLDVLS     ,GRD        ,LABON      ,
     2                PMIMX      ,FRADV      ,EXTRI      ,CINC       ,
     3                TITLE      ,LISTOP     ,CHILO      ,CON
      COMMON /CONR13/XVS(50),YVS(50),ICOUNT,SPVAL,SHIELD,
     1               SLDPLT
      LOGICAL SHIELD,SLDPLT
      COMMON /CONR14/LINEAR
      LOGICAL LINEAR
      COMMON /CONR15/ ISTRNG
        CHARACTER*64 ISTRNG
        COMMON /CONR16/ FORM
        CHARACTER*10 FORM
        COMMON /CONR17/ NDASH, IDASH, EDASH
        CHARACTER*10 NDASH, IDASH, EDASH
        COMMON /RANINT/ IRANMJ, IRANMN, IRANTX
        INTEGER OPLASF, OTXASF, LASF(13), OCOLI, OTEXCI
        SAVE
C
C
C+NOAO - Variable LNGTHS not used.
C       DATA LNGTHS(1),LNGTHS(2),LNGTHS(3),LNGTHS(4)/13,4,21,6/
C-NOAO
C
C ICONV CONVERT FORM 0-32767 TO 1-1024
C
        DATA ICONV/32/
C
C IABOVE AMOUNT TITLE IS PLACED ABOVE PLOT
C IBELOW, IBEL2 AMOUNT MESSAGE IS BELOW PLOT
C
C       DATA IABOVE,IBELOW,IBEL2/30,-30,-45/
C
C + NOAO - Label placement is improved by changed these values.  Also,
C          call the run time initialization subroutine, conbdn.
C
      iabove = 30
      ibelow = -15
      ibel2 = -30
      call conbdn
C - NOAO
C
C  THE FOLLOWING CALL IS FOR MONOTORING LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('NSSL','CONRAN','CONRAN','VERSION 01')
C
C  LIST THE OPTION VALUES IF REQUESTED
C
      IF (LISTOP) CALL CONOUT (2)
C
C  SET SWITCH TO MAP TRIANGLES, IN CONLOC, FOR QUICK SEARCHES
C
      NIT = 0
C
C  TEST TO SEE IF ENOUGH INPUT DATA
C
      IF (NDP.GE.NCP) GO TO    10
      CALL SETER (' CONRAN - INPUT PARAMETER NDP LESS THAN NCP',1,
     1             IREC)
      RETURN
C
   10 IF (NCPSZ.GE.NCP .AND. NCP.GE.2) GO TO    20
      CALL SETER (' CONRAN - NCP LT 2 OR GT NCPSZ',2,IREC)
C
   20 IWK(1) = NDP
      IWK(2) = NCP
      IWK(3) = 1
C
C  SET POLYLINE COLOR ASF TO INDIVIDUAL
C
        CALL GQASF(IERR,LASF)
        OPLASF = LASF(3)
        LASF(3) = 1
        OTXASF = LASF(10)
        LASF(10) = 1
        CALL GSASF(LASF)
C
C  INQUIRE CURRENT POLYLINE AND TEXT COLOR
C
        CALL GQPLCI(IERR,OCOLI)
        CALL GQTXCI(IERR,OTEXCI)
C
C  SET POLYLINE AND TEXT COLOR TO VALUE IN COMMON
C
        CALL GSPLCI(IRANMJ)
      CALL GSTXCI(IRANTX)
C
C CONSTRUCTION OF WORK SPACE POINTERS
C
C TRIANGLE POINT NUMBERS
C
      JWIPT = 16
C
C SCRATCH SPACE
C
      JWIWL = 6*NDP + 1
C
C  END POINTS OF BORDER LINE SEGMENTS AND TRIANGLE NUMBER
C
      JWIPL = 24*NDP + 1
C
C  POINT NUMBERS WHERE THE NCP DATA POINTS AROUND EACH POINT
C
      JWIPC = 27*NDP + 1
C
C  SCRATCH SPACE
C
      JWIWP = 30*NDP + 1
C
C  PARTIAL DERIVATIVES AT EACH DATA POINT
C
      IPR = 8*NDP + 1
C
C  TEST IF REPEAT (JUST NEW CONTOURS OF INTERPOLATED DATA)
C  OR NO REPEAT (TRIANGULATE AND CONTOUR)
C
      IF (REPEAT) GO TO    30
C
C TRIANGULATES THE X-Y PLANE.
C
      CALL CONTNG (NDP,XD,YD,NT,IWK(JWIPT),NL,IWK(JWIPL),IWK(JWIWL),
     1             IWK(JWIWP),WK)
      IF (NERRO(ITEMP).NE.0) RETURN
C
      IWK(5) = NT
      IWK(6) = NL
      NTNL = NT+NL
C
C  SKIP IF NOT LINEAR INTERPOLATION
C
      IF (.NOT.LINEAR) GO TO 25
C
C  FIND THE COEFICENTS FOR LINER INTERPOLATION OF EACH TRIANGLE
C
      CALL CONLIN(XD,YD,ZD,NT,IWK(JWIPT),WK(IPR))
      GO TO 35
C
C
C DETERMINES NCP POINTS CLOSEST TO EACH DATA POINT.
C
 25   CALL CONDET (NDP,XD,YD,NCP,IWK(JWIPC))
C
C  ESTIMATE THE PARTIAL DERIVATIVES AT ALL DATA POINTS
C
      CALL CONINT (NDP,XD,YD,ZD,NCP,IWK(JWIPC),WK(IPR))
C
C  VERIFY DATA VALUES VALID
C
   30 NT = IWK(5)
      NL = IWK(6)
      NTNL = NT+NL
C
C  COMPUTE STEP SIZE FOR CONTOURING
C
 35   CALL CONSTP (XD,YD,NDP)
C
C  SAVE ORIGINAL WINDOW, VIEWPORT OF TRANSFORMATION 1, AND ORIGINAL
C  LOG SCALING FLAG.
C
        CALL GQCNTN(IER,IOLDNT)
        CALL GQNT(IOLDNT,IER,WIND,VIEW)
        RX1 = VIEW(1)
        RX2 = VIEW(2)
        RY1 = VIEW(3)
        RY2 = VIEW(4)
C  SAVE NORMALIZATION TRANSFORMATION 1
        CALL GQNT(1,IER,WIND,VIEW)
        CALL GETUSV('LS',IOLLS)
C
C  DETERMINE SCALING OPTION
C
      ISC = ISCALE+1
      GO TO (   40,   60,   50),ISC
C
C  CONRAN SETS SCALING FACTOR
C
   40 CALL SET(PXST,PXED,PYST,PYED,XST,XED,YST,YED,1)
      GO TO 60
C
C  CONRAN PLOTS WITHIN USERS BOUNDARIES
C
   50 CALL SET(RX1,RX2,RY1,RY2,XST,XED,YST,YED,1)
C
C  IF TRIANGULATION PLOT ONLY BRANCH
C
   60 IF (EXTRI) GO TO   390
C
C  GENERATE CONTOURS IF NONE SUPPLIED BY USER
C
      CALL CONCLS (ZD,NDP)
      IF (NERRO(ITEMP).NE.0) RETURN
C
C  REORDER THE CONTOUR LINES FOR CORRECT PATTERN DISPLAY
C
      MAJLNS = 0
      IF (LABON) CALL CONREO (MAJLNS)
C
C  MAKE SURE INTEGER COORDINATES IN 1-1024 RANGE
C
      CALL SETUSV('XF',10)
        CALL SETUSV('YF',10)
C
C  SET THE DASH PATTERNS TO DEFAULT IF THEY HAVE NOT BEEN SET
C
C
      IF (IDASH(1:1).NE.' ') GO TO        80
C
C  SET POSITIVE CONTOUR VALUE TO DEFAULT
C
        IDASH = '$$$$$$$$$$'
   80 IF (NDASH(1:1).NE.' ') GO TO       100
C
C  SET NEGATIVE CONTOUR DASH PATTERN TO DEFAULT
C
        NDASH = '$$$$$$$$$$'
  100 IF (EDASH(1:1).NE.' ') GO TO   120
C
C  SET EQUAL CONTOUR DASH PATTERN TO DEFAULT
C
        EDASH = '$$$$$$$$$$'
C
C  INITIALIZE THE CONTOURING DATA STRUCTURE
C
  120 IF (.NOT.EXTRAP) YST = YST+STPSZ
C
C  LOAD THE SCRATCH SPACE
C
      CALL CONLOD (XD,YD,ZD,NDP,WK,IWK,SCRARR)
C
C  PERFORM SHIELDING IF SO REQUESTED
C
      IF (SHIELD) CALL CONSLD(SCRARR)
C
C  *******************************************************
C  *                                                     *
C  * IF THE USER NEEDS TO DIVIDE THE PROGRAM UP          *
C  * THIS IS THE BREAK POINT.  ALL SUBROUTINES CALLED    *
C  * PRIOR TO THIS MESSAGE ARE NOT USED AGAIN AND        *
C  * ALL ROUTINES AFTER THIS MESSAGE ARE NOT USED        *
C  * ANY EARLIER.  NOTE THIS ONLY REFEARS TO ENTRY POINTS*
C  * WHICH ARE PART OF THE CONRAN PACKAGE.               *
C  * ALL DATA STRUCTURES AND VARIABLES MUST BE RETAINED. *
C  *******************************************************
C
C
C  PLOT RELATIVE MINIMUMS AND MAXIMUMS IF REQUESTED
C
      IF (PMIMX) CALL CONPMM (SCRARR)
C
C
      LENDAS = NREP*10
C
C  SET THE ERROR MODE TO RECOVERY FOR THE CONTOURING STORAGE ERROR
C
      CALL ENTSR (IROLD,1)
C
C  DRAW THE CONTOURS
C
      DO   250 I=1,NCL
C
          CONV = CL(I)
          IF (CONV.GE.BPSIZ) GO TO   150
C
C  SET UP NEGATIVE CONTOUR PATTERN
C
              DO   140 J=1,10
                ICHAR = NDASH(J:J)
                  DO   130 K=1,NREP
                DPAT( J+( 10*(K-1) ): J+( 10*(K-1)) ) = ICHAR
  130             CONTINUE
  140         CONTINUE
          GO TO   210
C
C  SET UP POSITIVE CONTOUR DASH PATTERN
C
  150     IF (CONV.EQ.BPSIZ) GO TO   180
              DO   170 J=1,10
                ICHAR = IDASH(J:J)
                  DO   160 K=1,NREP
                  DPAT( J+( 10*(K-1) ): J+( 10*(K-1)) ) = ICHAR
  160             CONTINUE
  170         CONTINUE
          GO TO   210
C
C  SET UP EQUAL CONTOUR DASH PATTERN
C
  180         DO   200 J=1,10
                ICHAR = EDASH(J:J)
                  DO   190 K=1,NREP
                DPAT( J+( 10*(K-1) ): J+( 10*(K-1)) ) = ICHAR
  190             CONTINUE
  200         CONTINUE
C
  210     IF (I.GT.MAJLNS) GO TO   230
C
C  SET UP MAJOR LINES
C
          CALL GSPLCI (IRANMJ)
          CALL CONECD (CONV,IWORK,NCUSED)
          NCHAR = LENDAS + NCUSED
        DPAT(LENDAS+1:NCHAR) = IWORK(1:NCUSED)
          GO TO   240
C
C  SET UP MINOR LINES
C
  230     NCHAR = 10
      CALL GSPLCI (IRANMN)
C
C  PROCESS FOR ALL CONTOURS
C
  240     CALL DASHDC (DPAT(1:NCHAR),NCRT,ISIZEL)
C
C  DRAW ALL CONTOURS AT THIS LEVEL
C
          CALL CONDRW (SCRARR)
C
C  GET NEXT CONTOUR LEVEL
C
  250     CONTINUE
C
C  CONTOURING COMPLETED CHECK FOR OPTIONAL OUTPUTS ON PLOT
C
C  FIRST SET ERROR MODE BACK TO USERS VALUE
C
      CALL RETSR (IROLD)
C
C  GET PLOT BOUNDRIES FOR TITLING AND MESSAGE POSITIONING
C
      CALL GQCNTN(IER,ICN)
        CALL GQNT(ICN,IER,NWIND,NVIEW)
        XST = NWIND(1)
        XED = NWIND(2)
        YST = NWIND(3)
        YED = NWIND(4)
        CALL GETUSV('LS',LT)
C
C  RESET POLYLINE COLOR INDEX TO MAJOR (NORMAL)
C
        CALL GSPLCI (IRANMJ)
C
C  DRAW SHIELD ON PLOT IF REQUESTED
C
      IF(SLDPLT.AND.SHIELD) CALL CONDSD
C
C  DRAW PERIMETER ARROUND PLOT IF DESIRED
C
      IF (PER) CALL PERIM (ITICK,0,ITICK,0)
C
C  DRAW GRID IF REQUESTED
C
      IF (GRD) CALL GRID (ITICK,0,ITICK,0)
C
C  PLOT THE DATA VALUES IF REQUESTED
C
      IF (.NOT.PLDVLS) GO TO   260
      CALL CONPDV (XD,YD,ZD,NDP)
C
C  OUTPUT TITLE IF REQUESTED
C
  260 IF (.NOT.TITLE) GO TO   270
        CALL GSTXCI (IRANTX)
      CALL FL2INT (XED,YED,MX,MY)
      MY = (MY/ICONV)+IABOVE
        ILAST = 64
        DO 261 I = 64,1,-1
            IF (ISTRNG(I:I) .NE. ' ')THEN
                  ILAST = I + 1
                  GOTO 262
            ENDIF
  261 CONTINUE
  262 CONTINUE
C
C  POSITION STRINGS PROPERLY IF COORDS ARE IN PAU'S
C
      CALL GQCNTN(IER,ICN)
      CALL GSELNT(0)
      XC = ( NVIEW(1) + NVIEW(2)) / 2.
      YC = CPUY(MY)
      CALL WTSTR(XC,YC,ISTRNG(1:ILAST),ITLSIZ,0,0)
      CALL GSELNT(ICN)
C
C
C  OUTPUT MESSAGE IF REQUESTED
C
  270 IF (.NOT.MESS) GO TO   390
C
        CALL GSTXCI(IRANTX)
      CALL FL2INT (XST,YST,MX,MY)
      MY = (MY/ICONV)
C
C  IF PERIMETER OR GRID PUT OUT TICK INTERVAL
C
        IMSZ = 0
      IF (.NOT.PER .AND. .NOT.GRD) GO TO   300
        IWORK(1:36) = 'X INTERVAL=              Y INTERVAL='
C
C +NOAO - FTN internal writes rewritten as calls to encode.
C       WRITE(ENCSCR,'(G13.5)')XRG
C       WRITE(ENSCRY,'(G13.5)')YRG
        call encode (13, '(f13.5)', encscr, xrg)
	call encode (13, '(f13.5)', enscry, yrg)
C -NOAO
        IWORK(12:24) = ENCSCR
        IWORK(37:49) = ENSCRY
        IMSZ = 50
  300 IF (SCALE .EQ. 1.) GOTO 330
            IWORK(IMSZ:IMSZ+10) = ' SCALED BY '
C +NOAO
C           WRITE(ENCSCR,'(G13.5)')SCALE
            call encode (13, '(f13.5)', encscr, scale)
C -NOAO
            IWORK(IMSZ+11:IMSZ+23) = ENCSCR
            IMSZ = 73
  330 IF (IMSZ .NE. 0) THEN
        ILAST = IMSZ
        DO 291 I = IMSZ,1,-1
            IF (IWORK(I:I) .NE. ' ')THEN
                  ILAST = I + 1
                  GOTO 292
            ENDIF
  291 CONTINUE
  292 CONTINUE
C
C  POSITION STRINGS PROPERLY IF COORDS ARE IN PAU'S
C
      CALL GQCNTN(IER,ICN)
      CALL GSELNT(0)
      XC = ( NVIEW(1) + NVIEW(2)) / 2.
      YC = CPUY(MY+IBEL2)
      CALL WTSTR(XC,YC,IWORK(1:ILAST),8,0,0)
      CALL GSELNT(ICN)
        ENDIF
C
C  PRODUCE CONTOUR INFO
C
      IWORK(1:42) = 'CONTOUR FROM              TO              '
      IWORK(43:77) = 'CONTOUR INTERVAL OF                '
      HOLD(1) = FLO
      HOLD(2) = HI
      HOLD(3) = FINC
C
C +NOAO
C       WRITE(ENCSCR,'(G13.5)')HOLD(1)
        call encode (13, '(f13.5)', encscr, hold(1))
        IWORK(13:25) = ENCSCR
C       WRITE(ENCSCR,'(G13.5)')HOLD(2)
        call encode (13, '(f13.5)', encscr, hold(2))
        IWORK(29:41) = ENCSCR
C       WRITE(ENCSCR,'(G13.5)')HOLD(3)
        call encode (13, '(f13.5)', encscr, hold(3))
        IWORK(62:74) = ENCSCR
C -NOAO
C
C  IF IRREGULAR SPACED CONTOURS MODIFY CONTOUR INTERVAL STATEMENT
C
      IF (FINC.GE.0.) GO TO   380
      NC = 62
          IWORK(NC:NC+15) = ' IRREGULAR      '
C
        ILAST = 77
  380 DO 381 I = 77,1,-1
            IF (IWORK(I:I) .NE. ' ')THEN
                  ILAST = I + 1
                  GOTO 382
            ENDIF
  381 CONTINUE
  382 CONTINUE
C
C  POSITION STRINGS PROPERLY IF COORDS ARE IN PAU'S
C
      CALL GQCNTN(IER,ICN)
      CALL GSELNT(0)
      XC = ( NVIEW(1) + NVIEW(2)) / 2.
      YC = CPUY(MY+IBELOW)
      CALL WTSTR(XC,YC,IWORK(1:ILAST),8,0,0)
      CALL GSELNT(ICN)
C
C
C
C  PLOT TRIANGLES IF REQUESTED
C
  390 IF (LOOK) THEN
          CALL GSPLCI(IRANMN)
          CALL CONTLK (XD,YD,NDP,IWK(JWIPT))
          CALL GSPLCI(IRANMJ)
      ENDIF
C  RESTORE NORMALIZATION TRANSFORMATION 1 AND LOG SCALING
      IF (ISCALE .NE. 1) THEN
        CALL SET(VIEW(1),VIEW(2),VIEW(3),VIEW(4),
     -           WIND(1),WIND(2),WIND(3),WIND(4),IOLLS)
      ENDIF
C RESTORE ORIGINAL NORMALIZATION TRANSFORMATION NUMBER
      CALL GSELNT (IOLDNT)
C
C  RESTORE ORIGINAL COLOR
C
      CALL GSPLCI(OCOLI)
      CALL GSTXCI(OTEXCI)
C
C  RESTORE POLYLINE COLOR ASF TO WHAT IT WAS ON ENTRY TO GRIDAL
C
        LASF(10) = OTXASF
        LASF(3) = OPLASF
        CALL GSASF(LASF)
      RETURN
      END
      SUBROUTINE CONPMM (SCRARR)
C
C THIS ROUTINE FINDS RELATIVE MINIMUMS AND MAXIMUMS.  A RELATIVE MINIMUM
C (OR MAXIMUM) IS DEFINED TO BE THE LOWEST (OR HIGHEST) POINT WITHIN
C A CERTAIN NEIGHBORHOOD OF THE POINT.  THE NEIGHBORHOOD USED HERE
C IS + OR - IXRG IN THE X DIRECTION AND + OR - IYRG IN THE Y DIRECTION.
C
C
C
      COMMON /CONRA1/ CL(30)     ,NCL        ,OLDZ       ,PV(210)    ,
     1                FINC       ,HI         ,FLO
      COMMON /CONRA2/ REPEAT     ,EXTRAP     ,PER        ,MESS       ,
     1                ISCALE     ,LOOK       ,PLDVLS     ,GRD        ,
     2                CINC       ,CHILO      ,CON        ,LABON      ,
     3                PMIMX      ,SCALE      ,FRADV      ,EXTRI      ,
     4                BPSIZ      ,LISTOP
      COMMON /CONRA3/ IREC
      COMMON /CONRA4/ NCP        ,NCPSZ
      COMMON /CONRA5/ NIT        ,ITIPV
      COMMON /CONRA6/ XST        ,YST        ,XED        ,YED        ,
     1                STPSZ      ,IGRAD      ,IG         ,XRG        ,
     2                YRG        ,BORD       ,PXST       ,PYST       ,
     3                PXED       ,PYED       ,ITICK
      COMMON /CONRA7/ TITLE      ,ICNT   ,ITLSIZ
      COMMON /CONRA8/ IHIGH      ,INMAJ      ,INLAB      ,INDAT      ,
     1              LEN      ,IFMT       ,LEND       ,
     2                IFMTD      ,ISIZEP     ,INMIN
      COMMON /CONRA9/ ICOORD(500),NP         ,MXXY       ,TR         ,
     1                BR         ,TL         ,BL         ,CONV       ,
     2                XN         ,YN         ,ITLL       ,IBLL       ,
     3                ITRL       ,IBRL       ,XC         ,YC         ,
     4                ITLOC(210) ,JX         ,JY         ,ILOC       ,
     5                ISHFCT     ,XO         ,YO         ,IOC        ,NC
      COMMON /CONR10/ NT         ,NL         ,NTNL       ,JWIPT      ,
     1                JWIWL      ,JWIWP      ,JWIPL      ,IPR        ,
     2                ITPV
      COMMON /CONR11/ NREP       ,NCRT       ,ISIZEL     ,
     1                MINGAP     ,ISIZEM         ,
     2                TENS
      COMMON /CONR12/ IXMAX      ,IYMAX      ,XMAX       ,YMAX
      LOGICAL         REPEAT     ,EXTRAP     ,PER        ,MESS       ,
     1                LOOK       ,PLDVLS     ,GRD        ,LABON      ,
     2                PMIMX      ,FRADV      ,EXTRI      ,CINC       ,
     3                TITLE      ,LISTOP     ,CHILO      ,CON
      COMMON /CONR15/ ISTRNG
        CHARACTER*64 ISTRNG
        COMMON /CONR16/ FORM
        CHARACTER*10 FORM
        COMMON /CONR17/ NDASH, IDASH, EDASH
        CHARACTER*10 NDASH, IDASH, EDASH
C
C
C
      DIMENSION       SCRARR(*)
      CHARACTER*10    IA
      SAVE
C
C  CONVERT FROM 0-32767 TO 1-1024
C
        DATA ICONV/32/
C
C  ACCESSING FUNCTION INTO SCRARR
C
      SCRTCH(IXX,IYY) = SCRARR(IYY+(IXX-1)*IYMAX)
C
C  GRAPHICS MAPPING FUNCTIONS
C
      FX(XXX,YYY) = XXX
      FY(XXX,YYY) = YYY
C
C  MAPPING FROM INTEGER TO USER INPUT FLOATING POINT
C
      CONVX(IXX) = XST + FLOAT(IXX-1)*STPSZ
      CONVY(IYY) = YST + FLOAT(IYY-1)*STPSZ
C
C  SET INTENSITY TO HIGH
C
        IF (INDAT .NE. 1) THEN
          CALL GSTXCI (INDAT)
      ELSE
            CALL GSTXCI (IRANTX)
        ENDIF
C
C  COMPUTE THE SEARCH RANGE FOR MIN AND MAX DETERMINATION
C
      IXRG = MIN0(15,MAX0(2,IFIX(FLOAT(IXMAX)/8.)))
      IYRG = MIN0(15,MAX0(2,IFIX(FLOAT(IYMAX)/8.)))
C
C  LOOP THROUGH ALL ROWS OF THE DATA SEARCHING FOR AN IMMEDIATE MIN OR
C  MAX.
C
      IX = 1
C
C  SCAN A ROW
C
C  IF EXTRAPOLATING DONT LIMIT ROW SCANS
C
   10 IF (.NOT.EXTRAP) GO TO    20
      IYST = 1
      IYED = IYMAX
      IY = 1
      GO TO    30
C
C  NOT EXTRAPOLATING STAY IN HULL BOUNDRIES
C
   20 IYST = ITLOC(IX*2-1)
      IYED = ITLOC(IX*2)
      IF (IYST.EQ.0) GO TO   240
      IY = IYST
   30 VAL = SCRTCH(IX,IY)
C
C  SEARCH FOR A MIN
C
C
C  BRANCH IF NOT FIRST ON A ROW
C
      IF (IY.NE.IYST) GO TO    40
      IF (VAL.GE.SCRTCH(IX,IY+1)) GO TO   130
      IF (VAL.GE.SCRTCH(IX,IY+2)) GO TO   130
      GO TO    60
C
C  BRANCH IF NOT LAST ON ROW
C
   40 IF (IY.NE.IYED) GO TO    50
      IF (VAL.GE.SCRTCH(IX,IY-1)) GO TO   140
      IF (VAL.GE.SCRTCH(IX,IY-2)) GO TO   140
      GO TO    60
C
C  IN MIDDLE OF ROW
C
   50 IF (VAL.GE.SCRTCH(IX,IY+1)) GO TO   150
      IF (VAL.GE.SCRTCH(IX,IY-1)) GO TO   150
C
C  POSSIBLE MIN FOUND SEARCH NEIGHBORHOOD
C
   60 IXST = MAX0(1,IX-IXRG)
      IXSTOP = MIN0(IXMAX,IX+IXRG)
C
C IF NOT EXTRAPOLATING BRANCH
C
   70 IF (.NOT.EXTRAP) GO TO    80
      IYSRS = 1
      IYSRE = IYMAX
      GO TO    90
C
C  NOT EXTRAPOLATING STAY IN CONVEX HULL
C
   80 IYSRS = ITLOC(IXST*2-1)
      IYSRE = ITLOC(IXST*2)
      IF (IYSRS.EQ.0) GO TO   120
C
   90 IYSRS = MAX0(IYSRS,IY-IYRG)
      IYSRE = MIN0(IYSRE,IY+IYRG)
C
  100 CUR = SCRTCH(IXST,IYSRS)
      IF (VAL.LT.CUR) GO TO   110
      IF (VAL.GT.CUR) GO TO   230
      IF (IX.EQ.IXST .AND. IY.EQ.IYSRS) GO TO   110
      GO TO   230
C
C  SUCCESS SO FAR TRY NEXT SPACE
C
  110 IYSRS = IYSRS+1
      IF (IYSRS.LE.IYSRE) GO TO   100
  120 IXST = IXST+1
      IF (IXST.LE.IXSTOP) GO TO    70
C
C  SUCCESS, WE HAVE FOUND A RELATIVE MIN
C
      X = CONVX(IX)
      Y = CONVY(IY)
      X1 = FX(X,Y)
      CALL FL2INT (X1,FY(X,Y),MX,MY)
      MX = MX/ICONV
      MY = MY/ICONV
C
C  POSITION STRINGS PROPERLY IF COORDS ARE IN PAU'S
C
      CALL GQCNTN(IER,ICN)
      CALL GSELNT(0)
      XC = CPUX(MX)
      YC = CPUY(MY)
      CALL WTSTR(XC,YC,'L',ISIZEM,0,0)
      CALL GSELNT(ICN)
C
      CALL CONECD (VAL,IA,NC)
      MY = MY - 2*ISIZEM
C
C  POSITION STRINGS PROPERLY IF COORDS ARE IN PAU'S
C
      CALL GQCNTN(IER,ICN)
      CALL GSELNT(0)
      YC = CPUY(MY)
      CALL WTSTR(XC,YC,IA(1:NC),ISIZEM,0,0)
      CALL GSELNT(ICN)
C
      GO TO   230
C
C  SEARCH FOR A LOCAL MAXIMUM
C
C  IF FIRST LOC ON A ROW
C
  130 IF (VAL.LE.SCRTCH(IX,IY+1)) GO TO   230
      IF (VAL.LE.SCRTCH(IX,IY+2)) GO TO   230
      GO TO   160
C
C  IF LAST ON ROW
C
  140 IF (VAL.LE.SCRTCH(IX,IY-1)) GO TO   230
      IF (VAL.LE.SCRTCH(IX,IY-2)) GO TO   230
      GO TO   160
C
C  IN MIDDLE OF ROW
C
  150 IF (VAL.LE.SCRTCH(IX,IY+1)) GO TO   230
      IF (VAL.LE.SCRTCH(IX,IY-1)) GO TO   230
C
C  POSSIBLE MIN FOUND SEARCH NEIGHBORHOOD
C
  160 IXST = MAX0(1,IX-IXRG)
      IXSTOP = MIN0(IXMAX,IX+IXRG)
  170 IF (.NOT.EXTRAP) GO TO   180
      IYSRS = 1
      IYSRE = IYMAX
      GO TO   190
C
C  NOT EXTRAPOLATING STAY IN CONVEX HULL
C
  180 IYSRS = ITLOC(IXST*2-1)
      IYSRE = ITLOC(IXST*2)
      IF (IYSRS.EQ.0) GO TO   220
C
  190 IYSRS = MAX0(IYSRS,IY-IYRG)
      IYSRE = MIN0(IYSRE,IY+IYRG)
C
  200 CUR = SCRTCH(IXST,IYSRS)
      IF (VAL.GT.CUR) GO TO   210
      IF (VAL.LT.CUR) GO TO   230
      IF (IX.EQ.IXST .AND. IY.EQ.IYSRS) GO TO   210
      GO TO   230
C
C  SUCCESS SO FAR TRY NEXT SPACE
C
  210 IYSRS = IYSRS+1
      IF (IYSRS.LE.IYSRE) GO TO   200
  220 IXST = IXST+1
      IF (IXST.LE.IXSTOP) GO TO   170
C
C  SUCCESS WE HAVE A MAXIMUM
C
      X = CONVX(IX)
      Y = CONVY(IY)
      X1 = FX(X,Y)
      CALL FL2INT (X1,FY(X,Y),MX,MY)
      MX = MX/ICONV
      MY = MY/ICONV
C
C  POSITION STRINGS PROPERLY IF COORDS ARE IN PAU'S
C
      CALL GQCNTN(IER,ICN)
      CALL GSELNT(0)
      XC = CPUX(MX)
      YC = CPUY(MY)
      CALL WTSTR(XC,YC,'H',ISIZEM,0,0)
      CALL GSELNT(ICN)
C
      CALL CONECD (VAL,IA,NC)
      MY = MY - 2*ISIZEM
C
C  POSITION STRINGS PROPERLY IF COORDS ARE IN PAU'S
C
      CALL GQCNTN(IER,ICN)
      CALL GSELNT(0)
      YC = CPUY(MY)
      CALL WTSTR(XC,YC,IA(1:NC),ISIZEM,0,0)
      CALL GSELNT(ICN)
C
C  END OF SEARCH AT THIS LOCATION TRY NEXT
C
  230 IY = IY+1
      IF (IY.LE.IYED) GO TO    30
  240 IX = IX+1
      IF (IX.LE.IXMAX) GO TO    10
C
      CALL GSTXCI (IRANTX)
C
      RETURN
C
C******************************************************************
C*                                                                *
C*                   REVISION HISTORY                             *
C*                                                                *
C*  JUNE 1980   ADDED CONRAN TO ULIB                              *
C*  AUGUST 1980 CHANGED ACCESS CARD DOCUMENTATION                 *
C*  DECEMBER 1980 MODIFIED COMMENT CARD DOCUMENTATION             *
C*  MARCH 1983  ADDED ASPECT RATIO ERROR                          *
C*  JULY 1983  ADDED SHIELDING AND LINEAR INTERPOLATION           *
C*             REMOVED 7600 ACCESS CARDS                          *
C*  JULY 1984  CONVERTED TO STANDARD FORTRAN77 AND GKS        *
C*                                                                *
C******************************************************************
C
      END
