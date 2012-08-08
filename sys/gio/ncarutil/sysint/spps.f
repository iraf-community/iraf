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
      FUNCTION CFUX (RX)
C
C Given an x coordinate RX in the fractional system, CFUX(RX) is an x
C coordinate in the user system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      CALL GQNT (NT,IE,WD,VP)
      I=1
      IF (MI.GE.3) I=2
      CFUX=WD(I)+(RX-VP(1))/(VP(2)-VP(1))*(WD(3-I)-WD(I))
      IF (LL.GE.3) CFUX=10.**CFUX
      RETURN
      END
      FUNCTION CFUY (RY)
C
C Given a y coordinate RY in the fractional system, CFUY(RY) is a y
C coordinate in the user system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      CALL GQNT (NT,IE,WD,VP)
      I=3
      IF (MI.EQ.2.OR.MI.GE.4) I=4
      CFUY=WD(I)+(RY-VP(3))/(VP(4)-VP(3))*(WD(7-I)-WD(I))
      IF (LL.EQ.2.OR.LL.GE.4) CFUY=10.**CFUY
      RETURN
      END
      FUNCTION CMFX (IX)
C
C Given an x coordinate IX in the metacode system, CMFX(IX) is an x
C coordinate in the fractional system.
C
      CMFX=FLOAT(IX)/32767.
      RETURN
      END
      FUNCTION CMFY (IY)
C
C Given a y coordinate IY in the metacode system, CMFY(IY) is a y
C coordinate in the fractional system.
C
      CMFY=FLOAT(IY)/32767.
      RETURN
      END
      FUNCTION CMUX (IX)
C
C Given an x coordinate IX in the metacode system, CMUX(IX) is an x
C coordinate in the user system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      CALL GQNT (NT,IE,WD,VP)
      I=1
      IF (MI.GE.3) I=2
      CMUX=WD(I)+(FLOAT(IX)/32767.-VP(1))/(VP(2)-VP(1))*(WD(3-I)-WD(I))
      IF (LL.GE.3) CMUX=10.**CMUX
      RETURN
      END
      FUNCTION CMUY (IY)
C
C Given a y coordinate IY in the metacode system, CMUY(IY) is a y
C coordinate in the user system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      CALL GQNT (NT,IE,WD,VP)
      I=3
      IF (MI.EQ.2.OR.MI.GE.4) I=4
      CMUY=WD(I)+(FLOAT(IY)/32767.-VP(3))/(VP(4)-VP(3))*(WD(7-I)-WD(I))
      IF (LL.EQ.2.OR.LL.GE.4) CMUY=10.**CMUY
      RETURN
      END
      FUNCTION CPFX (IX)
C
C Given an x coordinate IX in the plotter system, CPFX(IX) is an x
C coordinate in the fractional system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      CPFX=FLOAT(IX-1)/(2.**MX-1.)
      RETURN
      END
      FUNCTION CPFY (IY)
C
C Given a y coordinate IY in the plotter system, CPFY(IY) is a y
C coordinate in the fractional system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      CPFY=FLOAT(IY-1)/(2.**MY-1.)
      RETURN
      END
      FUNCTION CPUX (IX)
C
C Given an x coordinate IX in the plotter system, CPUX(IX) is an x
C coordinate in the user system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      CALL GQNT (NT,IE,WD,VP)
      I=1
      IF (MI.GE.3) I=2
      CPUX=WD(I)+(FLOAT(IX-1)/(2.**MX-1.)-VP(1))/(VP(2)-VP(1))*
     +           (WD(3-I)-WD(I))
      IF (LL.GE.3) CPUX=10.**CPUX
      RETURN
      END
      FUNCTION CPUY (IY)
C
C Given a y coordinate IY in the plotter system, CPUY(IY) is a y
C coordinate in the user system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      CALL GQNT (NT,IE,WD,VP)
      I=3
      IF (MI.EQ.2.OR.MI.GE.4) I=4
      CPUY=WD(I)+(FLOAT(IY-1)/(2.**MY-1.)-VP(3))/(VP(4)-VP(3))*
     +     (WD(7-I)-WD(I))
      IF (LL.EQ.2.OR.LL.GE.4) CPUY=10.**CPUY
      RETURN
      END
      FUNCTION CUFX (RX)
C
C Given an x coordinate RX in the user system, CUFX(RX) is an x
C coordinate in the fractional system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      CALL GQNT (NT,IE,WD,VP)
      I=1
      IF (MI.GE.3) I=2
      IF (LL.LE.2) THEN
        CUFX=(RX-WD(I))/(WD(3-I)-WD(I))*(VP(2)-VP(1))+VP(1)
      ELSE
        CUFX=(ALOG10(RX)-WD(I))/(WD(3-I)-WD(I))*(VP(2)-VP(1))+VP(1)
      ENDIF
      RETURN
      END
      FUNCTION CUFY (RY)
C
C Given a y coordinate RY in the user system, CUFY(RY) is a y
C coordinate in the fractional system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      CALL GQNT (NT,IE,WD,VP)
      I=3
      IF (MI.EQ.2.OR.MI.GE.4) I=4
      IF (LL.LE.1.OR.LL.EQ.3) THEN
        CUFY=(RY-WD(I))/(WD(7-I)-WD(I))*(VP(4)-VP(3))+VP(3)
      ELSE
        CUFY=(ALOG10(RY)-WD(I))/(WD(7-I)-WD(I))*(VP(4)-VP(3))+VP(3)
      ENDIF
      RETURN
      END
      FUNCTION KFMX (RX)
C
C Given an x coordinate RX in the fractional system, KFMX(RX) is an x
C coordinate in the metacode system.
C
      KFMX=IFIX(RX*32767.)
      RETURN
      END
      FUNCTION KFMY (RY)
C
C Given a y coordinate RY in the fractional system, KFMY(RY) is a y
C coordinate in the metacode system.
C
      KFMY=IFIX(RY*32767.)
      RETURN
      END
      FUNCTION KFPX (RX)
C
C Given an x coordinate RX in the fractional system, KFPX(RX) is an x
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      KFPX=1+IFIX(RX*(2.**MX-1.))
      RETURN
      END
      FUNCTION KFPY (RY)
C
C Given a y coordinate RY in the fractional system, KFPY(RY) is a y
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      KFPY=1+IFIX(RY*(2.**MX-1.))
      RETURN
      END
      FUNCTION KMPX (IX)
C
C Given an x coordinate IX in the metacode system, KMPX(IX) is an x
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      KMPX=1+IFIX((2.**MX-1.)*FLOAT(IX)/32767.)
      RETURN
      END
      FUNCTION KMPY (IY)
C
C Given a y coordinate IY in the metacode system, KMPY(IY) is a y
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      KMPY=1+IFIX((2.**MY-1.)*FLOAT(IY)/32767.)
      RETURN
      END
      FUNCTION KPMX (IX)
C
C Given an x coordinate IX in the plotter system, KPMX(IX) is an x
C coordinate in the metacode system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      KPMX=IFIX(32767.*FLOAT(IX-1)/(2.**MX-1.))
      RETURN
      END
      FUNCTION KPMY (IY)
C
C Given a y coordinate IY in the plotter system, KPMY(IY) is a y
C coordinate in the metacode system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      KPMY=IFIX(32767.*FLOAT(IY-1)/(2.**MY-1.))
      RETURN
      END
      FUNCTION KUMX (RX)
C
C Given an x coordinate RX in the user system, KUMX(RX) is an x
C coordinate in the metacode system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      CALL GQNT (NT,IE,WD,VP)
      I=1
      IF (MI.GE.3) I=2
      IF (LL.LE.2) THEN
        KUMX=IFIX(((RX-WD(I))/(WD(3-I)-WD(I))*(VP(2)-VP(1))+VP(1))*
     +              32767.)
      ELSE
        KUMX=IFIX(((ALOG10(RX)-WD(I))/(WD(3-I)-WD(I))*(VP(2)-VP(1))+
     +              VP(1))*32767.)
      ENDIF
      RETURN
      END
      FUNCTION KUMY (RY)
C
C Given a y coordinate RY in the user system, KUMY(RY) is a y
C coordinate in the metacode system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      CALL GQNT (NT,IE,WD,VP)
      I=3
      IF (MI.EQ.2.OR.MI.GE.4) I=4
      IF (LL.LE.1.OR.LL.EQ.3) THEN
        KUMY=IFIX(((RY-WD(I))/(WD(7-I)-WD(I))*(VP(4)-VP(3))+VP(3))*
     +              32767.)
      ELSE
        KUMY=IFIX(((ALOG10(RY)-WD(I))/(WD(7-I)-WD(I))*(VP(4)-VP(3))+
     +              VP(3))*32767.)
      ENDIF
      RETURN
      END
      FUNCTION KUPX (RX)
C
C Given an x coordinate RX in the user system, KUPX(RX) is an x
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      CALL GQNT (NT,IE,WD,VP)
      I=1
      IF (MI.GE.3) I=2
      IF (LL.LE.2) THEN
        KUPX=1+IFIX(((RX-WD(I))/(WD(3-I)-WD(I))*(VP(2)-VP(1))+VP(1))*
     +              (2.**MX-1.))
      ELSE
        KUPX=1+IFIX(((ALOG10(RX)-WD(I))/(WD(3-I)-WD(I))*(VP(2)-VP(1))+
     +              VP(1))*(2.**MX-1.))
      ENDIF
      RETURN
      END
      FUNCTION KUPY (RY)
C
C Given a y coordinate RY in the user system, KUPY(RY) is a y
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      CALL GQNT (NT,IE,WD,VP)
      I=3
      IF (MI.EQ.2.OR.MI.GE.4) I=4
      IF (LL.LE.1.OR.LL.EQ.3) THEN
        KUPY=1+IFIX(((RY-WD(I))/(WD(7-I)-WD(I))*(VP(4)-VP(3))+VP(3))*
     +              (2.**MY-1.))
      ELSE
        KUPY=1+IFIX(((ALOG10(RY)-WD(I))/(WD(7-I)-WD(I))*(VP(4)-VP(3))+
     +              VP(3))*(2.**MY-1.))
      ENDIF
      RETURN
      END
      SUBROUTINE CLSGKS
C
C IU(6), in IUTLCM, is the current metacode unit number.
C
      COMMON /IUTLCM/ IU(100)
C
C Deactivate the metacode workstation, close the workstation, and
C close GKS.
C
      CALL GDAWK (IU(6))
      CALL GCLWK (IU(6))
      CALL GCLKS
C
      RETURN
C
      END
      SUBROUTINE CURVE (PX,PY,NP)
C
      DIMENSION PX(NP),PY(NP)
C
C CURVE draws the curve defined by the points (PX(I),PY(I)), for I = 1
C to NP.  All coordinates are stated in the user coordinate system.
C
C Define arrays to hold converted point coordinates when it becomes
C necessary to draw the curve piecewise.
C
      DIMENSION QX(10),QY(10)
C
C If NP is less than or equal to zero, there's nothing to do.
C
      IF (NP.LE.0) RETURN
C
C If NP is exactly equal to 1, just draw a point.
C
      IF (NP.EQ.1) THEN
        CALL POINT (PX(1),PY(1))
C
C Otherwise, draw the curve.
C
      ELSE
C
C Flush the pen-move buffer.
C
        CALL PLOTIF (0.,0.,2)
C
C Save the current SET parameters.
C
        CALL GETSET (F1,F2,F3,F4,F5,F6,F7,F8,LL)
C
C If the mapping defined by the last SET call was non-reversed and
C linear in both x and y, a single polyline will suffice.
C
        IF (F5.LT.F6.AND.F7.LT.F8.AND.LL.EQ.1) THEN
          CALL GPL (NP,PX,PY)
C
C Otherwise, piece the line together out of smaller chunks, converting
C the coordinates for each chunk as directed by the last SET call.
C
        ELSE
          DO 102 IP=1,NP,9
            NQ=MIN0(10,NP-IP+1)
            IF (NQ.GE.2) THEN
              DO 101 IQ=1,NQ
                QX(IQ)=CUFX(PX(IP+IQ-1))
                QY(IQ)=CUFY(PY(IP+IQ-1))
  101         CONTINUE
              CALL SET (F1,F2,F3,F4,F1,F2,F3,F4,1)
              CALL GPL (NQ,QX,QY)
              CALL SET (F1,F2,F3,F4,F5,F6,F7,F8,LL)
            END IF
  102     CONTINUE
        END IF
C
C Update the pen position.
C
        CALL FRSTPT (PX(NP),PY(NP))
C
      END IF
C
C Done.
C
      RETURN
C
      END
      SUBROUTINE FL2INT (PX,PY,IX,IY)
C
C Given the user coordinates PX and PY of a point, FL2INT returns the
C metacode coordinates IX and IY of that point.
C
C Declare the common block containing the user state variables LL, MI,
C MX, and MY.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
C
C Declare arrays in which to retrieve the variables defining the current
C window and viewport.
C
      DIMENSION WD(4),VP(4)
C
C Get the variables defining the current window and viewport.
C
      CALL GQCNTN (IE,NT)
      CALL GQNT (NT,IE,WD,VP)
C
C Compute IX.
C
      I=1
      IF (MI.GE.3) I=2
      IF (LL.LE.2) THEN
        IX=IFIX(((PX-WD(I))/(WD(3-I)-WD(I))*(VP(2)-VP(1))+VP(1))*32767.)
      ELSE
        IX=IFIX(((ALOG10(PX)-WD(I))/(WD(3-I)-WD(I))*
     +                              (VP(2)-VP(1))+VP(1))*32767.)
      ENDIF
C
C Compute IY.
C
      I=3
      IF (MI.EQ.2.OR.MI.GE.4) I=4
      IF (LL.LE.1.OR.LL.EQ.3) THEN
        IY=IFIX(((PY-WD(I))/(WD(7-I)-WD(I))*(VP(4)-VP(3))+VP(3))*32767.)
      ELSE
        IY=IFIX(((ALOG10(PY)-WD(I))/(WD(7-I)-WD(I))*
     +                              (VP(4)-VP(3))+VP(3))*32767.)
      ENDIF
C
C Done.
C
      RETURN
C
      END
C
C +NOAO - name conflict
C
C     SUBROUTINE FLUSH
      subroutine mcflsh
C
C - NOAO
C
C FLUSH currently does nothing except flush the pen-move buffer.
C
      CALL PLOTIF (0.,0.,2)
C
C Done.
C
      RETURN
C
      END
      SUBROUTINE FRAME
C
C FRAME is intended to advance to a new frame.  The GKS version clears
C all open workstations.
C
C First, flush the pen-move buffer.
C
      CALL PLOTIF (0.,0.,2)
C
C +NOAO - Initialize utilbd 'first' flag for next plot
      call initut
C 
C - NOAO
C Get the number of open workstations.  If there are none, we're done.
C
      CALL GQOPWK (0,IE,NO,ID)
      IF (NO.EQ.0) RETURN
C
C Otherwise, clear the open workstations.
C
      DO 101 I=1,NO
        CALL GQOPWK (I,IE,NO,ID)
        CALL GCLRWK (ID,1)
  101 CONTINUE
C
C Done.
C
      RETURN
C
      END
      SUBROUTINE FRSTPT (PX,PY)
C
C Given the user coordinates PX and PY of a point, FRSTPT generates a
C pen-up move to that point.
C
      CALL PLOTIF (CUFX(PX),CUFY(PY),0)
C
C Done.
C
      RETURN
C
      END
      SUBROUTINE GETSET (VL,VR,VB,VT,WL,WR,WB,WT,LF)
C
C GETSET returns to its caller the current values of the parameters
C defining the mapping from the user system to the fractional system
C (in GKS terminology, the mapping from world coordinates to normalized
C device coordinates).
C
C VL, VR, VB, and VT define the viewport (in the fractional system), WL,
C WR, WB, and WT the window (in the user system), and LF the nature of
C the mapping, according to the following table:
C
C    1  -  x linear, y linear
C    2  -  x linear, y logarithmic
C    3  -  x logarithmic, y linear
C    4  -  x logarithmic, y logarithmic
C
C Declare the common block containing the linear-log and mirror-imaging
C flags.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
C
C Define variables to receive the GKS viewport and window.
C
      DIMENSION VP(4),WD(4)
C
C Retrieve the number of the current GKS normalization transformation.
C
      CALL GQCNTN (IE,NT)
C
C Retrieve the definition of that normalization transformation.
C
      CALL GQNT (NT,IE,WD,VP)
C
C Pass the viewport definition to the caller.
C
      VL=VP(1)
      VR=VP(2)
      VB=VP(3)
      VT=VP(4)
C
C Pass the linear/log flag and a (possibly modified) window definition
C to the caller.
C
      LF=LL
C
      IF (LL.EQ.1.OR.LL.EQ.2) THEN
        WL=WD(1)
        WR=WD(2)
      ELSE
        WL=10.**WD(1)
        WR=10.**WD(2)
      END IF
C
      IF (MI.GE.3) THEN
        WW=WL
        WL=WR
        WR=WW
      END IF
C
      IF (LL.EQ.1.OR.LL.EQ.3) THEN
        WB=WD(3)
        WT=WD(4)
      ELSE
        WB=10.**WD(3)
        WT=10.**WD(4)
      END IF
C
      IF (MI.EQ.2.OR.MI.GE.4) THEN
        WW=WB
        WB=WT
        WT=WW
      END IF
C
      RETURN
C
      END
      SUBROUTINE GETSI (IX,IY)
C
C Return to the user the parameters which determine the assumed size of
C the target plotter and therefore determine how user coordinates are
C to be mapped into plotter coordinates.
C
C Declare the common block containing the scaling information.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
C
C Set the user variables.

      IX=MX
      IY=MY
C
      RETURN
C
      END
      SUBROUTINE GETUSV (VN,IV)
      CHARACTER*(*) VN
C
C This subroutine retrieves the current values of the utility state
C variables.  VN is the character name of the variable and IV is
C its value.
C
C The labelled common block IUTLCM contains all of the utility state
C variables.
C
      COMMON /IUTLCM/IU(100)
C
C Check for the linear-log scaling variable.
C
      IF (VN(1:2).EQ.'LS') THEN
        IV=IU(1)
C
C Check for the variable specifying the mirror-imaging of the axes.
C
      ELSE IF (VN(1:2).EQ.'MI') THEN
        IV=IU(2)
C
C Check for the variable specifying the resolution of the plotter in x.
C
      ELSE IF (VN(1:2).EQ.'XF') THEN
        IV=IU(3)
C
C Check for the variable specifying the resolution of the plotter in x.
C
      ELSE IF (VN(1:2).EQ.'YF') THEN
        IV=IU(4)
C
C Check for the variable specifying the size of the pen-move buffer.
C
      ELSE IF (VN(1:2).EQ.'PB') THEN
        IV=IU(5)
C
C Check for the variable specifying the metacode unit.
C
      ELSE IF (VN(1:2).EQ.'MU') THEN
        IV=IU(6)
C
C Check for one of the variables specifying color and intensity.
C
      ELSE IF (VN(1:2).EQ.'IR') THEN
        IV=IU(7)
C
      ELSE IF (VN(1:2).EQ.'IG') THEN
        IV=IU(8)
C
      ELSE IF (VN(1:2).EQ.'IB') THEN
        IV=IU(9)
C
      ELSE IF (VN(1:2).EQ.'IN') THEN
        IV=IU(10)
C
C Check for the variable specifying the current color index.
C
      ELSE IF (VN(1:2).EQ.'II') THEN
        IV=IU(11)
C
C Check for the variable specifying the maximum color index.
C
      ELSE IF (VN(1:2).EQ.'IM') THEN
        IV=IU(12)
C
C Check for the variable specifying the line width scale factor.
C
      ELSE IF (VN(1:2).EQ.'LW') THEN
        IV=IU(13)
C
C Check for the variable specifying the marker size scale factor.
C
      ELSE IF (VN(1:2).EQ.'MS') THEN
        IV=IU(14)
C
C Otherwise, the variable name is unknown.
C
      ELSE
        CALL SETER ('GETUSV - UNKNOWN VARIABLE NAME IN CALL',1,2)
C
      ENDIF
C
      RETURN
C
      END
      SUBROUTINE LINE (X1,Y1,X2,Y2)
C
C Draw a line connecting the point (X1,Y1) to the point (X2,Y2), in the
C user coordinate system.
C
      CALL PLOTIF (CUFX(X1),CUFY(Y1),0)
      CALL PLOTIF (CUFX(X2),CUFY(Y2),1)
      RETURN
      END
      SUBROUTINE MXMY (IX,IY)
C
C Return to the user the coordinates of the current pen position, in the
C plotter coordinate system.
C
C In the common block PLTCM are recorded the coordinates of the last
C pen position, in the metacode coordinate system.
C
      COMMON /PLTCM/ JX,JY
C
C Declare the common block containing the user state variables LL, MI,
C MX, and MY.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
C
C Return to the user the plotter-system equivalents of the values in
C the metacode system.
C
      IX=1+IFIX((2.**MX-1.)*FLOAT(JX)/32767.)
      IY=1+IFIX((2.**MY-1.)*FLOAT(JY)/32767.)
C
C Done.
C
      RETURN
C
      END
C
C + NOAO - Following subroutine 
C     SUBROUTINE OPNGKS
C
C IU(6), in IUTLCM, is the current metacode unit number.
C
C     COMMON /IUTLCM/ IU(100)
C
C Force all required BLOCKDATA's to load.
C
C     EXTERNAL GKSBD,G01BKD,UERRBD,UTILBD
C
C GKS buffer size (a dummy for NCAR GKS.)
C
C     DATA ISZ /0/
C
C Open GKS, define a workstation, and activate the workstation.
C
C     CALL GOPKS (6,ISZ)
C     CALL GOPWK (IU(6),2,1)
C     CALL GACWK (IU(6))
C
C     RETURN
C
C + NOAO
C
C     END
      SUBROUTINE PLOTIF (FX,FY,IP)
C
C Move the pen to the point (FX,FY), in the fractional cooordinate
C system.  If IP is zero, do a pen-up move.  If IP is one, do a pen-down
C move.  If IP is two, flush the buffer.
C
C The variable IU(5), in the labelled common block IUTLCM, specifies
C the size of the pen-move buffer (between 2 and 50).
C
      COMMON /IUTLCM/ IU(100)
C
C The common block VCTSEQ contains variables implementing the buffering
C of pen moves.
C
      COMMON /VCTSEQ/ NQ,QX(50),QY(50),NF,IF(25)
C
C In the common block PLTCM are recorded the coordinates of the last
C pen position, in the metacode coordinate system, for MXMY.
C
      COMMON /PLTCM/ JX,JY
C
C Force loading of the block data routine which initializes the contents
C of the common blocks.
C
C     EXTERNAL UTILBD
C
C VP and WD hold viewport and window parameters obtained, when needed,
C from GKS.
C
      DIMENSION VP(4),WD(4)
C
C + NOAO - block data utilbd has been rewritten as a run time initialization
C
      call utilbd
C
C - NOAO
C Check for out-of-range values of the pen parameter.
C
      IF (IP.LT.0.OR.IP.GT.2) THEN
        CALL SETER ('PLOTIF - ILLEGAL VALUE FOR IPEN',1,2)
      END IF
C
C If a buffer flush is requested, jump.
C
      IF (IP.EQ.2) GO TO 101
C
C Limit the given coordinates to the legal fractional range.
C
      GX=AMAX1(0.,AMIN1(1.,FX))
      GY=AMAX1(0.,AMIN1(1.,FY))
C
C Set JX and JY for a possible call to MXMY.
C
      JX=KFMX(GX)
      JY=KFMY(GY)
C
C If the current move is a pen-down move, or if the last one was, bump
C the pointer into the coordinate arrays and, if the current move is
C a pen-up move, make a new entry in the array IF, which records the
C positions of the pen-up moves.  Note that we never get two pen-up
C moves in a row, which means that IF need be dimensioned only half as
C large as QX and QY.
C
      IF (IP.NE.0.OR.IF(NF).NE.NQ) THEN
        NQ=NQ+1
        IF (IP.EQ.0) THEN
          NF=NF+1
          IF(NF)=NQ
        END IF
      END IF
C
C Save the coordinates of the point, in the fractional coordinate
C system.
C
      QX(NQ)=GX
      QY(NQ)=GY
C
C If the point-coordinate buffer is full, dump the buffers; otherwise,
C return.
C
      IF (NQ.LT.IU(5)) RETURN
C
C Dump the buffers.  If NQ is one, there's nothing to dump.  All that's
C there is a single pen-up move.
C
  101 IF (NQ.LE.1) RETURN
C
C Get NT, the number of the current transformation, and, if it is not
C zero, modify the current transformation so that we can use fractional
C coordinates (normalized device coordinates, in GKS terms).
C
      CALL GQCNTN (IE,NT)
      IF (NT.NE.0) THEN
        CALL GQNT (NT,IE,WD,VP)
        CALL GSWN (NT,VP(1),VP(2),VP(3),VP(4))
      END IF
C
C Dump out a series of polylines, each one defined by a pen-up move and
C a series of pen-down moves.
C
      DO 102 I=1,NF-1
        CALL GPL (IF(I+1)-IF(I),QX(IF(I)),QY(IF(I)))
  102 CONTINUE
      IF (IF(NF).NE.NQ) CALL GPL (NQ-IF(NF)+1,QX(IF(I)),QY(IF(I)))
C
C Put the current transformation back the way it was.
C
      IF (NT.NE.0) THEN
        CALL GSWN (NT,WD(1),WD(2),WD(3),WD(4))
      END IF
C
C Move the last pen position to the beginning of the buffer and pretend
C there was a pen-up move to that position.
C
      QX(1)=QX(NQ)
      QY(1)=QY(NQ)
      NQ=1
      IF(1)=1
      NF=1
C
C Done.
C
      RETURN
C
      END
      SUBROUTINE PLOTIT (IX,IY,IP)
C
C Move the pen to the point (IX,IY), in the metacode coordinate system.
C If IP is zero, do a pen-up move.  If IP is one, do a pen-down move.
C If IP is two, flush the buffer.  (For the sake of efficiency, the
C moves are buffered; "CALL PLOTIT (0,0,0)" will also flush the buffer.)
C
C The variable IU(5), in the labelled common block IUTLCM, specifies
C the size of the pen-move buffer (between 2 and 50).
C
      COMMON /IUTLCM/ IU(100)
C
C The common block VCTSEQ contains variables implementing the buffering
C of pen moves.
C
      COMMON /VCTSEQ/ NQ,QX(50),QY(50),NF,IF(25)
C
C In the common block PLTCM are recorded the coordinates of the last
C pen position, in the metacode coordinate system, for MXMY.
C
      COMMON /PLTCM/ JX,JY
C
C Force loading of the block data routine which initializes the contents
C of the common blocks.
C
C     EXTERNAL UTILBD
C
C VP and WD hold viewport and window parameters obtained, when needed,
C from GKS.
C
      DIMENSION VP(4),WD(4)
C
C + NOAO - Blockdata utilbd has been rewritten as a run time initialization
C
      call utilbd
C
C - NOAO
C Check for out-of-range values of the pen parameter.
C
      IF (IP.LT.0.OR.IP.GT.2) THEN
        CALL SETER ('PLOTIT - ILLEGAL VALUE FOR IPEN',1,2)
      END IF
C
C If a buffer flush is requested, jump.
C
      IF (IP.EQ.2) GO TO 101
C
C Limit the given coordinates to the legal metacode range.
C
      JX=MAX0(0,MIN0(32767,IX))
      JY=MAX0(0,MIN0(32767,IY))
C
C If the current move is a pen-down move, or if the last one was, bump
C the pointer into the coordinate arrays and, if the current move is
C a pen-up move, make a new entry in the array IF, which records the
C positions of the pen-up moves.  Note that we never get two pen-up
C moves in a row, which means that IF need be dimensioned only half as
C large as QX and QY.
C
      IF (IP.NE.0.OR.IF(NF).NE.NQ) THEN
        NQ=NQ+1
        IF (IP.EQ.0) THEN
          NF=NF+1
          IF(NF)=NQ
        END IF
      END IF
C
C Save the coordinates of the point, in the fractional coordinate
C system.
C
      QX(NQ)=FLOAT(JX)/32767.
      QY(NQ)=FLOAT(JY)/32767.
C
C If all three arguments were zero, or if the point-coordinate buffer
C is full, dump the buffers; otherwise, return.
C
      IF (IX.EQ.0.AND.IY.EQ.0.AND.IP.EQ.0) GO TO 101
      IF (NQ.LT.IU(5)) RETURN
C
C Dump the buffers.  If NQ is one, there's nothing to dump.  All that's
C there is a single pen-up move.
C
  101 IF (NQ.LE.1) RETURN
C
C Get NT, the number of the current transformation, and, if it is not
C zero, modify the current transformation so that we can use fractional
C coordinates (normalized device coordinates, in GKS terms).
C
      CALL GQCNTN (IE,NT)
      IF (NT.NE.0) THEN
        CALL GQNT (NT,IE,WD,VP)
        CALL GSWN (NT,VP(1),VP(2),VP(3),VP(4))
      END IF
C
C Dump out a series of polylines, each one defined by a pen-up move and
C a series of pen-down moves.
C
      DO 102 I=1,NF-1
        CALL GPL (IF(I+1)-IF(I),QX(IF(I)),QY(IF(I)))
  102 CONTINUE
      IF (IF(NF).NE.NQ) CALL GPL (NQ-IF(NF)+1,QX(IF(I)),QY(IF(I)))
C
C Put the current transformation back the way it was.
C
      IF (NT.NE.0) THEN
        CALL GSWN (NT,WD(1),WD(2),WD(3),WD(4))
      END IF
C
C Move the last pen position to the beginning of the buffer and pretend
C there was a pen-up move to that position.
C
      QX(1)=QX(NQ)
      QY(1)=QY(NQ)
      NQ=1
      IF(1)=1
      NF=1
C
C Done.
C
      RETURN
C
      END
      SUBROUTINE POINT (PX,PY)
C
C Draws a point at (PX,PY), defined in the user coordinate system.
C
      CALL PLOTIF (CUFX(PX),CUFY(PY),0)
      CALL PLOTIF (CUFX(PX),CUFY(PY),1)
      RETURN
      END
      SUBROUTINE POINTS (PX,PY,NP,IC,IL)
      DIMENSION PX(NP),PY(NP)
C
C Marks the points at positions in the user coordinate system defined
C by ((PX(I),PY(I)),I=1,NP).  If IC is zero, each point is marked with
C a simple point.  If IC is positive, each point is marked with the
C single character defined by the FORTRAN-77 function CHAR(IC).  If IC
C is negative, each point is marked with a GKS polymarker of type -IC.
C If IL is non-zero, a curve is also drawn, connecting the points.
C
C Define arrays to hold converted point coordinates when it becomes
C necessary to mark the points a few at a time.
C
      DIMENSION QX(10),QY(10)
C
C Define an array to hold the aspect source flags which may need to be
C retrieved from GKS.
C
      DIMENSION LA(13)
      CHARACTER*1 CHRTMP
C
C If the number of points is zero or negative, there's nothing to do.
C
      IF (NP.LE.0) RETURN
C
C Otherwise, flush the pen-move buffer.
C
      CALL PLOTIF (0.,0.,2)
C
C Retrieve the parameters from the last SET call.
C
      CALL GETSET (F1,F2,F3,F4,F5,F6,F7,F8,LL)
C
C If a linear-linear, non-mirror-imaged, mapping is being done and the
C GKS polymarkers can be used, all the points can be marked with a
C single polymarker call and joined, if requested, by a single polyline
C call.
C
      IF (F5.LT.F6.AND.F7.LT.F8.AND.LL.EQ.1.AND.IC.LE.0) THEN
        CALL GQASF (IE,LA)
        IF (LA(4).EQ.0) THEN
          CALL GQPMI (IE,IN)
          CALL GSPMI (MAX0(-IC,1))
          CALL GPM (NP,PX,PY)
          CALL GSPMI (IN)
        ELSE
          CALL GQMK (IE,IN)
          CALL GSMK (MAX0(-IC,1))
          CALL GPM (NP,PX,PY)
          CALL GSMK (IN)
        END IF
        IF (IL.NE.0.AND.NP.GE.2) CALL GPL (NP,PX,PY)
C
C Otherwise, things get complicated.  We have to do batches of nine
C points at a time.  (Actually, we convert ten coordinates at a time,
C so that the curve joining the points, if any, won't have gaps in it.)
C
      ELSE
C
C Initially, we have to reset either the polymarker index or the text
C alignment, depending on how we're marking the points.
C
        IF (IC.LE.0) THEN
          CALL GQASF (IE,LA)
          IF (LA(4).EQ.0) THEN
            CALL GQPMI (IE,IN)
            CALL GSPMI (MAX0(-IC,1))
          ELSE
            CALL GQMK (IE,IN)
            CALL GSMK (MAX0(-IC,1))
          END IF
        ELSE
          CALL GQTXAL (IE,IH,IV)
          CALL GSTXAL (2,3)
        END IF
C
C Loop through the points by nines.
C
        DO 104 IP=1,NP,9
C
C Fill the little point coordinate arrays with up to ten values,
C converting them from the user system to the fractional system.
C
          NQ=MIN0(10,NP-IP+1)
          MQ=MIN0(9,NQ)
          DO 102 IQ=1,NQ
            QX(IQ)=CUFX(PX(IP+IQ-1))
            QY(IQ)=CUFY(PY(IP+IQ-1))
  102     CONTINUE
C
C Change the SET call to allow the use of fractional coordinates.
C
          CALL SET (F1,F2,F3,F4,F1,F2,F3,F4,1)
C
C Crank out either a polymarker or a set of characters.
C
          IF (IC.LE.0) THEN
            CALL GPM (MQ,QX,QY)
          ELSE
            DO 103 IQ=1,MQ
              CHRTMP = CHAR(IC)
              CALL GTX (QX(IQ),QY(IQ),CHRTMP)
  103       CONTINUE
          END IF
          IF (IL.NE.0.AND.NQ.GE.2) CALL GPL (NQ,QX,QY)
C
C Put the SET parameters back the way they were.
C
          CALL SET (F1,F2,F3,F4,F5,F6,F7,F8,LL)
C
  104   CONTINUE
C
C Finally, we put either the polymarker index or the text alignment
C back the way it was.
C
        IF (IC.LE.0) THEN
          IF (LA(4).EQ.0) THEN
            CALL GSPMI (IN)
          ELSE
            CALL GSMK (IN)
          END IF
        ELSE
          CALL GSTXAL (IH,IV)
        END IF
C
      END IF
C
C Update the pen position.
C
      CALL FRSTPT (PX(NP),PY(NP))
C
C Done.
C
      RETURN
C
      END
      SUBROUTINE PWRIT (PX,PY,CH,NC,IS,IO,IC)
      CHARACTER*(*) CH
C
C PWRIT is called to draw a character string in a specified position.
C It is just like WTSTR, but has one extra argument.  NC is the number
C of characters to be written from the string CH.
C
      CALL WTSTR (PX,PY,CH(1:NC),IS,IO,IC)
C
C Done.
C
      RETURN
C
      END
      SUBROUTINE SET (VL,VR,VB,VT,WL,WR,WB,WT,LF)
C
C SET allows the user to change the current values of the parameters
C defining the mapping from the user system to the fractional system
C (in GKS terminology, the mapping from world coordinates to normalized
C device coordinates).
C
C VL, VR, VB, and VT define the viewport (in the fractional system), WL,
C WR, WB, and WT the window (in the user system), and LF the nature of
C the mapping, according to the following table:
C
C    1  -  x linear, y linear
C    2  -  x linear, y logarithmic
C    3  -  x logarithmic, y linear
C    4  -  x logarithmic, y logarithmic
C
C Declare the common block containing the linear-log and mirror-imaging
C flags.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
C
C Flush the pen-move buffer.
C
      CALL PLOTIF (0.,0.,2)
C
C Set the GKS viewport for transformation 1.
C
      CALL GSVP (1,VL,VR,VB,VT)
C
C Set the utility state variable controlling linear-log mapping.
C
      LL=MAX0(1,MIN0(4,LF))
C
C Set the GKS window for transformation 1.
C
      IF (WL.LT.WR) THEN
        MI=1
        QL=WL
        QR=WR
      ELSE
        MI=3
        QL=WR
        QR=WL
      END IF
C
      IF (WB.LT.WT) THEN
        QB=WB
        QT=WT
      ELSE
        MI=MI+1
        QB=WT
        QT=WB
      END IF
C
      IF (LL.EQ.1) THEN
        CALL GSWN (1,QL,QR,QB,QT)
      ELSE IF (LL.EQ.2) THEN
        CALL GSWN (1,QL,QR,ALOG10(QB),ALOG10(QT))
      ELSE IF (LL.EQ.3) THEN
        CALL GSWN (1,ALOG10(QL),ALOG10(QR),QB,QT)
      ELSE
        CALL GSWN (1,ALOG10(QL),ALOG10(QR),ALOG10(QB),ALOG10(QT))
      END IF
C
C Select transformation 1 as the current one.
C
      CALL GSELNT (1)
C
      RETURN
C
      END
      SUBROUTINE SETI (IX,IY)
C
C Allows the user to set the parameters which determine the assumed size
C of the target plotter and therefore determine how user coordinates are
C to be mapped into plotter coordinates.
C
C Declare the common block containing the scaling information.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
C
C Transfer the user's values into the common block.
C
      MX=MAX0(1,MIN0(15,IX))
      MY=MAX0(1,MIN0(15,IY))
C
      RETURN
C
      END
      SUBROUTINE SETUSV (VN,IV)
      CHARACTER*(*) VN
C
C This subroutine sets the values of various utility state variables.
C VN is the name of the variable and IV is its value.
C
C The labelled common block IUTLCM contains all of the utility state
C variables.
C
      COMMON /IUTLCM/ IU(100)
C
C Define an array in which to get the GKS aspect source flags.
C
      DIMENSION LF(13)
C
C Check for the linear-log scaling variable, which can take on these
C values:
C
C     1 = X linear, Y linear
C     2 = X linear, Y log
C     3 = X log   , Y linear
C     4 = X log   , Y log
C
      IF (VN(1:2).EQ.'LS') THEN
        IF (IV.LT.1.OR.IV.GT.4) THEN
          CALL SETER ('SETUSV - LOG SCALE VALUE OUT OF RANGE',2,2)
        END IF
        IU(1)=IV
C
C Check for the mirror-imaging variable, which can take on these
C values:
C
C     1 = X normal  , Y normal
C     2 = X normal  , Y reversed
C     3 = X reversed, Y normal
C     4 = X reversed, Y reversed
C
      ELSE IF (VN(1:2).EQ.'MI') THEN
        IF (IV.LT.1.OR.IV.GT.4) THEN
          CALL SETER ('SETUSV - MIRROR-IMAGING VALUE OUT OF RANGE',3,2)
        END IF
        IU(2)=IV
C
C Check for the scale factor setting the resolution of the plotter in
C the x direction.
C
      ELSE IF (VN(1:2).EQ.'XF') THEN
        IF (IV.LT.1.OR.IV.GT.15) THEN
          CALL SETER ('SETUSV - X RESOLUTION OUT OF RANGE',4,2)
        END IF
        IU(3)=IV
C
C Check for the scale factor setting the resolution of the plotter in
C the y direction.
C
      ELSE IF (VN(1:2).EQ.'YF') THEN
        IF (IV.LT.1.OR.IV.GT.15) THEN
          CALL SETER ('SETUSV - Y RESOLUTION OUT OF RANGE',5,2)
        END IF
        IU(4)=IV
C
C Check for the variable specifying the size of the pen-move buffer.
C
      ELSE IF (VN(1:2).EQ.'PB') THEN
        IF (IV.LT.2.OR.IV.GT.50) THEN
          CALL SETER ('SETUSV - PEN-MOVE BUFFER SIZE OUT OF RANGE',6,2)
        END IF
        CALL PLOTIF (0.,0.,2)
        IU(5)=IV
C
C Check for a metacode unit number.
C
      ELSE IF (VN(1:2).EQ.'MU') THEN
        IF (IV.LE.0) THEN
          CALL SETER ('SETUSV - METACODE UNIT NUMBER ILLEGAL',7,2)
        END IF
C
C For the moment (1/11/85), we have to deactivate and close the old
C workstation and open and activate a new one.  This does allow the
C user to break up his metacode output.  It does not necessarily allow
C for the resumption of output to a previously-written metacode file.
C
        CALL GDAWK (IU(6))
        CALL GCLWK (IU(6))
        IU(6)=IV
        CALL GOPWK (IU(6),2,1)
        CALL GACWK (IU(6))
C
C If, in the future, it becomes possible to have more than one metacode
C workstation open at once, the following code can be used instead.
C
C       CALL GDAWK (IU(6))
C       IU(6)=IV
C       CALL GQOPWK (0,IE,NO,ID)
C       IF (NO.NE.0) THEN
C         DO 101 I=1,NO
C           CALL GQOPWK (I,IE,NO,ID)
C           IF (ID.EQ.IU(6)) GO TO 102
C 101     CONTINUE
C       END IF
C       CALL GOPWK (IU(6),2,1)
C 102   CALL GAWK (IU(6))
C
C Check for one of the variables setting color and intensity.
C
      ELSE IF (VN(1:2).EQ.'IR') THEN
        IF (IV.LT.0) THEN
          CALL SETER ('SETUSV - ILLEGAL VALUE OF RED INTENSITY',8,2)
        END IF
        IU(7)=IV
C
      ELSE IF (VN(1:2).EQ.'IG') THEN
        IF (IV.LT.0) THEN
          CALL SETER ('SETUSV - ILLEGAL VALUE OF GREEN INTENSITY',9,2)
        END IF
        IU(8)=IV
C
      ELSE IF (VN(1:2).EQ.'IB') THEN
        IF (IV.LT.0) THEN
          CALL SETER ('SETUSV - ILLEGAL VALUE OF BLUE INTENSITY',10,2)
        END IF
        IU(9)=IV
C
      ELSE IF (VN(1:2).EQ.'IN') THEN
        IF (IV.LT.0.OR.IV.GT.10000) THEN
          CALL SETER ('SETUSV - ILLEGAL VALUE OF INTENSITY',11,2)
        END IF
        IU(10)=IV
C
C Assign the intensity-controlling variables to local variables with
C simple, meaningful names.
C
        IR=IU(7)
        IG=IU(8)
        IB=IU(9)
        IN=IU(10)
        II=IU(11)
        IM=IU(12)
C
C Compute the floating-point red, green, and blue intensities.
C
        FR=FLOAT(IR)/FLOAT(MAX0(IR,IG,IB,1))*FLOAT(IN)/10000.
        FG=FLOAT(IG)/FLOAT(MAX0(IR,IG,IB,1))*FLOAT(IN)/10000.
        FB=FLOAT(IB)/FLOAT(MAX0(IR,IG,IB,1))*FLOAT(IN)/10000.
C
C Dump the pen-move buffer before changing anything.
C
        CALL PLOTIF (0.,0.,2)
C
C Set the aspect source flags for all the color indices to "individual".
C
        CALL GQASF (IE,LF)
        LF( 3)=1
        LF( 6)=1
        LF(10)=1
        LF(13)=1
        CALL GSASF (LF)
C
C Pick a new color index and use it for polylines, polymarkers, text,
C and areas.
C
        II=MOD(II,IM)+1
        IU(11)=II
        CALL GSPLCI (II)
        CALL GSPMCI (II)
        CALL GSTXCI (II)
        CALL GSFACI (II)
C
C Now, redefine the color for that color index on each open workstation.
C
        CALL GQOPWK (0,IE,NO,ID)
C
        DO 103 I=1,NO
          CALL GQOPWK (I,IE,NO,ID)
          CALL GSCR (ID,II,FR,FG,FB)
  103   CONTINUE
C
C Check for variable resetting the color index.
C
      ELSE IF (VN(1:2).EQ.'II') THEN
        IF (IV.LT.1.OR.IV.GT.IU(12)) THEN
          CALL SETER ('SETUSV - ILLEGAL COLOR INDEX',12,2)
        END IF
        IU(11)=IV
C
        CALL PLOTIF (0.,0.,2)
C
        CALL GQASF (IE,LF)
        LF( 3)=1
        LF( 6)=1
        LF(10)=1
        LF(13)=1
        CALL GSASF (LF)
C
        CALL GSPLCI (IV)
        CALL GSPMCI (IV)
        CALL GSTXCI (IV)
        CALL GSFACI (IV)
C
C Check for the variable limiting the values of color index used.
C
      ELSE IF (VN(1:2).EQ.'IM') THEN
        IF (IV.LT.1) THEN
          CALL SETER ('SETUSV - ILLEGAL MAXIMUM COLOR INDEX',13,2)
        END IF
        IU(12)=IV
C
C Check for the variable setting the current line width scale factor.
C
      ELSE IF (VN(1:2).EQ.'LW') THEN
        IF (IV.LT.0) THEN
          CALL SETER ('SETUSV - ILLEGAL LINE WIDTH SCALE FACTOR',14,2)
        END IF
        IU(13)=IV
C
C Dump the pen-move buffer before changing anything.
C
        CALL PLOTIF (0.,0.,2)
C
C Set the aspect source flag for linewidth scale factor to "individual".
C
        CALL GQASF (IE,LF)
        LF(2)=1
        CALL GSASF (LF)
C
C Redefine the line width scale factor.
C
        CALL GSLWSC (FLOAT(IV)/1000.)
C
C Check for the variable setting the current marker size scale factor.
C
      ELSE IF (VN(1:2).EQ.'MS') THEN
        IF (IV.LT.0) THEN
          CALL SETER ('SETUSV - ILLEGAL MARKER SIZE SCALE FACTOR',15,2)
        END IF
        IU(14)=IV
C
C Set aspect source flag for marker size scale factor to "individual".
C
        CALL GQASF (IE,LF)
        LF(5)=1
        CALL GSASF (LF)
C
C Redefine the marker size scale factor.
C
        CALL GSMKSC (FLOAT(IV)/1000.)
C
C Otherwise, the variable name is unknown.
C
      ELSE
        CALL SETER ('SETUSV - UNKNOWN VARIABLE NAME IN CALL',1,2)
C
      ENDIF
      RETURN
      END
      SUBROUTINE VECTOR (PX,PY)
C
C Draw a vector (line segment) from the current pen position to the new
C pen position (PX,PY), in the user coordinate system, and then make
C (PX,PY) the current pen position.
C
      CALL PLOTIF (CUFX(PX),CUFY(PY),1)
      RETURN
      END
      SUBROUTINE WTSTR (PX,PY,CH,IS,IO,IC)
C
C WTSTR is called to draw a character string in a specified position.
C
C PX and PY specify, in user coordinates, the position of a point
C relative to which a character string is to be positioned.
C
C CH is the character string to be written.
C
C IS is the desired size of the characters to be used, stated as a
C character width in the plotter coordinate system.  The values 0, 1,
C 2, and 3 mean 8, 12, 16, and 24, respectively.
C
C IO is the desired orientation angle, in degrees counterclockwise from
C a horizontal vector pointing to the right.
C
C IC specifies the desired type of centering.  A negative value puts
C (PX,PY) in the center of the left end of the character string, a zero
C puts (PX,PY) in the center of the whole string, and a positive value
C puts (PX,PY) in the center of the right end of the character string.
C
      CHARACTER*(*) CH
C
C Define arrays in which to save the current viewport and window.
C
      DIMENSION VP(4),WD(4)
C
C Flush the pen-move buffer.
C
      CALL PLOTIF (0.,0.,2)
C
C Compute the coordinates of (PX,PY) in the fractional coordinate
C system (normalized device coordinates).
C
      XN=CUFX(PX)
      YN=CUFY(PY)
C
C Save the current window and, if necessary, redefine it so that we can
C use normalized device coordinates.
C
      CALL GQCNTN (IE,NT)
      IF (NT.NE.0) THEN
        CALL GQNT (NT,IE,WD,VP)
        CALL GSWN (NT,VP(1),VP(2),VP(3),VP(4))
      END IF
C
C Save current character height, text path, character up vector, and
C text alignment.
C
      CALL GQCHH (IE,OS)
      CALL GQTXP (IE,IP)
      CALL GQCHUP (IE,UX,UY)
      CALL GQTXAL (IE,IX,IY)
C
C Define the character height.  (The final scale factor is derived from
C the default font.)
C
      CALL GETUSV ('YF',MY)
      YS=FLOAT(2**MY)
      IF (IS.GE.0.AND.IS.LE.3) THEN
        CS=FLOAT(8+4*IS+4*(IS/3))/YS
      ELSE
        CS=AMIN1(FLOAT(IS),YS)/YS
      ENDIF
C
      CS=CS*25.5/27.
C
C + NOAO - make character size readable with IRAF font
      cs = cs * 2.0
C
C - NOAO

      CALL GSCHH(CS)
C
C Define the text path.
C
      CALL GSTXP (0)
C
C Define the character up vector.
C
      JO=MOD(IO,360)
      IF (JO.EQ.0) THEN
        CALL GSCHUP (0.,1.)
      ELSE IF (JO.EQ.90) THEN
        CALL GSCHUP (-1.,0.)
      ELSE IF (JO.EQ.180) THEN
        CALL GSCHUP (0.,-1.)
      ELSE IF (JO.EQ.270) THEN
        CALL GSCHUP (1.,0.)
      ELSE IF (JO.GT.0.AND.JO.LT.180) THEN
        CALL GSCHUP (-1.,1./TAN(FLOAT(JO)*3.1415926/180.))
      ELSE
        CALL GSCHUP (1.,-1./TAN(FLOAT(JO)*3.1415926/180.))
      ENDIF
C
C Define the text alignment.
C
      CALL GSTXAL (IC+2,3)
C
C Plot the characters.
C
      CALL GTX (XN,YN,CH)
C
C Restore the original text attributes.
C
      CALL GSCHH (OS)
      CALL GSTXP (IP)
      CALL GSCHUP (UX,UY)
      CALL GSTXAL (IX,IY)
C
C Restore the window definition.
C
      IF (NT.NE.0) THEN
        CALL GSWN (NT,WD(1),WD(2),WD(3),WD(4))
      END IF
C
C Update the pen position.
C
      CALL FRSTPT (PX,PY)
C
C Done.
C
      RETURN
C
      END
c + NOAO - blockdata utilbd changed to run time initialization
      subroutine utilbd
c     BLOCKDATA UTILBD
C
      logical first
C The common block IUTLCM contains integer utility variables which are
C user-settable by the routine SETUSV and user-retrievable by the
C routine GETUSV.
C
      COMMON /IUTLCM/ IU(100)
C
C The common block VCTSEQ contains variables realizing the buffering
C scheme used by PLOTIT/F for pen moves.  The dimension of QX and QY must
C be an even number greater than or equal to the value of IU(5).  The
C dimension of IF must be half that of QX and QY.
C
      COMMON /VCTSEQ/ NQ,QX(50),QY(50),NF,IF(25)
C
C In the common block PLTCM are recorded the coordinates of the last
C point to which a pen move was requested by a call to PLOTIT/F.
C
      COMMON /PLTCM/ JX,JY
C
C IU(1) contains the log scaling parameter, which may take on the
C following possible values:
C
C     1 = linear-linear
C     2 = log-linear
C     3 = linear-log
C     4 = log-log
C
c     DATA IU(1) / 1 /
      IU(1) = 1 
C
C IU(2) specifies the mirror-imaging of the x and y axes, as follows:
C
C     1 = x normal, y normal
C     2 = x normal, y reversed
C     3 = x reversed, y normal
C     4 = x reversed, y reversed
C
c +NOAO - logical parameter first inserted to avoid clobbering initialization
      data first /.true./
      if (.not. first) return
        first = .false.
c -NOAO
c     DATA IU(2) / 1 /
      IU(2) = 1 
C
C IU(3) specifies the assumed resolution of the plotter in the x
C direction.  Plotter x coordinates are assumed to lie between 1 and
C 2**IU(3), inclusive.
C
c     DATA IU(3) / 10 /
      IU(3) = 10 
C
C IU(4) specifies the assumed resolution of the plotter in the y
C direction.  Plotter y coordinates are assumed to lie between 1 and
C 2**IU(4), inclusive.
C
c     DATA IU(4) / 10 /
      IU(4) = 10 
C
C IU(5) specifies the size of the buffers used by PLOTIT/F.  Its value
C must be greater than or equal to 2 and not greater than the dimension
C of the variables QX and QY.  Using the value 2 effectively turns off
C the buffering.
C
c     DATA IU(5) / 50 /
      IU(5) = 50
C
C IU(6) specifies the current metacode unit, which is machine-dependent.
C At NCAR, the value "1" currently (1/11/85) causes metacode to be
C written on the file "GMETA".  Eventually, it will cause output to be
C written on unit number 1.  At that point, the value, on the Cray at
C least, should be changed to "4H$PLT", so that output will come out on
C the old familiar dataset.
C
c     DATA IU(6) / 1 /
      IU(6) = 1 
C
C IU(7), IU(8), IU(9), and IU(10) specify color and intensity, in the
C following way (letting IR=IU(7), IG=IU(8), IB=IU(9), and IN=IU(10)):
C
C     The red intensity is IR/(IR+IG+IB)*IN/10000.
C     The green intensity is IG/(IR+IG+IB)*IN/10000.
C     The blue intensity is IB/(IR+IG+IB)*IN/10000.
C
C The GKS calls to set these intensities are executed in response to a
C "CALL SETUSV ('IN',IN)", using the existing values of IR, IG, and IB.
C Thus, to completely determine the color and the intensity, the user
C must execute four calls, as follows:
C
C     CALL SETUSV ('IR',IR)
C     CALL SETUSV ('IG',IG)
C     CALL SETUSV ('IB',IB)
C     CALL SETUSV ('IN',IN)
C
C The default values create a white line at .8 x maximum intensity.
C
c     DATA IU(7) / 1 /
c     DATA IU(8) / 1 /
c     DATA IU(9) / 1 /
      IU(7) = 1
      IU(8) = 1 
      IU(9) = 1
C
c     DATA IU(10) / 8000 /
      IU(10) = 8000 
C
C IU(11) and IU(12) specify, respectively, the last color index used
C and the maximum number of color indices it is permissible to use.
C
c     DATA IU(11) / 0 /
c     DATA IU(12) / 1 /
      IU(11) = 0 
      IU(12) = 1 
C
C IU(13)/1000 specifies the current line width scale factor.
C
c     DATA IU(13) / 1000 /
      IU(13) = 1000
C
C IU(14)/1000 specifies the current marker size scale factor.
C
c     DATA IU(14) / 1000 /
      IU(14) = 1000 
C
C IU(15) through IU(100) are currently undefined.
C
C Initialization for the routine PLOTIT/F:  For values of I between 1 and
C NQ, (QX(I),QY(I)) is a point to which a pen move has been requested
C by a past call to PLOTIT/F.  The coordinates are stated in the fractional
C coordinate system.  For values of I between 1 and NF, IF(I) is the
C index, in QX and QY, of the coordinates of a point to which a pen-up
C move was requested.  NQ and NF are never allowed to be less than one.
C
c     DATA NQ,QX(1),QY(1),NF,IF(1) / 1 , 0. , 0. , 1 , 1 /
      NQ    = 1
      QX(1) = 0. 
      QY(1) = 0. 
      NF    = 1 
      IF(1) = 1 
C
C JX and JY are the coordinates, in the metacode system, of the last
C point to which a pen move was requested by a call to PLOTIT/F.
C
c     DATA JX,JY / 0 , 0 /
      JX = 0 
      JY = 0 
C
c -NOAO
      return
c
      entry initut
      first = .true.
      END
