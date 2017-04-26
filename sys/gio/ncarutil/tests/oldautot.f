      SUBROUTINE EXMPL1
C
C Define the data array.
C
      REAL YDRA(1001)
C
C Fill the data array.
C
      DO 101 I=1,1001
        X=FLOAT(I)/20.
        YDRA(I)=10.*(X-1.)*(X-11.)*(X-21.)*(X-31.)*(X-41.)*(X-51.)
     +                                                 +2.E7*(FRAN()-.5)
  101 CONTINUE
C
C Draw a boundary around the edge of the plotter frame.
C
c     CALL BNDARY
C
C Draw the graph, using EZY.
C
      CALL EZY (YDRA,1001,'EXAMPLE 1 (EZY)$')
C
c     STOP
C
      END
      FUNCTION FRAN()
C
C Random-number generator.
C
      DATA X / 2.7182818 /
      SAVE X
      X=AMOD(9821.*X+.211327,1.)
      FRAN=X
      RETURN
      END
      SUBROUTINE BNDARY
C
C Routine to draw the plotter-frame edge.
C
      CALL PLOTIT (    0,    0,0)
      CALL PLOTIT (32767,    0,1)
      CALL PLOTIT (32767,32767,1)
      CALL PLOTIT (    0,32767,1)
      CALL PLOTIT (    0,    0,1)
      RETURN
      END
c
      SUBROUTINE EXMPL2
C
C Define the data arrays.
C
      REAL XDRA(4001),YDRA(4001)
C
C Fill the data arrays.
C
      DO 101 I=1,4001
        THETA=.0015707963267949*FLOAT(I-1)
        RHO=SIN(2.*THETA)+.05*SIN(64.*THETA)
        XDRA(I)=RHO*COS(THETA)
        YDRA(I)=RHO*SIN(THETA)
  101 CONTINUE
C
C Draw a boundary around the edge of the plotter frame.
C
c     CALL BNDARY
C
C Draw the graph, using EZXY.
C
      CALL EZXY (XDRA,YDRA,4001,'EXAMPLE 2 (EZXY)$')
C
c     STOP
C
      END
c
      SUBROUTINE EXMPL3
C
C Define the data array.
C
      REAL YDRA(100,2)
C
C Fill the data array.
C
      DO 101 I=1,100
        YDRA(I,1)=COS(3.14159265358979*FLOAT(I)/25.)*FLOAT(I)**2
        YDRA(I,2)=COS(3.14159265358979*FLOAT(I)/25.)*10.**(.04*FLOAT(I))
  101 CONTINUE
C
C Draw a boundary around the edge of the plotter frame.
C
c     CALL BNDARY
C
C Draw the graph, using EZMY.
C
      CALL EZMY (YDRA,100,2,100,'EXAMPLE 3 (EZMY)$')
C
c     STOP
C
      END
c
      SUBROUTINE EXMPL4
C
C Define the data arrays.
C
      REAL XDRA(201),YDRA(201,10)
C
C Fill the data arrays.
C
      DO 102 I=1,201
        XDRA(I)=-1.+.02*FLOAT(I-1)
        IF (I.GT.101) XDRA(I)=2.-XDRA(I)
        DO 101 J=1,10
          YDRA(I,J)=FLOAT(J)*SQRT(1.000000000001-XDRA(I)**2)/10.
          IF (I.GT.101) YDRA(I,J)=-YDRA(I,J)
  101   CONTINUE
  102 CONTINUE
C
C Draw a boundary around the edge of the plotter frame.
C
c     CALL BNDARY
C
C Draw the graph, using EZMXY.
C
      CALL EZMXY (XDRA,YDRA,201,10,201,'EXAMPLE 4 (EZMXY)$')
C
c     STOP
C
      END
c
      SUBROUTINE EXMPL5
C
C Define the data arrays.
C
      REAL XDRA(401,6),YDRA(401,6)
C
C Compute required constants.
C
      PI=3.14159265358979
      PID200=PI/200.
      PITTWO=2.*PI
      PIT2D3=2.*PI/3.
      PIT4D3=4.*PI/3.
      RADOSC=SQRT(3.)/3.
      RADOLC=SQRT(3.)/2.
      BSSCLL=ATAN(SQRT(12.)/6.)
      BSSCUL=ATAN(SQRT(143.)/7.)
      BSLCLL=ATAN(SQRT(143.)/17.)
      BSLCUL=ATAN(SQRT(2.0))
C
C Fill the data arrays.
C
      DO 101 I=1,401
        THETA=PID200*FLOAT(I-1)
        XDRA(I,1)=   -.5+RADOSC*COS(THETA)
        YDRA(I,1)=       RADOSC*SIN(THETA)
        IF (ABS(THETA       ).GE.BSSCLL.AND.
     +      ABS(THETA       ).LE.BSSCUL) XDRA(I,1)=1.E36
        IF (ABS(THETA-PITTWO).GE.BSSCLL.AND.
     +      ABS(THETA-PITTWO).LE.BSSCUL) XDRA(I,1)=1.E36
        XDRA(I,2)=    .5+RADOSC*COS(THETA)
        YDRA(I,2)=       RADOSC*SIN(THETA)
        IF (ABS(THETA-PIT2D3).GE.BSSCLL.AND.
     +      ABS(THETA-PIT2D3).LE.BSSCUL) XDRA(I,2)=1.E36
        XDRA(I,3)=       RADOSC*COS(THETA)
        YDRA(I,3)=RADOLC+RADOSC*SIN(THETA)
        IF (ABS(THETA-PIT4D3).GE.BSSCLL.AND.
     +      ABS(THETA-PIT4D3).LE.BSSCUL) XDRA(I,3)=1.E36
        XDRA(I,4)=   -.5+RADOLC*COS(THETA)
        YDRA(I,4)=       RADOLC*SIN(THETA)
        IF (ABS(THETA       ).GE.BSLCLL.AND.
     +      ABS(THETA       ).LE.BSLCUL) XDRA(I,4)=1.E36
        IF (ABS(THETA-PITTWO).GE.BSLCLL.AND.
     +      ABS(THETA-PITTWO).LE.BSLCUL) XDRA(I,4)=1.E36
        XDRA(I,5)=    .5+RADOLC*COS(THETA)
        YDRA(I,5)=       RADOLC*SIN(THETA)
        IF (ABS(THETA-PIT2D3).GE.BSLCLL.AND.
     +      ABS(THETA-PIT2D3).LE.BSLCUL) XDRA(I,5)=1.E36
        XDRA(I,6)=       RADOLC*COS(THETA)
        YDRA(I,6)=RADOLC+RADOLC*SIN(THETA)
        IF (ABS(THETA-PIT4D3).GE.BSLCLL.AND.
     +      ABS(THETA-PIT4D3).LE.BSLCUL) XDRA(I,6)=1.E36
  101 CONTINUE
C
C Specify subscripting of XDRA and YDRA.
C
      CALL AGSETI ('ROW.',2)
C
C Make sure grid shape is such that one unit in x = one unit in y.
C
      CALL AGSETF ('GRID/SHAPE.',2.)
C
C Turn off background, then turn labels back on.
C
      CALL AGSETF ('BACKGROUND.',4.)
      CALL AGSETI ('LABEL/CONTROL.',2)
C
C Turn off left label.
C
      CALL AGSETC ('LABEL/NAME.','L')
      CALL AGSETI ('LABEL/SUPPRESSION FLAG.',1)
C
C Change text of bottom label.
C
      CALL AGSETC ('LABEL/NAME.','B')
      CALL AGSETI ('LINE/NUMBER.',-100)
      CALL AGSETC ('LINE/TEXT.','PURITY, BODY, AND FLAVOR$')
C
C Draw a boundary around the edge of the plotter frame.
C
c     CALL BNDARY
C
C Draw the graph, using EZMXY.
C
      CALL EZMXY (XDRA,YDRA,401,6,401,'EXAMPLE 5 (EZMXY)$')
C
c     STOP
C
      END
c
      SUBROUTINE EXMPL6
C
C Define the data arrays.
C
      REAL XDRA(501),YDRA(501)
C
      CHARACTER*35 GLAB
      CHARACTER*23 BACK(4)
      CHARACTER*12 LNLG(4)
      character*1  tmp
C Define the graph-window parameter array.
C
      REAL GWND (4,4)
C
      DATA (GWND(I,1),I=1,4) / 0.0 , 0.5 , 0.5 , 1.0 /
      DATA (GWND(I,2),I=1,4) / 0.5 , 1.0 , 0.5 , 1.0 /
      DATA (GWND(I,3),I=1,4) / 0.0 , 0.5 , 0.0 , 0.5 /
      DATA (GWND(I,4),I=1,4) / 0.5 , 1.0 , 0.0 , 0.5 /
C
C Define variables used in setting up informational labels on the graph.
C
C
      DATA BACK(1) / '(PERIMETER BACKGROUND)$' /
      DATA BACK(2) / '(GRID BACKGROUND)$     ' /
      DATA BACK(3) / '(HALF-AXIS BACKGROUND)$' /
      DATA BACK(4) / '(NO BACKGROUND)$       ' /
C
      DATA LNLG(1) / 'LINEAR$' /
      DATA LNLG(2) / 'LOGARITHMIC$' /
C
C Fill the data arrays.
C
      DO 101 I=1,501
        THETA=.031415926535898*FLOAT(I-1)
        XDRA(I)=500.+.9*FLOAT(I-1)*COS(THETA)
        YDRA(I)=500.+.9*FLOAT(I-1)*SIN(THETA)
  101 CONTINUE
C
C
C Do four graphs on the same frame, using different backgrounds.
C
      DO 102 IGRF = 1,4
C
C Suppress the frame advance.
C
      CALL AGSETI ('FRAME.',2)
C
C Position the graph window.
C
        CALL AGSETP ('GRAPH WINDOW.',GWND(1,IGRF),4)
C
C Declare the background type.
C
        CALL AGSETI ('BACKGROUND TYPE.',IGRF)
C
C Setting the background type may have turned the informational labels
C off.  In that case, turn them back on.
C
        IF (IGRF.EQ.4) CALL AGSETI ('LABEL/CONTROL.',2)
C
C Set up parameters determining the linear/log nature of the axes.
C
        ILLX=(IGRF-1)/2
        ILLY=MOD(IGRF-1,2)
C
C Declare the linear/log nature of the graph.
C
        CALL AGSETI ('X/LOGARITHMIC.',ILLX)
        CALL AGSETI ('Y/LOGARITHMIC.',ILLY)
C
C Change the x- and y-axis labels to reflect the linear/log nature of
C the graph.
C
        CALL AGSETC ('LABEL/NAME.','B')
        CALL AGSETI ('LINE/NUMBER.',-100)
        CALL AGSETC ('LINE/TEXT.',LNLG(ILLX+1))
C
        CALL AGSETC ('LABEL/NAME.','L')
        CALL AGSETI ('LINE/NUMBER.',100)
        CALL AGSETC ('LINE/TEXT.',LNLG(ILLY+1))
C
C Set up the label for the top of the graph.
C
c     WRITE (GLAB,1001) IGRF,BACK(IGRF)
      glab(1:35) = 'EXAMPLE 6-                         '
      glab(11:11) = char (igrf + ichar ('0'))
      glab(13:35) = back (igrf)
C
C Draw the graph, using EZXY.
C
        CALL EZXY (XDRA,YDRA,501,GLAB)
C
  102 CONTINUE
C
C Draw a boundary around the edge of the plotter frame.
C
c     CALL BNDARY
C
C Advance the frame.
C
      CALL FRAME
C
c     STOP
C
C Format for encode.
C
c1001 FORMAT ('EXAMPLE 6-',I1,' ',A23)
      END
c
      SUBROUTINE EXMPL7
C
C Define the data arrays and the dash-pattern array.
C
      REAL XDRA(101),YDRA(101,9)
      CHARACTER*28 DSHP(9)
C
C Declare the type of the dash-pattern-name generator.
C
      CHARACTER*16 AGDSHN
C
C Fill the data arrays and the dash pattern array.
C
      DO 101 I=1,101
        XDRA(I)=-90.+1.8*FLOAT(I-1)
  101 CONTINUE
C
      DO 103 J=1,9
c       WRITE (DSHP(J),1001) J
        dshp(j) = '$$$$$$$$$$$$$$$$$$$$$ J =   '
	dshp(j)(27:27) = char (j + ichar ('0'))
        FJ=J
        DO 102 I=1,101
          YDRA(I,J)=3.*FJ-(FJ/2700.)*XDRA(I)**2
  102   CONTINUE
  103 CONTINUE
C
C Turn on windowing.  (Some curves run outside the curve window.)
C
      CALL AGSETI ('WINDOWING.',1)
C
C Move the edges of the curve window (grid).
C
      CALL AGSETF ('GRID/LEFT.'  ,.10)
      CALL AGSETF ('GRID/RIGHT.' ,.90)
      CALL AGSETF ('GRID/BOTTOM.',.10)
      CALL AGSETF ('GRID/TOP.'   ,.85)
C
C Set the x and y minimum and maximum.
C
      CALL AGSETF ('X/MINIMUM.',-90.)
      CALL AGSETF ('X/MAXIMUM.',+90.)
      CALL AGSETF ('Y/MINIMUM.',  0.)
      CALL AGSETF ('Y/MAXIMUM.', 18.)
C
C Set left axis parameters.
C
      CALL AGSETI ('LEFT/MAJOR/TYPE.',1)
      CALL AGSETF ('LEFT/MAJOR/BASE.',3.)
      CALL AGSETI ('LEFT/MINOR/SPACING.',2)
C
C Set right axis parameters.
C
      CALL AGSETI ('RIGHT/FUNCTION.',1)
      CALL AGSETF ('RIGHT/NUMERIC/TYPE.',1.E36)
C
C Set bottom axis parameters.
C
      CALL AGSETI ('BOTTOM/MAJOR/TYPE.',1)
      CALL AGSETF ('BOTTOM/MAJOR/BASE.',15.)
      CALL AGSETI ('BOTTOM/MINOR/SPACING.',2)
C
C Set top axis parameters.
C
      CALL AGSETI ('TOP/FUNCTION.',1)
      CALL AGSETF ('TOP/NUMERIC/TYPE.',1.E36)
C
C Set up the dash patterns to be used.
C
      CALL AGSETI ('DASH/SELECTOR.',9)
      CALL AGSETI ('DASH/LENGTH.',28)
      DO 104 I=1,9
        CALL AGSETC (AGDSHN(I),DSHP(I))
  104 CONTINUE
C
C Set up the left label.
C
      CALL AGSETC ('LABEL/NAME.','L')
      CALL AGSETI ('LINE/NUMBER.',100)
      CALL AGSETC ('LINE/TEXT.','HEIGHT (KILOMETERS)$')
C
C Set up the right label.
C
      CALL AGSETC ('LABEL/NAME.','R')
      CALL AGSETI ('LINE/NUMBER.',-100)
      CALL AGSETC ('LINE/TEXT.','PRESSURE (TONS/SQUARE FURLONG)$')
C
C Set up the bottom labels.
C
      CALL AGSETC ('LABEL/NAME.','B')
      CALL AGSETI ('LINE/NUMBER.',-100)
      CALL AGSETC ('LINE/TEXT.','LATITUDE (DEGREES)$')
C
      CALL AGSETC ('LABEL/NAME.','SP')
      CALL AGSETF ('LABEL/BASEPOINT/X.',.000001)
      CALL AGSETF ('LABEL/BASEPOINT/Y.',0.)
      CALL AGSETF ('LABEL/OFFSET/Y.',-.015)
      CALL AGSETI ('LINE/NUMBER.',-100)
      CALL AGSETC ('LINE/TEXT.','SP$')
C
      CALL AGSETC ('LABEL/NAME.','NP')
      CALL AGSETF ('LABEL/BASEPOINT/X.',.999999)
      CALL AGSETF ('LABEL/BASEPOINT/Y.',0.)
      CALL AGSETF ('LABEL/OFFSET/Y.',-.015)
      CALL AGSETI ('LINE/NUMBER.',-100)
      CALL AGSETC ('LINE/TEXT.','NP$')
C
C Set up the top label.
C
      CALL AGSETC ('LABEL/NAME.','T')
      CALL AGSETI ('LINE/NUMBER.',80)
      CALL AGSETC ('LINE/TEXT.','DISTANCE FROM EQUATOR (MILES)$')
      CALL AGSETI ('LINE/NUMBER.',90)
      CALL AGSETC ('LINE/TEXT.',' $')
      CALL AGSETI ('LINE/NUMBER.',100)
      CALL AGSETC ('LINE/TEXT.','LINES OF CONSTANT INCRUDESCENCE$')
      CALL AGSETI ('LINE/NUMBER.',110)
      CALL AGSETC ('LINE/TEXT.','EXAMPLE 7 (EZMXY)$')
C
C Set up centered (box 6) label.
C
      CALL AGSETC ('LABEL/NAME.','EQUATOR')
      CALL AGSETI ('LABEL/ANGLE.',90)
      CALL AGSETI ('LINE/NUMBER.',0)
      CALL AGSETC ('LINE/TEXT.','EQUATOR$')
C
C Draw a boundary around the edge of the plotter frame.
C
c     CALL BNDARY
C
C Draw the graph, using EZMXY.
C
      CALL EZMXY (XDRA,YDRA,101,9,101,0)
C
c     STOP
C
C Format for encode above.
C
c1001 FORMAT ('$$$$$$$$$$$$$$$$$$$$$''J''=''',I1,'''')
C
      END
c
      SUBROUTINE EXMPL8
C
C Define the data arrays.
C
      REAL XDRA(101),YDRA(4,101)
C
C Fill the data arrays.
C
      DO 101 I=1,101
        XDRA(I)=-3.14159265358979+.062831853071796*FLOAT(I-1)
  101 CONTINUE
C
      DO 103 I=1,4
        FLTI=I
        BASE=2.*FLTI-1.
        DO 102 J=1,101
          YDRA(I,J)=BASE+.75*SIN(-3.14159265358979+.062831853071796*
     +                                                  FLTI*FLOAT(J-1))
  102   CONTINUE
  103 CONTINUE
C
C Change the line-end character to a period.
C
      CALL AGSETC ('LINE/END.','.')
C
C Specify labels for x and y axes.
C
      CALL ANOTAT ('SINE FUNCTIONS OF T.','T.',0,0,0,0)
C
C Use a half-axis background.
C
      CALL AGSETI ('BACKGROUND.',3)
C
C Move x axis to the zero point on the y axis.
C
      CALL AGSETF ('BOTTOM/INTERSECTION/USER.',0.)
C
C Specify base value for spacing of major ticks on x axis.
C
      CALL AGSETF ('BOTTOM/MAJOR/BASE.',1.)
C
C Run major ticks on x axis to edge of curve window.
C
      CALL AGSETF ('BOTTOM/MAJOR/INWARD.',1.)
      CALL AGSETF ('BOTTOM/MAJOR/OUTWARD.',1.)
C
C Position x axis minor ticks.
C
      CALL AGSETI ('BOTTOM/MINOR/SPACING.',9)
C
C Run the y axis backward.
C
      CALL AGSETI ('Y/ORDER.',1)
C
C Run plots full-scale in y.
C
      CALL AGSETI ('Y/NICE.',0)
C
C Have AUTOGRAPH scale x and y data the same.
C
      CALL AGSETF ('GRID/SHAPE.',.01)
C
C Use the alphabetic set of dashed-line patterns.
C
      CALL AGSETI ('DASH/SELECTOR.',-1)
C
C Tell AUTOGRAPH how the data arrays are dimensioned.
C
      CALL AGSETI ('ROW.',-1)
C
C Reverse the roles of the x and y arrays.
C
      CALL AGSETI ('INVERT.',1)
C
C Draw a boundary around the edge of the plotter frame.
C
c     CALL BNDARY
C
C Draw the curves.
C
      CALL EZMXY (XDRA,YDRA,4,4,101,'EXAMPLE 8.')
C
c     STOP
C
      END
c
C      SUBROUTINE EXMPL9
CC
CC Define the data arrays.
CC
C      DIMENSION XDAT(400),YDAT(400)
CC
CC Fill the data arrays.
CC
C      DO 101 I=1,400
C        XDAT(I)=(FLOAT(I)-1.)/399.
C  101 CONTINUE
CC
C      CALL GENDAT (YDAT(  1),200,200,1,3,3,+.01,+10.)
C      CALL GENDAT (YDAT(201),200,200,1,3,3,-10.,-.01)
CC
CC The y data ranges over both positive and negative values.  It is
CC desired that both ranges be represented on the same graph and that
CC each be shown logarithmically, ignoring values in the range -.01 to
CC +.01, in which we're not interested.  First we map each y datum into
CC its absolute value (.01 if the absolute value is too small).  Then we
CC take the base-10 logarithm, add 2.0001 (so as to be sure of getting a
CC positive number), and re-attach the original sign.  We can plot the
CC resulting y data on a linear y axis.
CC
C      DO 102 I=1,400
C        YDAT(I)=SIGN(ALOG10(AMAX1(ABS(YDAT(I)),.01))+2.0001,YDAT(I))
C  102 CONTINUE
CC
CC In order that the labels on the y axis should show the original values
CC of the y data, we change the user-system-to-label-system mapping on
CC both y axes and force major ticks to be spaced logarithmically in the
CC label system (which will be defined by the subroutine AGUTOL in such
CC a way as to re-create numbers in the original range).
CC
C      CALL AGSETI ('LEFT/FUNCTION.',1)
C      CALL AGSETI ('LEFT/MAJOR/TYPE.',2)
CC
C      CALL AGSETI ('RIGHT/FUNCTION.',1)
C      CALL AGSETI ('RIGHT/MAJOR/TYPE.',2)
CC
CC Change the label on the left axis to reflect what's going on.
CC
C      CALL AGSETC ('LABEL/NAME.','L')
C      CALL AGSETI ('LINE/NUMBER.',100)
C      CALL AGSETC ('LINE/TEXT.','LOG SCALING, POSITIVE AND NEGATIVE$')
CC
CC Draw a boundary around the edge of the plotter frame.
CC
Cc     CALL BNDARY
CC
CC Draw the curve.
CC
C      CALL EZXY (XDAT,YDAT,400,'EXAMPLE 9$')
CC
Cc     STOP
CC
C      END
Cc
C      SUBROUTINE GENDAT (DATA,IDIM,M,N,MLOW,MHGH,DLOW,DHGH)
CC
CC This is a routine to generate test data for two-dimensional graphics
CC routines.  Given an array "DATA", dimensioned "IDIM x 1", it fills
CC the sub-array ((DATA(I,J),I=1,M),J=1,N) with a two-dimensional field
CC of data having approximately "MLOW" lows and "MHGH" highs, a minimum
CC value of exactly "DLOW" and a maximum value of exactly "DHGH".
CC
CC "MLOW" and "MHGH" are each forced to be greater than or equal to 1
CC and less than or equal to 25.
CC
CC The function used is a sum of exponentials.
CC
C      DIMENSION DATA(IDIM,1),CCNT(3,50)
CC
C      FOVM=9./FLOAT(M)
C      FOVN=9./FLOAT(N)
CC
C      NLOW=MAX0(1,MIN0(25,MLOW))
C      NHGH=MAX0(1,MIN0(25,MHGH))
C      NCNT=NLOW+NHGH
CC
C      DO 101 K=1,NCNT
C        CCNT(1,K)=1.+(FLOAT(M)-1.)*FRAN()
C        CCNT(2,K)=1.+(FLOAT(N)-1.)*FRAN()
C        IF (K.LE.NLOW) THEN
C          CCNT(3,K)=-1.
C        ELSE
C          CCNT(3,K)=+1.
C        END IF
C  101 CONTINUE
CC
C      DMIN=+1.E36
C      DMAX=-1.E36
C      DO 104 J=1,N
C        DO 103 I=1,M
C          DATA(I,J)=.5*(DLOW+DHGH)
C          DO 102 K=1,NCNT
C            DATA(I,J)=DATA(I,J) + .5 * (DHGH-DLOW) * CCNT(3,K) *
C     +                EXP( - ( ( FOVM*(FLOAT(I)-CCNT(1,K)) )**2 +
C     +                         ( FOVN*(FLOAT(J)-CCNT(2,K)) )**2   ) )
C  102     CONTINUE
C          DMIN=AMIN1(DMIN,DATA(I,J))
C          DMAX=AMAX1(DMAX,DATA(I,J))
C  103   CONTINUE
C  104 CONTINUE
CC
C      DO 106 J=1,N
C        DO 105 I=1,M
C          DATA(I,J)=(DATA(I,J)-DMIN)/(DMAX-DMIN)*(DHGH-DLOW)+DLOW
C  105   CONTINUE
C  106 CONTINUE
CC
C      RETURN
CC
C      END
Cc
C      SUBROUTINE XMPL10
C      RETURN
C      END
Cc
      SUBROUTINE XMPL11
C
C Create a sort of histogram.
C
      REAL XDRA(249),YDRA(249),WORK(204),IWRK(204)
C
C Fill the data arrays.  First, we define the histogram outline.  This
C will be used in the call to FILL which fills in the area under the
C histogram.
C
      XDRA(1)=0.
      YDRA(1)=0.
C
      DO 101 I=2,100,2
        XDRA(I  )=XDRA(I-1)
        YDRA(I  )=EXP(-16.*(FLOAT(I/2)/50.-.51)**2)+.1*FRAN()
        XDRA(I+1)=XDRA(I-1)+.02
        YDRA(I+1)=YDRA(I)
  101 CONTINUE
C
      XDRA(102)=1.
      YDRA(102)=0.
C
C Then, we define lines separating the vertical boxes from each other.
C
      NDRA=102
C
      DO 102 I=3,99,2
        XDRA(NDRA+1)=1.E36
        YDRA(NDRA+1)=1.E36
        XDRA(NDRA+2)=XDRA(I)
        YDRA(NDRA+2)=0.
        XDRA(NDRA+3)=XDRA(I)
        YDRA(NDRA+3)=AMIN1(YDRA(I),YDRA(I+1))
        NDRA=NDRA+3
  102 CONTINUE
C
C Draw a boundary around the edge of the plotter frame.
C
c     CALL BNDARY
C
C Suppress the frame advance.
C
      CALL AGSETI ('FRAME.',2)
C
C Draw the graph, using EZXY.
C
      CALL EZXY (XDRA,YDRA,249,'EXAMPLE 11 (HISTOGRAM)$')
C
C Use the XLIB routine FILL to fill the area defined by the data.  Note
C that FILL is not a part of the AUTOGRAPH package.
C
c     CALL FILLOP ('AN',45)
c    CALL FILLOP ('SP',128)
c    CALL FILL (XDRA,YDRA,102,WORK,204,IWRK,204)
C
C Advance the frame.
C
c     CALL FRAME
C
c     STOP
C
      END
c
      SUBROUTINE EXMPLF
C
C Define the data array.
C
      DIMENSION XYCD(226)
C
C Fill the data array.
C
c     READ 1001 , XYCD
C
      DO 101 I=1,226
        IF (XYCD(I).EQ.1.E36) GO TO 101
        XYCD(I)=2.**((XYCD(I)-15.)/2.5)
  101 CONTINUE
C
C Specify log/log plot.
C
      CALL DISPLA (0,0,4)
C
C Bump the line-maximum parameter past 42.
C
      CALL AGSETI ('LINE/MAXIMUM.',50)
C
C Specify x- and y-axis labels, grid background.
C
      CALL ANOTAT ('LOGARITHMIC, BASE 2, EXPONENTIAL LABELING$',
     +             'LOGARITHMIC, BASE 2, NO-EXPONENT LABELING$',2,0,0,0)
C
C Specify the graph label.
C
      CALL AGSETC ('LABEL/NAME.','T')
      CALL AGSETI ('LINE/NUMBER.',100)
      CALL AGSETC ('LINE/TEXT.','FINAL EXAMPLE$')
C
C Specify x-axis ticks and labels.
C
      CALL AGSETI ('BOTTOM/MAJOR/TYPE.',3)
      CALL AGSETF ('BOTTOM/MAJOR/BASE.',2.)
      CALL AGSETI ('BOTTOM/NUMERIC/TYPE.',2)
      CALL AGSETI ('BOTTOM/MINOR/SPACING.',4)
c     CALL AGSETI ('BOTTOM/MINOR/PATTERN.',125252B)
C
C Specify y-axis ticks and labels.
C
      CALL AGSETI ('LEFT/MAJOR/TYPE.',3)
      CALL AGSETF ('LEFT/MAJOR/BASE.',2.)
      CALL AGSETI ('LEFT/NUMERIC/TYPE.',3)
      CALL AGSETI ('LEFT/MINOR/SPACING.',4)
c     CALL AGSETI ('LEFT/MINOR/PATTERN.',125252B)
C
C Compute secondary control parameters.
C
      CALL AGSTUP (XYCD(1),1,0,113,2,XYCD(2),1,0,113,2)
C
C Draw the background.
C
      CALL AGBACK
C
C Draw the curve twice to make it darker.
C
      CALL AGCURV (XYCD(1),2,XYCD(2),2,113,1)
      CALL AGCURV (XYCD(1),2,XYCD(2),2,113,1)
C
C Draw a boundary around the edge of the plotter frame.
C
c     CALL BNDARY
C
C Advance the frame.
C
c     CALL FRAME
C
c     STOP
C
C Format.
C
c1001 FORMAT (14E5.0)
C
      END
C  1.8  2.1  2.7         1.6  4.2  1.5  5.7  1.9  6.3  2.9  6.5  4.7  6.0  6.7
C  5.6  8.6  5.4        10.7  5.6 13.1  4.8 11.2  3.7  9.7 1E36 1E36  7.0  8.2
C  7.7 10.6  8.2        12.6  8.2 14.3  8.0 15.3  7.7 15.6  7.5 15.1  7.4 14.0
C  7.6 12.3  7.7        10.7  7.9  8.9  8.2  7.3  8.5  4.6  8.5  7.3  8.6  9.3
C  8.8 10.2  9.1        10.5  9.4 10.1  9.6  9.1  9.9  7.8 10.3  6.9 11.1  7.0
C 11.7  7.8 12.0         8.6 12.3 10.0 12.5 11.5 12.4 12.7 12.2 13.0 11.9 12.6
C 11.7 11.7 11.6        10.5 11.7  9.3 12.0  8.6 12.5  8.6 13.0  9.0 13.8 10.1
C 14.3 11.1 1E36        1E36 18.5 23.4 18.2 23.5 17.8 23.2 17.2 22.6 16.8 21.8
C 16.0 20.2 15.8        19.5 16.0 19.3 16.6 19.6 17.8 20.6 17.3 19.1 16.9 17.3
C 16.6 16.0 16.6        14.5 16.8 13.7 17.1 13.1 17.8 13.2 18.4 14.0 19.2 15.5
C 19.8 16.8 20.3        18.0 20.9 20.1 21.1 18.9 21.1 17.4 21.1 18.9 21.2 19.7
C  1.5 20.5 21.8        20.8 22.0 20.4 22.1 19.6 22.3 18.7 22.6 18.4 23.1 18.9
C 23.6 20.0 24.1        21.7 24.7 22.9 25.3 23.9 24.7 22.9 24.4 21.6 24.4 20.6
C 24.7 20.2 25.2        20.7 25.6 21.5 26.0 22.9 26.4 24.5 26.7 25.9 26.8 27.9
C 26.6 30.0 26.4        30.3 26.2 30.0 25.7 28.0 25.5 26.1 25.3 24.9 25.3 23.9
C 25.4 22.9 25.9        22.5 26.6 22.4 27.4 23.1 28.2 24.0 29.0 25.0 30.1 26.4
C 1E36 1E36
