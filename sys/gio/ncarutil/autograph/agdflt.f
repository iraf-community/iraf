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
C ---------------------------------------------------------------------
C
c +noao:  blockdata rewritten to be run time initialization
c     BLOCK DATA AGDFLT
      subroutine agdflt
C
C The block data subroutine AGDFLT defines the default values of those
C AUTOGRAPH parameters which can be declared in a DATA statement.  See
C AGINIT for code initializing other AUTOGRAPH parameters.
C
C Following are declarations of all the AUTOGRAPH common blocks.
C
C The following common block contains the AUTOGRAPH control parameters,
C all of which are real.  If it is changed, all of AUTOGRAPH (especially
C the routine AGSCAN) must be examined for possible side effects.
C
      COMMON /AGCONP/ QFRA,QSET,QROW,QIXY,QWND,QBAC , SVAL(2) ,
     +                XLGF,XRGF,YBGF,YTGF , XLGD,XRGD,YBGD,YTGD , SOGD ,
     +                XMIN,XMAX,QLUX,QOVX,QCEX,XLOW,XHGH ,
     +                YMIN,YMAX,QLUY,QOVY,QCEY,YLOW,YHGH ,
     +                QDAX(4),QSPA(4),PING(4),PINU(4),FUNS(4),QBTD(4),
     +                BASD(4),QMJD(4),QJDP(4),WMJL(4),WMJR(4),QMND(4),
     +                QNDP(4),WMNL(4),WMNR(4),QLTD(4),QLED(4),QLFD(4),
     +                QLOF(4),QLOS(4),DNLA(4),WCLM(4),WCLE(4) ,
     +                QODP,QCDP,WOCD,WODQ,QDSH(26) ,
     +                     QDLB,QBIM,FLLB(10,8),QBAN ,
     +                QLLN,TCLN,QNIM,FLLN(6,16),QNAN ,
     +                XLGW,XRGW,YBGW,YTGW , XLUW,XRUW,YBUW,YTUW ,
     +                XLCW,XRCW,YBCW,YTCW , WCWP,HCWP,SCWP ,
     +                XBGA(4),YBGA(4),UBGA(4),XNDA(4),YNDA(4),UNDA(4),
     +                QBTP(4),BASE(4),QMNT(4),QLTP(4),QLEX(4),QLFL(4),
     +                QCIM(4),QCIE(4),RFNL(4),WNLL(4),WNLR(4),WNLB(4),
     +                WNLE(4),QLUA(4) ,
     +                RBOX(6),DBOX(6,4),SBOX(6,4)
C
C The following common block contains other AUTOGRAPH variables, both
C real and integer, which are not control parameters.
C
      COMMON /AGORIP/ SMRL , ISLD , MWCL,MWCM,MWCE,MDLA,MWCD,MWDQ ,
     +                INIF
C
C The following common block contains other AUTOGRAPH variables, of
C type character.
C
      COMMON /AGOCHP/ CHS1,CHS2
C
c+noao
c     CHARACTER*504 CHS1,CHS2
      CHARACTER*500 CHS1,CHS2
c-noao
C
C The following common blocks contain variables which are required for
C the character-storage-and-retrieval scheme of AUTOGRAPH.
C
      COMMON /AGCHR1/ LNIC,INCH(2,50),LNCA,INCA
C
      COMMON /AGCHR2/ CHRA(2000)
C
      CHARACTER*1 CHRA
C
c +noao:  logical flag added to prevent "over-initialization"
      logical first
      data first /.true./
      call utilbd
      if (.not. first) return
      first = .false.
c -noao
C ---------------------------------------------------------------------
C
C Following are declarations of default values of variables in the
C AUTOGRAPH common blocks.
C
C ---------------------------------------------------------------------
C
C QFRA defines the control parameter 'FRAME.', which specifies when, if
C ever, the EZ... routines are to call FRAME to advance to a new frame.
C
c     DATA QFRA / 1. /
      QFRA = 1. 
C
C QSET defines the control parameter 'SET.', which determines how the
C last call to the plot-package routine "SET" is to affect AUTOGRAPH.
C
c     DATA QSET / 1. /
      QSET = 1.
C
C QROW defines the control parameter 'ROW.', which determines how the x
C and y input arrays (in calls to AGSTUP and AGCURV) are to be used.
C
c     DATA QROW / 1. /
      QROW = 1.
C
C QIXY defines the control parameter 'INVERT.', which, if set non-zero,
C causes the routines AGSTUP and AGCURV to behave as if the arguments
C defining the x and y data had been interchanged.
C
c     DATA QIXY / 0. /
      QIXY = 0.
C
C QWND defines the control parameter 'WINDOW.', which, if set non-zero,
C causes curves drawn to be scissored by the edge of the curve window.
C
c     DATA QWND / 0. /
      QWND = 0.
C
C QBAC defines the control parameter 'BACKGROUND.', which can be given
C any of four values to set up four specific types of plot background.
C
c     DATA QBAC / 1. /
      QBAC = 1.
C
C SVAL defines the control parameters 'NULL/1.' and 'NULL/2.', which are
C used in various ways by AUTOGRAPH.
C
c     DATA SVAL(1) / 1E36 / , SVAL(2) / 2E36 /
      SVAL(1) = 1E36 
      SVAL(2) = 2E36
C
C XLGF, XRGF, YBGF, and YTGF define the parameter-group 'GRAPH.'; they
C specify the position of the graph window within the plotter frame.
C
c     DATA XLGF / 0. / , XRGF / 1. / , YBGF / 0. / , YTGF / 1. /
      XLGF = 0. 
      XRGF = 1.
      YBGF = 0.
      YTGF = 1.
C
C XLGD, XRGD, YBGD, and YTGD define the first four parameters in the
C group 'GRID.'; they specify the position of the grid window within
C the graph window.
C
c     DATA XLGD / .15 / , XRGD / .95 / , YBGD / .15 / , YTGD / .95 /
      XLGD = .15
      XRGD = .95 
      YBGD = .15 
      YTGD = .95
C
C SOGD defines the control parameter 'GRID/SHAPE.', which defines the
C shape of the grid window.
C
c     DATA SOGD / 0. /
      SOGD = 0.
C
C XMIN and XMAX define the control parameters 'X/MIN.' and 'X/MAX.',
C which determine how minimum and maximum values of x are to be chosen.
C Null values imply that AUTOGRAPH is to choose real values; non-null
C values are the actual values to be used (perhaps after rounding).
C
c     DATA XMIN / 1E36 / , XMAX / 1E36 /
      XMIN = 1E36
      XMAX = 1E36
C
C QLUX defines the control parameter 'X/LOG.', which is set non-zero to
C specify that the horizontal axis is to be logarithmic.
C
c     DATA QLUX / 0. /
      QLUX = 0.
C
C QOVX defines the control parameter 'X/ORDER.', which is set non-zero
C to flip the horizontal axis end-for-end.
C
c     DATA QOVX / 0. /
      QOVX = 0.
C
C QCEX defines the control parameter 'X/NICE.', which determines which,
C if either, of the horizontal axes is to have "nice" (rounded) values
C at its ends.
C
c     DATA QCEX / -1. /
      QCEX = -1.
C
C XLOW and XHGH define the control parameters 'X/SMALLEST.' and
C 'X/LARGEST.'; they come into play only when XMIN and/or XMAX are null
C and they are non-null, in which case they set limits on the range of
C x data to be considered when choosing the minimum and/or maximum.
C
c     DATA XLOW / 1E36 / , XHGH / 1E36 /
      XLOW = 1E36
      XHGH = 1E36
C
C YMIN and YMAX define the control parameters 'Y/MIN.' and 'Y/MAX.',
C which determine how minimum and maximum values of y are to be chosen.
C Null values imply that AUTOGRAPH is to choose real values; non-null
C values are the actual values to be used (perhaps after rounding).
C
c     DATA YMIN / 1E36 / , YMAX / 1E36 /
      YMIN = 1E36
      YMAX = 1E36
C
C QLUY defines the control parameter 'Y/LOG.', which is set non-zero to
C specify that the horizontal axis is to be logarithmic.
C
c     DATA QLUY / 0. /
      QLUY = 0.
C
C QOVY defines the control parameter 'Y/ORDER.', which is set non-zero
C to flip the horizontal axis end-for-end.
C
c     DATA QOVY / 0. /
      QOVY = 0.
C
C QCEY defines the control parameter 'Y/NICE.', which determines which,
C if either, of the horizontal axes is to have "nice" (rounded) values
C at its ends.
C
c     DATA QCEY / -1. /
      QCEY = -1.
C
C YLOW and YHGH define the control parameters 'Y/SMALLEST.' and
C 'Y/LARGEST.'; they come into play only when YMIN and/or YMAX are null
C and they are non-null, in which case they set limits on the range of
C y data to be considered when choosing the minimum and/or maximum.
C
c     DATA YLOW / 1E36 / , YHGH / 1E36 /
      YLOW = 1E36
      YHGH = 1E36
C
C QDAX(i) defines the control parameters 'AXIS/s/CONTROL.' (i=1 implies
C s='LEFT', i=2 implies s='RIGHT', i=3 implies s='BOTTOM', i=4 implies
C s='TOP').  Each of these specifies whether or not a given axis will
C be drawn or not and what liberties may be taken with numeric labels
C on the axis.
C
c     DATA QDAX(1)/ 4. / , QDAX(2)/ 4. / , QDAX(3)/ 4. / , QDAX(4)/ 4. /
      QDAX(1) = 4.
      QDAX(2) = 4.
      QDAX(3) = 4.
      QDAX(4) = 4.
C
C Each QSPA(i) defines a control parameter 'AXIS/s/LINE.', which says
C whether or not the line portion of a particular axis is to be drawn.
C
c     DATA QSPA(1)/ 0. / , QSPA(2)/ 0. / , QSPA(3)/ 0. / , QSPA(4)/ 0. /
      QSPA(1) = 0.
      QSPA(2) = 0.
      QSPA(3) = 0.
      QSPA(4) = 0.
C
C Each PING(i) defines a control parameter 'AXIS/s/INTERSECTION/GRID.',
C which may be used to move a particular axis to a specified position.
C
c     DATA PING(1)/1E36/ , PING(2)/1E36/ , PING(3)/1E36/ , PING(4)/1E36/
      PING(1) = 1E36
      PING(2) = 1E36
      PING(3) = 1E36
      PING(4) = 1E36
C
C Each PINU(i) defines a control parameter 'AXIS/s/INTERSECTION/USER.',
C which may be used to move a particular axis to a specified position.
C
c     DATA PINU(1)/1E36/ , PINU(2)/1E36/ , PINU(3)/1E36/ , PINU(4)/1E36/
      PINU(1) = 1E36
      PINU(2) = 1E36
      PINU(3) = 1E36
      PINU(4) = 1E36
C
C Each FUNS(i) defines a control parameter 'AXIS/s/FUNCTION.', which is
C used within a user-supplied version of AGUTOL to select a particular
C uset-system-to-label-system mapping for a particular axis.  The
C default value selects the identity mapping.
C
c     DATA FUNS(1)/ 0. / , FUNS(2)/ 0. / , FUNS(3)/ 0. / , FUNS(4)/ 0. /
      FUNS(1) =  0.
      FUNS(2) =  0.
      FUNS(3) =  0.
      FUNS(4) =  0.
C
C The values of QBTD(i), BASD(i), QMJD(i), QJDP(i), WMJL(i), and WMJR(i)
C together define the control-parameter group 'AXIS/s/TICKS/MAJOR.',
C which determines the positioning and appearance of the major ticks on
C a particular axis.
C
c     DATA QBTD(1)/1E36/ , QBTD(2)/1E36/ , QBTD(3)/1E36/ , QBTD(4)/1E36/
c     DATA BASD(1)/1E36/ , BASD(2)/1E36/ , BASD(3)/1E36/ , BASD(4)/1E36/
c     DATA QMJD(1)/ 6. / , QMJD(2)/ 6. / , QMJD(3)/ 6. / , QMJD(4)/ 6. /
c     DATA QJDP(1)/1E36/ , QJDP(2)/1E36/ , QJDP(3)/1E36/ , QJDP(4)/1E36/
c     DATA WMJL(1)/ 0. / , WMJL(2)/ 0. / , WMJL(3)/ 0. / , WMJL(4)/ 0. /
c     DATA WMJR(1)/.015/ , WMJR(2)/.015/ , WMJR(3)/.015/ , WMJR(4)/.015/
      QBTD(1) = 1E36
      QBTD(2) = 1E36
      QBTD(3) = 1E36
      QBTD(4) = 1E36
      BASD(1) = 1E36
      BASD(2) = 1E36
      BASD(3) = 1E36
      BASD(4) = 1E36
      QMJD(1) =  6. 
      QMJD(2) =  6. 
      QMJD(3) =  6. 
      QMJD(4) =  6. 
      QJDP(1) = 1E36
      QJDP(2) = 1E36
      QJDP(3) = 1E36
      QJDP(4) = 1E36
      WMJL(1) =  0. 
      WMJL(2) =  0. 
      WMJL(3) =  0. 
      WMJL(4) =  0. 
      WMJR(1) = .015
      WMJR(2) = .015
      WMJR(3) = .015
      WMJR(4) = .015
C
C The values of QMND(i), QNDP(i), WMNL(i), and WMNR(i) together define
C the control-parameter group 'AXIS/s/TICKS/MINOR.', which determines
C the positioning and appearance of the major ticks on a particular
C axis.
C
c     DATA QMND(1)/1E36/ , QMND(2)/1E36/ , QMND(3)/1E36/ , QMND(4)/1E36/
c     DATA QNDP(1)/1E36/ , QNDP(2)/1E36/ , QNDP(3)/1E36/ , QNDP(4)/1E36/
c     DATA WMNL(1)/ 0. / , WMNL(2)/ 0. / , WMNL(3)/ 0. / , WMNL(4)/ 0. /
c     DATA WMNR(1)/.010/ , WMNR(2)/.010/ , WMNR(3)/.010/ , WMNR(4)/.010/
      QMND(1) = 1E36
      QMND(2) = 1E36
      QMND(3) = 1E36
      QMND(4) = 1E36
      QNDP(1) = 1E36
      QNDP(2) = 1E36
      QNDP(3) = 1E36
      QNDP(4) = 1E36
      WMNL(1) =  0. 
      WMNL(2) =  0. 
      WMNL(3) =  0. 
      WMNL(4) =  0. 
      WMNR(1) = .010
      WMNR(2) = .010
      WMNR(3) = .010
      WMNR(4) = .010
C
C The values of QLTD(i), QLED(i), QLFD(i), QLOF(i), QLOS(i), DNLA(i),
C WCLM(i), and WCLE(i) together define the control-parameter group
C 'AXIS/s/NUMERIC.', which determines the positioning and appearance of
C the numeric labels on a particular axis.
C
c     DATA QLTD(1)/1E36/ , QLTD(2)/  0./ , QLTD(3)/1E36/ , QLTD(4)/  0./
c     DATA QLED(1)/1E36/ , QLED(2)/1E36/ , QLED(3)/1E36/ , QLED(4)/1E36/
c     DATA QLFD(1)/1E36/ , QLFD(2)/1E36/ , QLFD(3)/1E36/ , QLFD(4)/1E36/
c     DATA QLOF(1)/ 0. / , QLOF(2)/ 0. / , QLOF(3)/ 0. / , QLOF(4)/ 0. /
c     DATA QLOS(1)/ 90./ , QLOS(2)/ 90./ , QLOS(3)/ 90./ , QLOS(4)/ 90./
c     DATA DNLA(1)/.015/ , DNLA(2)/.015/ , DNLA(3)/.015/ , DNLA(4)/.015/
c     DATA WCLM(1)/.015/ , WCLM(2)/.015/ , WCLM(3)/.015/ , WCLM(4)/.015/
c     DATA WCLE(1)/.010/ , WCLE(2)/.010/ , WCLE(3)/.010/ , WCLE(4)/.010/
      QLTD(1) = 1E36
      QLTD(2) =   0.
      QLTD(3) = 1E36
      QLTD(4) =   0.
      QLED(1) = 1E36
      QLED(2) = 1E36
      QLED(3) = 1E36
      QLED(4) = 1E36
      QLFD(1) = 1E36
      QLFD(2) = 1E36
      QLFD(3) = 1E36
      QLFD(4) = 1E36
      QLOF(1) =  0. 
      QLOF(2) =  0. 
      QLOF(3) =  0. 
      QLOF(4) =  0. 
      QLOS(1) =  90.
      QLOS(2) =  90.
      QLOS(3) =  90.
      QLOS(4) =  90.
      DNLA(1) = .015
      DNLA(2) = .015
      DNLA(3) = .015
      DNLA(4) = .015
      WCLM(1) = .015
      WCLM(2) = .015
      WCLM(3) = .015
      WCLM(4) = .015
      WCLE(1) = .010
      WCLE(2) = .010
      WCLE(3) = .010
      WCLE(4) = .010
C
C QODP defines the control parameter 'DASH/SELECTOR.', the sign of which
C determines which set of dash patterns is used by EZMY and EZMXY (the
C alphabetic set or the user-specified set); if the user-specified set
C is selected, the magnitude of QODP determines how many of them are to
C be used.
C
c     DATA QODP / 1. /
      QODP = 1. 
C
C QCDP defines the control parameter 'DASH/LENGTH.', which specifies the
C assumed length of dash patterns tendered to AUTOGRAPH.
C
c     DATA QCDP / 8. /
      QCDP =  8. 
C
C WOCD and WODQ define the control parameters 'DASH/CHARACTER.' and
C 'DASH/DOLLAR-QUOTE.', which specify the widths of characters used in
C character-string dash patterns.
C
c     DATA WOCD / .010 / , WODQ / .010 /
      WOCD = .010 
      WODQ = .010
C
C QDSH defines the control-parameter group 'DASH/PATTERN.'.  Each value,
C if positive, defines a binary dash pattern, and, if negative, serves
C as an identifier in retrieving a character-string dash pattern.
C
c     DATA QDSH / 26*65535. /
      do 20, ijk = 1, 26
  20      QDSH(ijk) = 65535.
C
C QDLB defines the control parameter 'LABEL/CONTROL.', which specifies
C what may be done with informational labels in response to overlap
C problems.
C
c     DATA QDLB /2./
      QDLB = 2.
C
C QBIM defines the control parameter 'LABEL/BUFFER/LENGTH.' and must
C be equal to the second dimension of the array FLLB.
C
c     DATA QBIM / 8. /
      QBIM =  8.
C
C QBAN defines the control parameter 'LABEL/NAME.'; its value is really
C a pointer into the label list.  The default value, zero, means that
C the pointer has not been set.
C
c     DATA QBAN / 0. /
      QBAN =  0.
C
C QLLN defines the control parameter 'LINE/MAXIMUM.' - the assumed
C maximum length of character strings intended for use as the text of a
C line of a label.
C
c     DATA QLLN /40./
      QLLN = 40.
C
C TCLN defines the control parameter 'LINE/TERMINATOR.' - which is used
C to mark the end of character strings intended for use as the text of a
C line of a label.  It is initialized in AGINIT.
C
C QNIM defines the control parameter 'LINE/BUFFER/LENGTH.' and must be
C equal to the second dimension of FLLN.
C
c     DATA QNIM / 16. /
      QNIM =  16.
C
C QNAN defines the control parameter 'LINE/NUMBER.'; its value is really
C a pointer into the line list.  The default value, zero, says that the
C pointer has not been set.
C
c     DATA QNAN / 0. /
      QNAN =  0.
C
C (FLLB(I,1),I=1,10) and (FLLN(I,1),I=1,6) define the label to the left
C of the grid.  The name, in FLLB(1,1), and the line text, in FLLN(4,1),
C must be filled in by AGINIT.
C
c     DATA FLLB( 1,1)/   0./ , FLLB( 2,1)/   0./ , FLLB( 3,1)/   0./ ,
c    +     FLLB( 4,1)/   .5/ , FLLB( 5,1)/-.015/ , FLLB( 6,1)/   0./ ,
c    +     FLLB( 7,1)/  90./ , FLLB( 8,1)/   0./ , FLLB( 9,1)/   1./ ,
c    +     FLLB(10,1)/   1./ , FLLN( 1,1)/+100./ , FLLN( 2,1)/   0./ ,
c    +     FLLN( 3,1)/ .015/ , FLLN( 4,1)/  -2./ , FLLN( 5,1)/   1./ ,
c    +     FLLN( 6,1)/   0./
           FLLB( 1,1) = 0.
           FLLB( 2,1) = 0.
           FLLB( 3,1) = 0.
           FLLB( 4,1) = .5
           FLLB( 5,1) = -.015
           FLLB( 6,1) = 0.
           FLLB( 7,1) = 90.
           FLLB( 8,1) = 0.
           FLLB( 9,1) = 1.
           FLLB(10,1) = 1.
           FLLN( 1,1) = +100.
           FLLN( 2,1) = 0.
           FLLN( 3,1) = .015
           FLLN( 4,1) = -2.
           FLLN( 5,1) = 1.
           FLLN( 6,1) = 0.
C
C (FLLB(I,2),I=1,10) and (FLLN(I,2),I=1,6) define the label to the right
C of the grid.  The name, in FLLB(1,2), and the line text, in FLLN(4,2),
C must be filled in by AGINIT.
C
c     DATA FLLB( 1,2)/   0./ , FLLB( 2,2)/   0./ , FLLB( 3,2)/   1./ ,
c    +     FLLB( 4,2)/   .5/ , FLLB( 5,2)/+.015/ , FLLB( 6,2)/   0./ ,
c    +     FLLB( 7,2)/  90./ , FLLB( 8,2)/   0./ , FLLB( 9,2)/   1./ ,
c    +     FLLB(10,2)/   2./ , FLLN( 1,2)/-100./ , FLLN( 2,2)/   0./ ,
c    +     FLLN( 3,2)/ .015/ , FLLN( 4,2)/  -3./ , FLLN( 5,2)/   0./ ,
c    +     FLLN( 6,2)/   0./
           FLLB( 1,2) =  0.
           FLLB( 2,2) =  0.
           FLLB( 3,2) =  1.
           FLLB( 4,2) =  .5
           FLLB( 5,2) = +.015
           FLLB( 6,2) =  0.
           FLLB( 7,2) = 90.
           FLLB( 8,2) =  0.
           FLLB( 9,2) =  1.
           FLLB(10,2) =  2.
           FLLN( 1,2) = -100.
           FLLN( 2,2) =  0.
           FLLN( 3,2) =  .015
           FLLN( 4,2) =  -3.
           FLLN( 5,2) =  0.
           FLLN( 6,2) =  0.
C
C (FLLB(I,3),I=1,10) and (FLLN(I,3),I=1,6) define the label below the
C grid.  The name, in FLLB(1,3), and the line text, in FLLN(4,3), must
C be filled in by AGINIT.
C
c     DATA FLLB( 1,3)/   0./ , FLLB( 2,3)/   0./ , FLLB( 3,3)/   .5/ ,
c    +     FLLB( 4,3)/   0./ , FLLB( 5,3)/   0./ , FLLB( 6,3)/-.015/ ,
c    +     FLLB( 7,3)/   0./ , FLLB( 8,3)/   0./ , FLLB( 9,3)/   1./ ,
c    +     FLLB(10,3)/   3./ , FLLN( 1,3)/-100./ , FLLN( 2,3)/   0./ ,
c    +     FLLN( 3,3)/ .015/ , FLLN( 4,3)/  -1./ , FLLN( 5,3)/   1./ ,
c    +     FLLN( 6,3)/   0./
           FLLB( 1,3) =  0.
           FLLB( 2,3) =  0.
           FLLB( 3,3) = .5
           FLLB( 4,3) =  0.
           FLLB( 5,3) =  0.
           FLLB( 6,3) = -.015
           FLLB( 7,3) =  0.
           FLLB( 8,3) =  0.
           FLLB( 9,3) =  1.
           FLLB(10,3) =  3.
           FLLN( 1,3) = -100.
           FLLN( 2,3) =  0.
           FLLN( 3,3) =  .015
           FLLN( 4,3) =   -1.
           FLLN( 5,3) =  1.
           FLLN( 6,3) =  0.
C
C (FLLB(I,4),I=1,10) and (FLLN(I,4),I=1,6) define the label above the
C grid.  The name, in FLLB(1,4), and the line text, in FLLN(4,4), must
C be filled in by AGINIT.
C
c     DATA FLLB( 1,4)/   0./ , FLLB( 2,4)/   0./ , FLLB( 3,4)/   .5/ ,
c    +     FLLB( 4,4)/   1./ , FLLB( 5,4)/   0./ , FLLB( 6,4)/+.020/ ,
c    +     FLLB( 7,4)/   0./ , FLLB( 8,4)/   0./ , FLLB( 9,4)/   1./ ,
c    +     FLLB(10,4)/   4./ , FLLN( 1,4)/+100./ , FLLN( 2,4)/   0./ ,
c    +     FLLN( 3,4)/ .020/ , FLLN( 4,4)/  -3./ , FLLN( 5,4)/   0./ ,
c    +     FLLN( 6,4)/   0./
           FLLB( 1,4) =  0.
           FLLB( 2,4) =  0.
           FLLB( 3,4) =  .5
           FLLB( 4,4) =  1.
           FLLB( 5,4) =  0.
           FLLB( 6,4) = +.020
           FLLB( 7,4) =  0.
           FLLB( 8,4) =  0.
           FLLB( 9,4) =  1.
           FLLB(10,4) =  4.
           FLLN( 1,4) = +100.
           FLLN( 2,4) =  0.
           FLLN( 3,4) =  .020
           FLLN( 4,4) =  -3.
           FLLN( 5,4) =  0.
           FLLN( 6,4) =  0.
C
C Certain secondary parameters must be initialized to prevent bombing.
C
c     DATA QBTP(1)/  0./ , QBTP(2)/  0./ , QBTP(3)/  0./ , QBTP(4)/  0./
c     DATA BASE(1)/  0./ , BASE(2)/  0./ , BASE(3)/  0./ , BASE(4)/  0./
c     DATA QMNT(1)/  0./ , QMNT(2)/  0./ , QMNT(3)/  0./ , QMNT(4)/  0./
c     DATA QLTP(1)/  0./ , QLTP(2)/  0./ , QLTP(3)/  0./ , QLTP(4)/  0./
c     DATA QLEX(1)/  0./ , QLEX(2)/  0./ , QLEX(3)/  0./ , QLEX(4)/  0./
c     DATA QLFL(1)/  0./ , QLFL(2)/  0./ , QLFL(3)/  0./ , QLFL(4)/  0./
c     DATA QCIM(1)/  0./ , QCIM(2)/  0./ , QCIM(3)/  0./ , QCIM(4)/  0./
c     DATA QCIE(1)/  0./ , QCIE(2)/  0./ , QCIE(3)/  0./ , QCIE(4)/  0./
c     DATA RFNL(1)/  0./ , RFNL(2)/  0./ , RFNL(3)/  0./ , RFNL(4)/  0./
c     DATA QLUA(1)/  0./ , QLUA(2)/  0./ , QLUA(3)/  0./ , QLUA(4)/  0./
      QBTP(1) =   0.
      QBTP(2) =   0.
      QBTP(3) =   0.
      QBTP(4) =   0.
      BASE(1) =   0.
      BASE(2) =   0.
      BASE(3) =   0.
      BASE(4) =   0.
      QMNT(1) =   0.
      QMNT(2) =   0.
      QMNT(3) =   0.
      QMNT(4) =   0.
      QLTP(1) =   0.
      QLTP(2) =   0.
      QLTP(3) =   0.
      QLTP(4) =   0.
      QLEX(1) =   0.
      QLEX(2) =   0.
      QLEX(3) =   0.
      QLEX(4) =   0.
      QLFL(1) =   0.
      QLFL(2) =   0.
      QLFL(3) =   0.
      QLFL(4) =   0.
      QCIM(1) =   0.
      QCIM(2) =   0.
      QCIM(3) =   0.
      QCIM(4) =   0.
      QCIE(1) =   0.
      QCIE(2) =   0.
      QCIE(3) =   0.
      QCIE(4) =   0.
      RFNL(1) =   0.
      RFNL(2) =   0.
      RFNL(3) =   0.
      RFNL(4) =   0.
      QLUA(1) =   0.
      QLUA(2) =   0.
      QLUA(3) =   0.
      QLUA(4) =   0.
C
C SMRL and ISLD are set by the routine AGINIT (which see, below).
C
C MWCL, MWCM, MWCE, MDLA, MWCD, and MWDQ are the minimum widths of label
C characters, mantissa characters, exponent characters, label-to-axis
C distances, dash-pattern characters, and dash-pattern spaces, respect-
C ively (in the plotter coordinate system).
C
c     DATA MWCL /8/, MWCM /8/, MWCE /8/, MDLA /8/, MWCD /8/, MWDQ /8/
      MWCL = 8
      MWCM = 8
      MWCE = 8
      MDLA = 8
      MWCD = 8
      MWDQ = 8
C
C INIF is an initialization flag, set non-zero to indicate that the
C routine AGINIT has been executed to set the values of AUTOGRAPH
C parameters which, for one reason or another, cannot be preset by
C this block data routine.
C
c     DATA INIF / 0 /
      INIF =  0 
C
C CHS1 and CHS2 are used within AUTOGRAPH when manipulating character
C strings retrieved by calls to AGGTCH.  They need not be preset.
C
C LNIC is the second dimension of the array (INCH) which holds an index
C of the character strings stored by AGSTCH.
C
c     DATA LNIC / 50 /
      LNIC = 50 
C
C INCH is an index of character strings currently stored in CHRA.  Each
C entry has the following format:
C
C    INCH(1,I), if non-zero, is the index, in the array CHRA, of the
C    first character of the Ith character string.
C
C    INCH(2,I) is the length of the Ith character string.
C
c     DATA (INCH(1,I),I=1,50) / 50*0 /
c     DATA (INCH(2,I),I=1,50) / 50*0 /
      do 10, ijk = 1, 50
        inch (1, ijk) = 0
        inch (2, ijk) = 0
  10  continue
C
C LNCA is the size of the array (CHRA) in which AGSTCH stores character
C strings.
C
c     DATA LNCA / 2000 /
      LNCA =  2000 
C
C INCA is the index of the last character used in CHRA.
C
c     DATA INCA / 0 /
      INCA =  0 
C
C CHRA holds character strings stored by AGSTCH.  It need not be pre-set
C to anything.
C
      return
c 
      entry initag
      first = .true.
      END
