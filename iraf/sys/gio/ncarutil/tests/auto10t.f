      SUBROUTINE XMPL10
C
C Define the data arrays.
C
      REAL XDRA(1201),YDRA(1201)
C
C Fill the data arrays.  The independent variable represents time during
C the year (a hypothetical year with equal-length months) and is set up
C so that the minor ticks can be lengthened to delimit the months; the
C major ticks, though shortened to invisibility, will determine where
C the labels go.
C
      DO 101 I=1,1201
        XDRA(I)=FLOAT(I-51)
        YDRA(I)=COSH(FLOAT(I-601)/202.)
  101 CONTINUE
C
C Change the labels on the bottom and left axes.
C
      CALL ANOTAT ('MONTHS OF THE YEAR$','ROMAN NUMERALS$',0,0,0,0)
C
C Fix the minimum and maximum values on both axes and prevent AUTOGRAPH
C from using rounded values at the ends of the axes.
C
      CALL AGSETF ('X/MIN.',-50.)
      CALL AGSETF ('X/MAX.',1150.)
      CALL AGSETI ('X/NICE.',0)
C
      CALL AGSETF ('Y/MIN.',1.)
      CALL AGSETF ('Y/MAX.',10.)
      CALL AGSETI ('Y/NICE.',0)
C
C Specify the spacing between major tick marks on all axes.  Note that
C the AUTOGRAPH dummy routine AGCHNL is supplanted (below) by one which
C supplies dates for the bottom axis and Roman numerals for the left
C axis in place of the numeric labels one would otherwise get.
C
      CALL AGSETI ('  LEFT/MAJOR/TYPE.',1)
      CALL AGSETI (' RIGHT/MAJOR/TYPE.',1)
      CALL AGSETI ('BOTTOM/MAJOR/TYPE.',1)
      CALL AGSETI ('   TOP/MAJOR/TYPE.',1)
C
      CALL AGSETF ('  LEFT/MAJOR/BASE.',  1.)
      CALL AGSETF (' RIGHT/MAJOR/BASE.',  1.)
      CALL AGSETF ('BOTTOM/MAJOR/BASE.',100.)
      CALL AGSETF ('   TOP/MAJOR/BASE.',100.)
C
C Suppress minor ticks on the left and right axes.
C
      CALL AGSETI ('  LEFT/MINOR/SPACING.',0)
      CALL AGSETI (' RIGHT/MINOR/SPACING.',0)
C
C On the bottom and top axes, put one minor tick between each pair of
C major ticks, shorten the major ticks to invisibility, and lengthen
C the minor ticks.  The net effect is to make the minor ticks delimit
C the beginning and end of each month, while the major ticks, though
C invisible, cause the names of the months to be where we want them.
C
      CALL AGSETI ('BOTTOM/MINOR/SPACING.',1)
      CALL AGSETI ('   TOP/MINOR/SPACING.',1)
C
      CALL AGSETF ('BOTTOM/MAJOR/INWARD. ',0.)
      CALL AGSETF ('BOTTOM/MINOR/INWARD. ',.015)
      CALL AGSETF ('   TOP/MAJOR/INWARD. ',0.)
      CALL AGSETF ('   TOP/MINOR/INWARD. ',.015)
C
C Draw a boundary around the edge of the plotter frame.
C
c     CALL BNDARY
C
C Draw the graph, using EZXY.
C
      CALL EZXY (XDRA,YDRA,1201,'EXAMPLE 10 (MODIFIED NUMERIC LABELS)$')
C
c     STOP
C
      END
      SUBROUTINE AGCHNL (IAXS,VILS,CHRM,MCIM,NCIM,IPXM,CHRE,MCIE,NCIE)
C
      CHARACTER*(*) CHRM,CHRE
C
C The routine AGCHNL is called by AGAXIS just after it has set up the
C character strings comprising a numeric label along an axis.  The
C default version does nothing.  A user may supply his own version to
C change the numeric labels.  For each numeric label, this routine is
C called twice by AGAXIS - once to determine how much space will be
C required when the label is actually drawn and once just before it
C is actually drawn.  The arguments are as follows:
C
C - IAXS is the number of the axis being drawn.  Its value is 1, 2, 3,
C   or 4, implying the left, right, bottom, or top axes, respectively.
C   The value of IAXS must not be altered.
C
C - VILS is the value to be represented by the numeric label, in the
C   label system for the axis.  The value of VILS must not be altered.
C
C - CHRM, on entry, is a character string containing the mantissa of the
C   numeric label, as it will appear if AGCHNL makes no changes.  If the
C   numeric label includes a "times" symbol, it will be represented by
C   a blank in CHRM.  (See IPXM, below.)  CHRM may be modified.
C
C - MCIM is the length of CHRM - the maximum number of characters that
C   it will hold.  The value of MCIM must not be altered.
C
C - NCIM, on entry, is the number of meaningful characters in CHRM.  If
C   CHRM is changed, NCIM should be changed accordingly.
C
C - IPXM, on entry, is zero if there is no "times" symbol in CHRM; if it
C   is non-zero, it is the index of the appropriate character position
C   in CHRM.  If AGCHNL changes the position of the "times" symbol in
C   CHRM, removes it, or adds it, the value of IPXM must be changed.
C
C - CHRE, on entry, is a character string containing the exponent of the
C   numeric label, as it will appear if AGCHNL makes no changes.  CHRE
C   may be modified.
C
C - MCIE is the length of CHRE - the maximum number of characters that
C   it will hold.  The value of MCIE must not be altered.
C
C - NCIE, on entry, is the number of meaningful characters in CHRE.  If
C   CHRE is changed, NCIE should be changed accordingly.
C
C Define the names of the months for use on the bottom axis.
C
      CHARACTER*3 MONS(12)
      DATA MONS / 'JAN','FEB','MAR','APR','MAY','JUN',
     +            'JUL','AUG','SEP','OCT','NOV','DEC'/
C
C Modify the numeric labels on the left axis.
C
      IF (IAXS.EQ.1) THEN
        CALL AGCORN (IFIX(VILS),CHRM,NCIM)
        IPXM=0
        NCIE=0
C
C Modify the numeric labels on the bottom axis.
C
      ELSE IF (IAXS.EQ.3) THEN
        IMON=IFIX(VILS+.5)/100+1
        CHRM(1:3)=MONS(IMON)
        NCIM=3
        IPXM=0
        NCIE=0
      END IF
C
C Done.
C
      RETURN
C
      END
      SUBROUTINE AGCORN (NTGR,BCRN,NCRN)
C
      CHARACTER*(*) BCRN
C
C This routine receives an integer in NTGR and returns its Roman-numeral
C equivalent - NCRN characters - in the character variable BCRN.  It
C only works for integers within a limited range and it does some rather
C unorthodox things (like using zero and minus).
C
C ICH1, ICH5, and IC10 are character variables used for the single-unit,
C five-unit, and ten-unit symbols at a given level.
C
      CHARACTER*1 ICH1,ICH5,IC10
C
C Treat numbers outside the range (-4000,+4000) as infinites.
C
      IF (IABS(NTGR).GE.4000) THEN
        IF (NTGR.GT.0) THEN
          NCRN=5
          BCRN(1:5)='(INF)'
        ELSE
          NCRN=6
          BCRN(1:6)='(-INF)'
        END IF
        RETURN
      END IF
C
C Use the symbol '0' for the zero.  The Romans never had it so good.
C
      IF (NTGR.EQ.0) THEN
        NCRN=1
        BCRN(1:1)='0'
        RETURN
      END IF
C
C Zero the character counter.
C
      NCRN=0
C
C Handle negative integers by prefixing a minus sign.
C
      IF (NTGR.LT.0) THEN
        NCRN=NCRN+1
        BCRN(NCRN:NCRN)='-'
      END IF
C
C Initialize some constants.  We'll check for thousands first.
C
      IMOD=10000
      IDIV=1000
      ICH1='M'
C
C Find out how many thousands (hundreds, tens, units) there are and jump
C to the proper code block for each case.
C
  101 INTG=MOD(IABS(NTGR),IMOD)/IDIV
C
      GO TO (107,104,104,104,102,103,103,103,103,106) , INTG+1
C
C Four - add ICH1 followed by ICH5.
C
  102 NCRN=NCRN+1
      BCRN(NCRN:NCRN)=ICH1
C
C Five through eight - add ICH5, followed by INTG-5 ICH1's.
C
  103 NCRN=NCRN+1
      BCRN(NCRN:NCRN)=ICH5
C
      INTG=INTG-5
      IF (INTG.LE.0) GO TO 107
C
C One through three - add that many ICH1's.
C
  104 DO 105 I=1,INTG
        NCRN=NCRN+1
        BCRN(NCRN:NCRN)=ICH1
  105 CONTINUE
C
      GO TO 107
C
C Nine - add ICH1, followed by IC10.
C
  106 NCRN=NCRN+1
      BCRN(NCRN:NCRN)=ICH1
      NCRN=NCRN+1
      BCRN(NCRN:NCRN)=IC10
C
C If we're done, exit.
C
  107 IF (IDIV.EQ.1) RETURN
C
C Otherwise, tool up for the next digit and loop back.
C
      IMOD=IMOD/10
      IDIV=IDIV/10
      IC10=ICH1
C
      IF (IDIV.EQ.100) THEN
        ICH5='D'
        ICH1='C'
      ELSE IF (IDIV.EQ.10) THEN
        ICH5='L'
        ICH1='X'
      ELSE
        ICH5='V'
        ICH1='I'
      END IF
C
      GO TO 101
C
      END
