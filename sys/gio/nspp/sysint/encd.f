      SUBROUTINE ENCD (VALU,ASH,IOUT,NC,IOFFD)
C
C
C
C
C ON INPUT     VALU	 FLOATING POINT	NUMBER FROM WHICH THE LABEL IS
C			 TO BE CREATED.
C	       ASH	 SEE IOFFD.
C	       IOFFD	 IF IOFFD .EQ. 0, A LABEL WHICH	REFLECTS THE
C			      MAGNITUDE	OF VALU	IS TO BE CREATED.
C			      .1 .LE. ABS(VALU)	.LE. 99999.49999...
C			      OR VALUE .EQ. 0.0.  THE LABEL CREATED
C			      SHOULD HAVE 3 TO 5 CHARACTERS DEPENDING
C			      ON THE MAGNITUDE OF VALU.	 SEE IOUT.
C			 IF IOFFD .NE. 0, A LABEL WHICH	DOES NOT REFLECT
C			      THE MAGNITUDE OF VALU IS TO BE CREATED.
C			      ASH IS USED AS THE NORMALIZATION FACTOR.
C			      1. .LE. ASH*ABS(VALU) .LT. 1000. OR
C			      VALU .EQ.	0.0.  THE LABEL	CREATED	SHOULD
C			      HAVE 1 TO	3 CHARACTERS, DEPENDING	ON THE
C			      MAGNITUDE	OF ASH*VALU.  SEE IOUT.
C ON OUTPUT    IOUT	 CONTAINS THE LABEL CREATED.  IT SHOULD	HAVE NO
C			 LEADING BLANKS.  SEE NC.
C	       NC	 THE NUMBERS IN	THE LABEL IN IOUT.  SHOULD BE
C			 1 TO 5.
C
      SAVE
      CHARACTER*11 IFMT, IOUT
C
C IFMT MUST HOLD 11 CHARACTERS
C
      VAL = VALU
      IF (IOFFD	.NE. 0)	GO TO 103
      IF (VAL) 101,104,101
  101 LOG = IFIX((ALOG10(ABS(VAL))+.00001)+5000.)-5000
      V	= VAL
      NS = MAX0(4,MIN0(6,LOG+2))
      ND = MIN0(3,MAX0(0,2-LOG))
c     IF (VAL.LT.0)  NS	= NS + 1
c +noao: replacing ftn i/o for iraf implementation
c 102 WRITE (IFMT,'(A2,I2,A1,I1,A1)') '(F',NS,'.',ND,')'
  102 continue
c     if (len (char (ns + ichar ('0'))) .eq. 2) then
c	 ifmt(1:7) = '(f  . )'
c	 ifmt(3:4) = char (ns + ichar ('0'))
c	 ifmt(6:6) = char (nd + ichar ('0'))
c     else 
c	 ifmt(1:6) = '(f . )'
c	 ifmt(3:3) = char (ns + ichar ('0'))
c	 ifmt(5:5) = char (nd + ichar ('0'))
c     endif
c     WRITE (IOUT,IFMT)	V
      call encode (ns, ifmt, iout, v)
      NC = NS
c +noao
c The following statement was making 5 digit labels (+4800) come out
c truncated (+480) and it has been commented out.
c     IF (LOG.GE.3)  NC	= NC - 1
c -noao
      RETURN
  103 NS = 4
      IF (VAL.LT.0.)  NS=5
      IF (VAL.EQ.0.)  NS=2
      ND = 0
      V	= VAL*ASH
      LOG = 100
      GO TO 102
  104 iout(1:3) = '0.0'
      nc = 3
c 104 NS = 3
c     ND = 1
c     LOG = -100
c     V	= 0.
c     GO TO 102
C
C1001 FORMAT('(F',I2,'.',I1,',1H',A1,')')
C
      END
