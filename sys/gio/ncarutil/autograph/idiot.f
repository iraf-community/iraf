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
      SUBROUTINE IDIOT (XDRA,YDRA,NPTS,LTYP,LDSH,LABX,LABY,LABG,LFRA)
C
      REAL XDRA(*),YDRA(*)
C
      INTEGER LDSH(*)
C
      CHARACTER*(*) LABX,LABY,LABG
C
      CHARACTER*16 AGBNCH
C
C This is an implementation of the routine from which AUTOGRAPH grew.
C It should work pretty much as the original did (if you can figure out
C what that was).
C
C Do statistics-gathering call.
C
      LOGICAL Q8Q4
      SAVE Q8Q4
      DATA Q8Q4 /.TRUE./
      IF (Q8Q4) THEN
        CALL Q8QST4('GRAPHX','AUTOGRAPH','IDIOT','VERSION 07')
        Q8Q4 = .FALSE.
      ENDIF
C
C +NOAO
C
      call agdflt
C 
C -NOAO
      CALL ANOTAT (LABX,LABY,1,2-ISIGN(1,NPTS),1,AGBNCH(LDSH))
C
      CALL DISPLA (2-MAX0(-1,MIN0(1,LFRA)),1,LTYP)
C
      CALL AGEZSU (5,XDRA,YDRA,IABS(NPTS),1,IABS(NPTS),LABG,IIVX,IIEX,
     +                                                        IIVY,IIEY)
      CALL AGBACK
C
      CALL AGCURV (XDRA,1,YDRA,1,IABS(NPTS),1)
C
      IF (LFRA.GT.0) CALL FRAME
C
C +NOAO
C
      call plotit (0, 0, 2)
      call initut
C
C -NOAO
      RETURN
      END
