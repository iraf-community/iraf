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
c + noao: block data gridt changed to run time initialization
c       BLOCK DATA GRIDT
        subroutine gridt
C
C
        COMMON /LAB/ SIZX,SIZY,XDEC,YDEC,IXORI
        COMMON /CLAB/ XFMT, YFMT
        COMMON /TICK/ MAJX, MINX, MAJY, MINY
        COMMON /GRIINT/ IGRIMJ, IGRIMN, IGRITX
        CHARACTER*8 XFMT,YFMT
        REAL MAJX,MINX,MAJY,MINY
C
c +noao: following flag added to prevent initializing more than once
        logical first
        SAVE
        data first /.true./
        if (.not. first) then
            return
        endif
        first = .false.
C
c       DATA XFMT,YFMT /'(E10.3) ','(E10.3) '/
        XFMT = '(E10.3) '
        YFMT = '(E10.3) '
c
c       DATA SIZX,SIZY / 0.01, 0.01 /
        SIZX = 0.01
        SIZY = 0.01
c
c       DATA XDEC,YDEC / 0., 0. /
        XDEC = 0.
        YDEC = 0.
c
c       DATA IXORI / 0 /
        IXORI = 0 
c
c       DATA MAJX,MINX,MAJY,MINY / 0., 0., 0., 0./
        MAJX = 0.
        MINX = 0.
        MAJY = 0.
        MINY = 0.
c
c       DATA IGRIMJ,IGRIMN,IGRITX / 1, 1, 1/
c+noao: These values changed so major axes and labels are bold
        IGRIMJ = 2
        IGRIMN = 1
        IGRITX = 2
C - noao
        END
C REVISION HISTORY---------------
C----------------------------------------------------------

