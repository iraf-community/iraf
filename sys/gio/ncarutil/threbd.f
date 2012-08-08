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
c +noao: block data threbd changed to run time initialization
        subroutine threbd
c     BLOCKDATA THREBD
      COMMON /TEMPR/  RZERO
      COMMON /SET31/  ISCALE     ,XMIN       ,XMAX   ,YMIN,
     1                YMAX       ,BIGD       ,R0     ,NLX,
     2                NBY        ,NRX        ,NTY
      COMMON /TCK31/  TMAGU      ,TMINU      ,TMAGV  ,TMINV,
     1                TMAGW      ,TMINW
      COMMON /THRINT/ ITHRMJ     ,ITHRMN     ,ITHRTX
c +noao: following flag added to prevent over-initialization
      logical first
      SAVE
      data first /.true./
      if (.not. first) then
        return
      endif
      first = .false.

c     DATA RZERO/0./
      RZERO = 0.
c
c     DATA NLX,NBY,NRX,NTY/10,10,1010,1010/
      NLX = 10
      NBY = 10
      NRX = 1010
      NTY = 1010   
c
c     DATA TMAGU,TMINU,TMAGV,TMINV,TMAGW,TMINW/12.,8.,12.,8.,12.,8./
      TMAGU = 12.
      TMINU =  8.
      TMAGV = 12.
      TMINV =  8.
      TMAGW = 12.
      TMINW =  8.
c
c     DATA ITHRMJ,ITHRMN,ITHRTX/ 1,1,1/
      ITHRMJ = 2
      ITHRMN = 1
      ITHRTX = 1
c
c -noao
      END
