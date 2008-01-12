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
c +noao: here is the changed block data
c     BLOCKDATA SRFABD
      subroutine srfabd
c
      integer first, temp
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL,
     1                LL         ,FACT       ,IROT       ,NDRZ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO,
     4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
      COMMON /SRFIP1/ IFR        ,ISTP       ,IROTS      ,IDRX,
     1                IDRY       ,IDRZ       ,IUPPER     ,ISKIRT,
     2                NCLA       ,THETA      ,HSKIRT     ,CHI,
     3                CLO        ,CINC       ,ISPVAL
      COMMON /SRFINT/ ISRFMJ     ,ISRFMN     ,ISRFTX
c +noao: common block added 4NOV85 to allow user control of viewport.
      common /noaovp/ vpx1, vpx2, vpy1, vpy2
c-noao
C
c +noao: following flag added to prevent initialization more than once
        common /frstfg/ first
	SAVE
        data temp /1/
	first = temp
        if (first .ne. 1) then
            return
        endif
        temp = 0
c
C +noao: by default, the full device viewport is used
      vpx1 = 0.0
      vpx2 = 1.0
      vpy1 = 0.0
      vpy2 = 1.0
c -noao
C  INITIALIZATION OF INTERNAL PARAMETERS
C
c     DATA ISPVAL/-999/
      ISPVAL = -999

c     DATA IFR,ISTP,IROTS,IDRX,IDRY,IDRZ,IUPPER,ISKIRT,NCLA/
c    1       1,   0,    0,       1,       1,   0,         0,     0,       6/
c +noao: initial value of ifr changed to 0 to suppress frame advance.  This
c       function should be performed by the calling procedure.
c -noao
      IFR    = 0
      ISTP   = 0
      IROTS  = 0  
      IDRX   = 1
      IDRY   = 1
      IDRZ   = 0
      IUPPER = 0
      ISKIRT = 0
      NCLA   = 6
     
c     DATA THETA,HSKIRT,CHI,CLO,CINC/
c    1       .02,    0., 0., 0.,  0./
      THETA  =.02
      HSKIRT = 0.
      CHI    = 0.
      CLO    = 0.
      CINC   = 0.
     
c     DATA NRSWT/0/
      NRSWT = 0

c     DATA IOFFP,SPVAL/0,0.0/
      IOFFP = 0
      SPVAL = 0.0

C LINE COLOR INDEX
c     DATA ISRFMJ/1/
      ISRFMJ = 1
C
c -noao
      END
