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
C     BLOCKDATA CONBD
      subroutine conbd
      integer first, temp
      common /conflg/ first
      COMMON /CONRE1/ IOFFP      ,SPVAL
      COMMON /CONRE2/ IX         ,IY         ,IDX  ,IDY ,
     1                IS         ,ISS        ,NP         ,CV ,
     2                INX(8)     ,INY(8)     ,IR(80000)    ,NR
c +noao: dimension of stline ir array increased from 20000 to 80000 6-93
      COMMON /CONRE4/ ISIZEL     ,ISIZEM     ,ISIZEP     ,NREP,
     1                NCRT       ,ILAB       ,NULBLL     ,IOFFD,
     2                EXT        ,IOFFM      ,ISOLID     ,NLA,
     3                NLM        ,XLT        ,YBT        ,SIDE
      COMMON /RECINT/ IRECMJ     ,IRECMN          ,IRECTX
C
      SAVE
C
C     DATA IOFFP,SPVAL/0,0.0/
      data temp /1/
      first = temp
      IOFFP  = 0
      SPVAL  = 0.0
C     DATA ISIZEL,ISIZEM,ISIZEP,NLA,NLM,XLT,YBT,SIDE,ISOLID,NREP,NCRT/
C    1       1,  2,   0, 16, 40,.05,.05,  .9,  1023,   6,   4 /
      if (first .ne. 1) then
        return
      endif

      temp = 0

c     ISIZEL = 1
c noao: size of contour labels seemed too large.  Changed from 1 to 0
      isizel = 0
      ISIZEM = 2
      ISIZEP = 0
      NLA    = 16
      NLM    = 40
      XLT    = .05
      YBT    = .05
      SIDE   = .9
      ISOLID = 1023
      NREP   = 4
      NCRT   = 2 
C     DATA EXT,IOFFD,NULBLL,IOFFM,ILAB/.25,0,3,0,1/
C +noao value of "extreme" axes ratios changed from 1/4 to 1/16 (ShJ 6-10-88)
C     EXT    = .25
      EXT    = .0625
C -noao
      IOFFD  = 0
      NULBLL = 3
      IOFFM  = 0
      ILAB   = 1
C     DATA INX(1),INX(2),INX(3),INX(4),INX(5),INX(6),INX(7),INX(8)/
C    1        -1 ,   -1 ,    0 ,    1 ,    1 ,          1 ,      0 ,   -1 /
      INX(1) = -1 
      INX(2) = -1
      INX(3) = 0 
      INX(4) = 1 
      INX(5) = 1 
      INX(6) = 1
      INX(7) = 0
      INX(8) = -1
C     DATA INY(1),INY(2),INY(3),INY(4),INY(5),INY(6),INY(7),INY(8)/
C    1         0 ,    1 ,    1 ,    1 ,    0 ,   -1 ,   -1 ,   -1 /
      INY(1) = 0 
      INY(2) = 1
      INY(3) = 1
      INY(4) = 1 
      INY(5) = 0
      INY(6) = -1
      INY(7) = -1
      INY(8) = -1
C     DATA NR/500/
c +noao: dimension of stline array increased from 500 to 5000 6March87
c +noao: dimension of stline array increased from 5000 to 20000 Jan90
c +noao: dimension of stline array increased from 20000 to 80000 6-93
      NR     = 80000
C     DATA IRECMJ,IRECMN,IRECTX/ 1 ,   1 ,   1/
c +noao: value of irecmj changed so major divisions are high intensity
      IRECMJ = 2 
      IRECMN = 1
      IRECTX = 1
C
C - noao
C
C REVISION HISTORY---
C
C JANUARY 1980     ADDED REVISION HISTORY AND CHANGED LIBRARY NAME
C                  FROM CRAYLIB TO PORTLIB FOR MOVE TO PORTLIB
C
C MAY 1980         ARRAYS IWORK AND ENCSCR, PREVIOUSLY TOO SHORT FOR
C                  SHORT-WORD-LENGTH MACHINES, LENGTHENED.  SOME
C                  DOCUMENTATION CLARIFIED AND CORRECTED.
C
C JUNE 1984        CONVERTED TO FORTRAN 77 AND TO GKS
C-------------------------------------------------------------------
C
      END
