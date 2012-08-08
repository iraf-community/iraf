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
c +noao: blockdata isosrb changed to run time initialization subroutine
      subroutine isosrb
c     BLOCKDATA ISOSRB
C
C     BLOCK DATA
C
      COMMON /ISOSR2/ LX         ,NX               ,NY   ,ISCR(8,128),
     1                ISCA(8,128)
      COMMON /ISOSR4/ RX         ,RY
      COMMON /ISOSR5/ NBPW       ,MASK(16)      ,GENDON
      LOGICAL               GENDON
      COMMON /ISOSR6/ IX         ,IY               ,IDX  ,IDY,
     1                IS         ,ISS        ,NP         ,CV,
     2                INX(8)     ,INY(8)     ,IR(500)    ,NR
      COMMON /ISOSR7/ IENTRY     ,IONES
      COMMON /ISOSR8/ NMASK(16)  ,IXOLD      ,IYOLD      ,IBTOLD,
     1                HBFLAG     ,IOSLSN     ,LRLX       ,IFSX,
     2                IFSY       ,FIRST      ,IYDIR      ,IHX,
     3                IHB        ,IHS        ,IHV        ,IVOLD,
     4                IVAL       ,IHRX       ,YCHANG     ,ITPD,
     5                IHF
      COMMON /ISOSR9/ BIG        ,IXBIT
      COMMON /TEMPR/  RZERO
      LOGICAL               YCHANG       ,HBFLAG        ,FIRST   ,IHF
C
      logical first1
      SAVE
      data first1 /.true./
      if (.not. first1) then
        return
      endif
      first1 = .false.
c
c     DATA LX,NX,NY/8,128,128/
      LX = 8
      NX = 128
      NY = 128   
c
c     DATA INX(1),INX(2),INX(3),INX(4),INX(5),INX(6),INX(7),INX(8)/
c    1        -1 ,   -1 ,    0 ,    1 ,    1 ,          1 ,      0 ,   -1 /
      INX(1) = -1
      INX(2) = -1
      INX(3) =  0
      INX(4) =  1
      INX(5) =  1
      INX(6) =  1
      INX(7) =  0
      INX(8) = -1
c
c     DATA INY(1),INY(2),INY(3),INY(4),INY(5),INY(6),INY(7),INY(8)/
c    1         0 ,    1 ,    1 ,    1 ,    0 ,   -1 ,   -1 ,   -1 /
      INY(1) =  0
      INY(2) =  1
      INY(3) =  1
      INY(4) =  1
      INY(5) =  0
      INY(6) = -1
      INY(7) = -1
      INY(8) = -1
c
c     DATA NR/500/
      NR = 500
c
c     DATA NBPW/16/
      NBPW = 16
c
c     DATA IHF/.FALSE./
      IHF = .FALSE.
C
c     DATA GENDON /.FALSE./
      GENDON = .FALSE.
c
c     DATA RZERO/0./
      RZERO = 0.
C
C
C RX = (NX-1)/SCREEN WIDTH FROM TRN32I
C RY = (NY-1)/SCREEN HEIGHT FROM TRN32I
C
c     DATA RX,RY/.00389,.00389/
      RX = .00389
      RY = .00389  
C
c -noao
      END
