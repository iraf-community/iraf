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
c +noao: block data veldat changed to run time initialization
c     BLOCK DATA VELDAT
        subroutine veldat
C
C THIS 'ROUTINE' DEFINES THE DEFAULT VALUES OF THE VELVCT PARAMETERS.
C
      COMMON /VEC1/   ASH        ,EXT        ,ICTRFG     ,ILAB,
     +                IOFFD      ,IOFFM      ,ISX        ,ISY,
     +                RMN        ,RMX        ,SIDE       ,SIZE,
     +                XLT        ,YBT        ,ZMN        ,ZMX
C
      COMMON /VEC2/   BIG        ,INCX       ,INCY
C
c     DATA     EXT /    0.25 /
c     DATA  ICTRFG /    1         /
c     DATA    ILAB /    0         /
c     DATA   IOFFD /    0         /
c     DATA   IOFFM /    0         /
c     DATA     RMN /  160.00 /
c     DATA     RMX / 6400.00 /
c     DATA    SIDE /    0.90 /
c     DATA    SIZE /  256.00 /
c     DATA     XLT /    0.05 /
c     DATA     YBT /    0.05 /
c     DATA     ZMX /    0.00 /
c     DATA    INCX /    1          /
c     DATA    INCY /    1          /
c
c +noao: following flag added to prevent over-initialization
      logical first
        SAVE
      data first /.true./
      if (.not. first) then
          return
      endif
      first = .false.

      EXT =     0.25 
      ICTRFG =  1    
      ILAB =    0         
      IOFFD =   0    
      IOFFM =   0    
      RMN =  160.00 
      RMX = 6400.00 
      SIDE =    0.90 
      SIZE =  256.00 
      XLT =     0.05 
      YBT =     0.05 
      ZMX =     0.00 
      INCX =    1          
      INCY =    1          
C
c - noao
      END
