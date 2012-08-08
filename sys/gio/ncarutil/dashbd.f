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
c +noao: block data changed to run time initialization. Logical param
c "first" added, so initialization doesn't occur more than once.
c     BLOCKDATA DASHBD
        subroutine dashbd
C
C DASHBD IS USED TO INITIALIZE VARIABLES IN NAMED COMMON.
C
        logical first
c
      COMMON /DASHD1/  ISL,  L,  ISIZE,  IP(100),  NWDSM1,  IPFLAG(100)
     1                 ,MNCSTR, IGP
C
      COMMON /FDFLAG/ IFLAG
C
      COMMON /DDFLAG/ IFCFLG
C
      COMMON /DCFLAG/ IFSTFL
C
      COMMON /DFFLAG/ IFSTF2
C
      COMMON /CFFLAG/ IVCTFG
C
      COMMON /DSAVE3/ IXSTOR,IYSTOR
C
      COMMON /DSAVE5/ XSAVE(70), YSAVE(70), XSVN, YSVN, XSV1, YSV1,
     1                SLP1, SLPN, SSLP1, SSLPN, N, NSEG
C
      COMMON /SMFLAG/ IOFFS
C
      COMMON/INTPR/IPAU,FPART,TENSN,NP,SMALL,L1,ADDLR,ADDTB,MLLINE,
     1    ICLOSE
C
        SAVE
        data first /.true./
        if (.not. first) return
        first = .false.
            
C IFSTFL CONTROLS THAT FRSTD IS CALLED BEFORE VECTD IS CALLED (IN CFVLD)
C WHENEVER DASHDB OR DASHDC HAS BEEN CALLED.
C
c     DATA IFSTFL /1/
      IFSTFL  = 1
C
C IVCTFG INDICATES IF VECTD IS BEING CALLED OR LASTD (IN CFVLD)
C
c     DATA IVCTFG /1/
      IVCTFG = 1
C
C ISL IS A FLAG FOR AN ALL SOLID PATTERN (+1) OR AN ALL GAP PATTERN (-1)
C
c     DATA ISL /1/
      ISL = 1
C
C IGP IS AN INTERNAL PARAMETER. IT IS DESCRIBED IN THE DOCUMENTATION
C TO THE DASHED LINE PACKAGE.
C
c     DATA IGP /9/
      IGP = 9
C
C MNCSTR IS THE MAXIMUM NUMBER OF CHARACTERS ALLOWED IN A HOLLERITH
C STRING PASSED TO DASHDC.
C
c     DATA MNCSTR /15/
      MNCSTR = 15
C
C IOFFS IS AN INTERNAL PARAMETER.
C IOFFS IS USED IN FDVDLD AND DRAWPV.
C
c     DATA IOFFS /0/
      IOFFS = 0
C
C  INTERNAL PARAMETERS
C
c     DATA IPAU/3/
      IPAU = 3
c     DATA FPART/1./
      FPART = 1.
c     DATA TENSN/2.5/
      TENSN = 2.5
c     DATA NP/150/
      NP = 150
c     DATA SMALL/128./
      SMALL = 128.
c     DATA L1/70/
      L1 = 70
c     DATA ADDLR/2./
      ADDLR = 2.
c     DATA ADDTB/2./
      ADDTB = 2.
c     DATA MLLINE/384/
      MLLINE = 384
c     DATA ICLOSE/6/
      ICLOSE = 6
C
C IFSTF2 IS A FLAG TO CONTROL THAT FRSTD IS CALLED BEFORE VECTD IS
C CALLED (IN SUBROUTINE FDVDLD), WHENEVER DASHDB OR DASHDC
C HAS BEEN CALLED.
C
c     DATA IFSTF2 /1/
      IFSTF2  = 1
C
C IFLAG CONTROLS IF LASTD CAN BE CALLED DIRECTLY OR IF IT WAS JUST
C CALLED FROM BY VECTD SO THAT THIS CALL CAN BE IGNORED.
C
c     DATA IFLAG /1/
      IFLAG = 1
C
C IFCFLG IS THE FIRST CALL FLAG FOR SUBROUTINES DASHDB AND DASHDC.
C  1 = FIRST CALL TO DASHDB OR DASHDC.
C  2 = DASHDB OR DASHDC HAS BEEN CALLED BEFORE.
C
c     DATA IFCFLG /1/
      IFCFLG = 1
C
C IXSTOR AND IYSTOR CONTAIN THE CURRENT PEN POSITION. THEY ARE
C INITIALIZED TO AN IMPOSSIBLE VALUE.
C
c     DATA IXSTOR,IYSTOR /-9999,-9999/
      IXSTOR = -9999 
      IYSTOR = -9999
C
C SLP1 AND SLPN ARE INITIALIZED TO AVOID THAT THEY ARE PASSED AS ACTUAL
C PARAMETERS FROM FDVDLD TO KURV1S WITHOUT BEING DEFINED.
C
c     DATA SLP1,SLPN /-9999.,-9999./
      SLP1 = -9999. 
      SLPN = -9999.
c -noao
C
      END
