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
C +NOAO - This routine is a no-op in IRAF.
C ---------------------------------------------------------------------
C
      SUBROUTINE AGSAVE (IFNO)
C
C This subroutine is called to save the current state of AUTOGRAPH by
C writing all of its important variables as a record on the file which
C is associated with the unit number IFNO.
C
C The following common block contains the AUTOGRAPH control parameters,
C all of which are real.  If it is changed, all of AUTOGRAPH (especially
C the routine AGSCAN) must be examined for possible side effects.
C
      COMMON /AGCONP/ QFRA,QSET,QROW,QIXY,QWND,QBAC , SVAL(2) ,
     +                XLGF,XRGF,YBGF,YTGF , XLGD,XRGD,YBGD,YTGD , SOGD ,
     +                XMIN,XMAX,QLUX,QOVX,QCEX,XLOW,XHGH ,
     +                YMIN,YMAX,QLUY,QOVY,QCEY,YLOW,YHGH ,
     +                QDAX(4),QSPA(4),PING(4),PINU(4),FUNS(4),QBTD(4),
     +                BASD(4),QMJD(4),QJDP(4),WMJL(4),WMJR(4),QMND(4),
     +                QNDP(4),WMNL(4),WMNR(4),QLTD(4),QLED(4),QLFD(4),
     +                QLOF(4),QLOS(4),DNLA(4),WCLM(4),WCLE(4) ,
     +                QODP,QCDP,WOCD,WODQ,QDSH(26) ,
     +                     QDLB,QBIM,FLLB(10,8),QBAN ,
     +                QLLN,TCLN,QNIM,FLLN(6,16),QNAN ,
     +                XLGW,XRGW,YBGW,YTGW , XLUW,XRUW,YBUW,YTUW ,
     +                XLCW,XRCW,YBCW,YTCW , WCWP,HCWP,SCWP ,
     +                XBGA(4),YBGA(4),UBGA(4),XNDA(4),YNDA(4),UNDA(4),
     +                QBTP(4),BASE(4),QMNT(4),QLTP(4),QLEX(4),QLFL(4),
     +                QCIM(4),QCIE(4),RFNL(4),WNLL(4),WNLR(4),WNLB(4),
     +                WNLE(4),QLUA(4) ,
     +                RBOX(6),DBOX(6,4),SBOX(6,4)
C
C The following common block contains other AUTOGRAPH variables, both
C real and integer, which are not control parameters.
C
      COMMON /AGORIP/ SMRL , ISLD , MWCL,MWCM,MWCE,MDLA,MWCD,MWDQ ,
     +                INIF
C
C The following common blocks contain variables which are required for
C the character-storage-and-retrieval scheme of AUTOGRAPH.
C
      COMMON /AGCHR1/ LNIC,INCH(2,50),LNCA,INCA
C
      COMMON /AGCHR2/ CHRA(2000)
C
      CHARACTER*1 CHRA
C
C If initialization has not yet been done, do it.
C
      IF (INIF.EQ.0) THEN
        CALL AGINIT
      END IF
C
C Write the record.  Variables from each COMMON block are together, in
C alphabetical order.
C
C     WRITE (IFNO,ERR=901)
C    1 BASD,BASE,DBOX,DNLA,FLLB,FLLN,FUNS,HCWP,PING,PINU,QBAC,QBAN,QBIM,
C    2 QBTD,QBTP,QCDP,QCEX,QCEY,QCIE,QCIM,QDAX,QDLB,QDSH,QFRA,QIXY,QJDP,
C    3 QLED,QLEX,QLFD,QLFL,QLLN,QLOF,QLOS,QLTD,QLTP,QLUA,QLUX,QLUY,QMJD,
C    4 QMND,QMNT,QNAN,QNDP,QNIM,QODP,QOVX,QOVY,QROW,QSET,QSPA,QWND,RBOX,
C    5 RFNL,SBOX,SCWP,SOGD,SVAL,TCLN,UBGA,UNDA,WCLE,WCLM,WCWP,WMJL,WMJR,
C    6 WMNL,WMNR,WNLB,WNLE,WNLL,WNLR,WOCD,WODQ,XBGA,XHGH,XLCW,XLGD,XLGF,
C    7 XLGW,XLOW,XLUW,XMAX,XMIN,XNDA,XRCW,XRGD,XRGF,XRGW,XRUW,YBCW,YBGA,
C    8 YBGD,YBGF,YBGW,YBUW,YHGH,YLOW,YMAX,YMIN,YNDA,YTCW,YTGD,YTGF,YTGW,
C    9 YTUW,
C    + INIF,ISLD,MDLA,MWCD,MWCE,MWCL,MWCM,MWDQ,SMRL,
C    1 INCA,INCH,LNCA,LNIC,
C    2 CHRA
C
C Done.
C
      RETURN
C
C Error exit.
C
C 901 CALL SETER ('AGSAVE - ERROR ON WRITE',10,2)
C
C -NOAO
      END
