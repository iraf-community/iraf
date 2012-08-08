      subroutine entrkw
      common /cdefio/ bp, buf (4096)
      integer bp
      integer buf
      common /cfname/ fcname (30)
      integer fcname
      common /cfor/ fordep, forstk (200)
      integer fordep
      integer forstk
      common /cgoto/ xfer
      integer xfer
      common /clabel/ label, retlab, memflg, col, logic0
      integer label
      integer retlab
      integer memflg
      integer col
      integer logic0
      common /cline/ dbgout, dbglev, level, linect (5), infile (5), fnam
     *p, fnames ( 150)
      integer dbgout
      integer dbglev
      integer level
      integer linect
      integer infile
      integer fnamp
      integer fnames
      common /cmacro/ cp, ep, evalst (500), deftbl
      integer cp
      integer ep
      integer evalst
      integer deftbl
      common /coutln/ outp, outbuf (74)
      integer outp
      integer outbuf
      common /csbuf/ sbp, sbuf(2048), smem(240)
      integer sbp
      integer sbuf
      integer smem
      common /cswtch/ swtop, swlast, swstak(1000), swvnum, swvlev, swvst
     *k(10), swinrg
      integer swtop
      integer swlast
      integer swstak
      integer swvnum
      integer swvlev
      integer swvstk
      integer swinrg
      common /ckword/ rkwtbl
      integer rkwtbl
      common /clname/ fkwtbl, namtbl, gentbl, errtbl, xpptbl
      integer fkwtbl
      integer namtbl
      integer gentbl
      integer errtbl
      integer xpptbl
      common /erchek/ ername, body, esp, errstk(30)
      integer ername
      integer body
      integer esp
      integer errstk
      integer mem( 60000)
      common/cdsmem/mem
      integer sif(3)
      integer selse(5)
      integer swhile(6)
      integer sdo(3)
      integer sbreak(6)
      integer snext(5)
      integer sfor(4)
      integer srept(7)
      integer suntil(6)
      integer sret(7)
      integer sstr(7)
      integer sswtch(7)
      integer scase(5)
      integer sdeflt(8)
      integer send(4)
      integer serrc0(7)
      integer siferr(6)
      integer sifno0(8)
      integer sthen(5)
      integer sbegin(6)
      integer spoint(8)
      integer sgoto(5)
      data sif(1)/105/,sif(2)/102/,sif(3)/-2/
      data selse(1)/101/,selse(2)/108/,selse(3)/115/,selse(4)/101/,selse
     *(5)/-2/
      data swhile(1)/119/,swhile(2)/104/,swhile(3)/105/,swhile(4)/108/,s
     *while(5)/101/,swhile(6)/-2/
      data sdo(1)/100/,sdo(2)/111/,sdo(3)/-2/
      data sbreak(1)/98/,sbreak(2)/114/,sbreak(3)/101/,sbreak(4)/97/,sbr
     *eak(5)/107/,sbreak(6)/-2/
      data snext(1)/110/,snext(2)/101/,snext(3)/120/,snext(4)/116/,snext
     *(5)/-2/
      data sfor(1)/102/,sfor(2)/111/,sfor(3)/114/,sfor(4)/-2/
      data srept(1)/114/,srept(2)/101/,srept(3)/112/,srept(4)/101/,srept
     *(5)/97/,srept(6)/116/,srept(7)/-2/
      data suntil(1)/117/,suntil(2)/110/,suntil(3)/116/,suntil(4)/105/,s
     *until(5)/108/,suntil(6)/-2/
      data sret(1)/114/,sret(2)/101/,sret(3)/116/,sret(4)/117/,sret(5)/1
     *14/,sret(6)/110/,sret(7)/-2/
      data sstr(1)/115/,sstr(2)/116/,sstr(3)/114/,sstr(4)/105/,sstr(5)/1
     *10/,sstr(6)/103/,sstr(7)/-2/
      data sswtch(1)/115/,sswtch(2)/119/,sswtch(3)/105/,sswtch(4)/116/,s
     *swtch(5)/99/,sswtch(6)/104/,sswtch(7)/-2/
      data scase(1)/99/,scase(2)/97/,scase(3)/115/,scase(4)/101/,scase(5
     *)/-2/
      data sdeflt(1)/100/,sdeflt(2)/101/,sdeflt(3)/102/,sdeflt(4)/97/,sd
     *eflt(5)/117/,sdeflt(6)/108/,sdeflt(7)/116/,sdeflt(8)/-2/
      data send(1)/101/,send(2)/110/,send(3)/100/,send(4)/-2/
      data serrc0(1)/101/,serrc0(2)/114/,serrc0(3)/114/,serrc0(4)/99/,se
     *rrc0(5)/104/,serrc0(6)/107/,serrc0(7)/-2/
      data siferr(1)/105/,siferr(2)/102/,siferr(3)/101/,siferr(4)/114/,s
     *iferr(5)/114/,siferr(6)/-2/
      data sifno0(1)/105/,sifno0(2)/102/,sifno0(3)/110/,sifno0(4)/111/,s
     *ifno0(5)/101/,sifno0(6)/114/,sifno0(7)/114/,sifno0(8)/-2/
      data sthen(1)/116/,sthen(2)/104/,sthen(3)/101/,sthen(4)/110/,sthen
     *(5)/-2/
      data sbegin(1)/98/,sbegin(2)/101/,sbegin(3)/103/,sbegin(4)/105/,sb
     *egin(5)/110/,sbegin(6)/-2/
      data spoint(1)/112/,spoint(2)/111/,spoint(3)/105/,spoint(4)/110/,s
     *point(5)/116/,spoint(6)/101/,spoint(7)/114/,spoint(8)/-2/
      data sgoto(1)/103/,sgoto(2)/111/,sgoto(3)/116/,sgoto(4)/111/,sgoto
     *(5)/-2/
      call enter (sif, -99, rkwtbl)
      call enter (selse, -87, rkwtbl)
      call enter (swhile, -95, rkwtbl)
      call enter (sdo, -96, rkwtbl)
      call enter (sbreak, -79, rkwtbl)
      call enter (snext, -78, rkwtbl)
      call enter (sfor, -94, rkwtbl)
      call enter (srept, -93, rkwtbl)
      call enter (suntil, -70, rkwtbl)
      call enter (sret, -77, rkwtbl)
      call enter (sstr, -75, rkwtbl)
      call enter (sswtch, -92, rkwtbl)
      call enter (scase, -91, rkwtbl)
      call enter (sdeflt, -90, rkwtbl)
      call enter (send, -82, rkwtbl)
      call enter (serrc0, -84, rkwtbl)
      call enter (siferr, -98, rkwtbl)
      call enter (sifno0, -97, rkwtbl)
      call enter (sthen, -86, rkwtbl)
      call enter (sbegin, -83, rkwtbl)
      call enter (spoint, -88, rkwtbl)
      call enter (sgoto, -76, rkwtbl)
      return
      end
c     sifno0  sifnoerr
c     logic0  logical_column
c     serrc0  serrchk
