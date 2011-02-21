      subroutine entxkw
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
      integer sbool(7)
      integer schar(7)
      integer sshort(8)
      integer sint(6)
      integer slong(7)
      integer sreal(7)
      integer sdble(7)
      integer scplx(7)
      integer spntr(7)
      integer sfchr(7)
      integer sfunc(7)
      integer ssubr(7)
      integer sextn(7)
      integer dbool(8)
      integer dchar(10)
      integer dshort(10)
C     integer dint(10)
C     integer dlong(10)
C     integer dpntr(10)
      integer dint(8)
      integer dlong(8)
      integer dpntr(8)
      integer dreal(5)
      integer ddble(17)
      integer dcplx(8)
      integer dfchr(10)
      integer dfunc(9)
      integer dsubr(11)
      integer dextn(9)
      data sbool(1)/120/,sbool(2)/36/,sbool(3)/98/,sbool(4)/111/,sbool(5
     *)/111/,sbool(6)/108/,sbool(7)/-2/
      data schar(1)/120/,schar(2)/36/,schar(3)/99/,schar(4)/104/,schar(5
     *)/97/,schar(6)/114/,schar(7)/-2/
      data sshort(1)/120/,sshort(2)/36/,sshort(3)/115/,sshort(4)/104/,ss
     *hort(5)/111/,sshort(6)/114/,sshort(7)/116/,sshort(8)/-2/
      data sint(1)/120/,sint(2)/36/,sint(3)/105/,sint(4)/110/,sint(5)/11
     *6/,sint(6)/-2/
      data slong(1)/120/,slong(2)/36/,slong(3)/108/,slong(4)/111/,slong(
     *5)/110/,slong(6)/103/,slong(7)/-2/
      data sreal(1)/120/,sreal(2)/36/,sreal(3)/114/,sreal(4)/101/,sreal(
     *5)/97/,sreal(6)/108/,sreal(7)/-2/
      data sdble(1)/120/,sdble(2)/36/,sdble(3)/100/,sdble(4)/98/,sdble(5
     *)/108/,sdble(6)/101/,sdble(7)/-2/
      data scplx(1)/120/,scplx(2)/36/,scplx(3)/99/,scplx(4)/112/,scplx(5
     *)/108/,scplx(6)/120/,scplx(7)/-2/
      data spntr(1)/120/,spntr(2)/36/,spntr(3)/112/,spntr(4)/110/,spntr(
     *5)/116/,spntr(6)/114/,spntr(7)/-2/
      data sfchr(1)/120/,sfchr(2)/36/,sfchr(3)/102/,sfchr(4)/99/,sfchr(5
     *)/104/,sfchr(6)/114/,sfchr(7)/-2/
      data sfunc(1)/120/,sfunc(2)/36/,sfunc(3)/102/,sfunc(4)/117/,sfunc(
     *5)/110/,sfunc(6)/99/,sfunc(7)/-2/
      data ssubr(1)/120/,ssubr(2)/36/,ssubr(3)/115/,ssubr(4)/117/,ssubr(
     *5)/98/,ssubr(6)/114/,ssubr(7)/-2/
      data sextn(1)/120/,sextn(2)/36/,sextn(3)/101/,sextn(4)/120/,sextn(
     *5)/116/,sextn(6)/110/,sextn(7)/-2/
      data dbool(1)/108/,dbool(2)/111/,dbool(3)/103/,dbool(4)/105/,dbool
     *(5)/99/,dbool(6)/97/,dbool(7)/108/,dbool(8)/-2/
      data dchar(1)/105/,dchar(2)/110/,dchar(3)/116/,dchar(4)/101/,dchar
     *(5)/103/,dchar(6)/101/,dchar(7)/114/,dchar(8)/42/,dchar(9)/50/,dch
     *ar(10)/-2/
      data dshort(1)/105/,dshort(2)/110/,dshort(3)/116/,dshort(4)/101/,d
     *short(5)/103/,dshort(6)/101/,dshort(7)/114/,dshort(8)/42/,dshort(9
     *)/50/,dshort(10)/-2/
C     data dint(1)/105/,dint(2)/110/,dint(3)/116/,dint(4)/101/,dint(5)/1
C    *03/,dint(6)/101/,dint(7)/114/,dint(8)/42/,dint(9)/56/,dint(10)/-2/
      data dint(1)/105/,dint(2)/110/,dint(3)/116/,dint(4)/101/,dint(5)/1
     *03/,dint(6)/101/,dint(7)/114/,dint(8)/-2/
C     data dlong(1)/105/,dlong(2)/110/,dlong(3)/116/,dlong(4)/101/,dlong
C    *(5)/103/,dlong(6)/101/,dlong(7)/114/,dlong(8)/42/,dlong(9)/52/,dlo
C    *ng(10)/-2/
      data dlong(1)/105/,dlong(2)/110/,dlong(3)/116/,dlong(4)/101/,dlong
     *(5)/103/,dlong(6)/101/,dlong(7)/114/,dlong(8)/-2/
C     data dpntr(1)/105/,dpntr(2)/110/,dpntr(3)/116/,dpntr(4)/101/,dpntr
C    *(5)/103/,dpntr(6)/101/,dpntr(7)/114/,dpntr(8)/42/,dpntr(9)/56/,dpn
C    *tr(10)/-2/
      data dpntr(1)/105/,dpntr(2)/110/,dpntr(3)/116/,dpntr(4)/101/,dpntr
     *(5)/103/,dpntr(6)/101/,dpntr(7)/114/,dpntr(8)/-2/
      data dreal(1)/114/,dreal(2)/101/,dreal(3)/97/,dreal(4)/108/,dreal(
     *5)/-2/
      data ddble(1)/100/,ddble(2)/111/,ddble(3)/117/,ddble(4)/98/,ddble(
     *5)/108/,ddble(6)/101/,ddble(7)/32/,ddble(8)/112/,ddble(9)/114/,ddb
     *le(10)/101/,ddble(11)/99/,ddble(12)/105/,ddble(13)/115/,ddble(14)/
     *105/,ddble(15)/111/,ddble(16)/110/,ddble(17)/-2/
      data dcplx(1)/99/,dcplx(2)/111/,dcplx(3)/109/,dcplx(4)/112/,dcplx(
     *5)/108/,dcplx(6)/101/,dcplx(7)/120/,dcplx(8)/-2/
      data dfchr(1)/99/,dfchr(2)/104/,dfchr(3)/97/,dfchr(4)/114/,dfchr(5
     *)/97/,dfchr(6)/99/,dfchr(7)/116/,dfchr(8)/101/,dfchr(9)/114/,dfchr
     *(10)/-2/
      data dfunc(1)/102/,dfunc(2)/117/,dfunc(3)/110/,dfunc(4)/99/,dfunc(
     *5)/116/,dfunc(6)/105/,dfunc(7)/111/,dfunc(8)/110/,dfunc(9)/-2/
      data dsubr(1)/115/,dsubr(2)/117/,dsubr(3)/98/,dsubr(4)/114/,dsubr(
     *5)/111/,dsubr(6)/117/,dsubr(7)/116/,dsubr(8)/105/,dsubr(9)/110/,ds
     *ubr(10)/101/,dsubr(11)/-2/
      data dextn(1)/101/,dextn(2)/120/,dextn(3)/116/,dextn(4)/101/,dextn
     *(5)/114/,dextn(6)/110/,dextn(7)/97/,dextn(8)/108/,dextn(9)/-2/
      call entdef (sbool, dbool, xpptbl)
      call entdef (schar, dchar, xpptbl)
      call entdef (sshort, dshort, xpptbl)
      call entdef (sint, dint, xpptbl)
      call entdef (slong, dlong, xpptbl)
      call entdef (spntr, dpntr, xpptbl)
      call entdef (sreal, dreal, xpptbl)
      call entdef (sdble, ddble, xpptbl)
      call entdef (scplx, dcplx, xpptbl)
      call entdef (sfchr, dfchr, xpptbl)
      call entdef (sfunc, dfunc, xpptbl)
      call entdef (ssubr, dsubr, xpptbl)
      call entdef (sextn, dextn, xpptbl)
      end
c     logic0  logical_column
