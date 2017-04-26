      SUBROUTINE NLCFIT(IM,INN,IN,INTA,XEPSI,XV,XYD)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     NONLINEAR LEAST SQUARES FITTING USING SIMPLEX
C     METHOD AND QUADRATIC APPROXIMATION.
C     WITH LINEAR PARAMETER ELIMINATION.
C-------------------------------------------------------------
      INTEGER IM,INN,IN,INTA
      REAL XEPSI,XV(IM),XYD(IM)
      COMMON /NLC/ EPSI,IFLAG,IL,IQ,INDEX,F(15,120),M,N
      COMMON /NLC/ SOLD,Y(20),YVAL,XF(11),X(11,11),V(120),YD(120,10)
      COMMON /NLC/ GG(11,11),GINV(11,11),EM(15,120),BB(5),NT
      COMMON /NLC/ GA(120,5),NN
	COMMON /NLCOUT/ FF(120),PARS(10),BPARS(10)
      DIMENSION SUMC(11),XC(11),XE(11),XCO(11),XR(11)
      REAL LERROR
C-----
C RESET ERROR HANDLER
c...UNIX has general handler only!
c	call trpfpe (0, 0d0)
C-----
C FLOAT OVERFLOW
c	CALL ERRSET(72,.TRUE.,.FALSE.,.FALSE.,.FALSE.,15)
C FLOAT UNDERFLOW
c	CALL ERRSET(74,.TRUE.,.FALSE.,.FALSE.,.FALSE.,15)
C EXP TOO SMALL
c	CALL ERRSET(89,.TRUE.,.FALSE.,.FALSE.,.FALSE.,15)
C EXP TOO LRGE
c	CALL ERRSET(88,.TRUE.,.FALSE.,.FALSE.,.FALSE.,15)
C-----
      LERROR=1.E30
	IFLAG=0
C     COEFFICIENTS
C-----
C ASSIGN EXTERNAL PARAMETERS
	M=IM
	NN=INN
	N=IN
	NT=INTA
	EPSI=XEPSI
	DO 8100 I=1,M
		V(I)=XV(I)
		YD(I,1)=XYD(I)
8100	CONTINUE
C-----
      T=1.0
      A=1.0
      B=0.5
      G=2.0
      ICOUNT=0
      INDEX=1
      IQ=3*N
      DO 140 J=1,N
140   X(1,J)=1.0
160   DO 172 J=1,N
172   XF(J)=X(1,J)
      CALL FVAL
      Y(1)=YVAL
      SOLD=YVAL
C---- CONSTRUCT SIMPLEX
      EN=N
      PN=(SQRT(EN+1.0)-1.0+EN)/(EN*SQRT(2.0))*T
      QN=(SQRT(EN+1.0)-1.0)/(EN*SQRT(2.0))*T
      NP1=N+1
      DO 305 I=2,NP1
      INDEX=I
      DO 300 J=1,N
      EJ=0.0
      EI=0.0
      IF(I-1.NE.J) EJ=1.0
      IF(I-1.EQ.J) EI=1.0
      X(I,J)=X(1,J)+EI*PN+EJ*QN
300   XF(J)=X(I,J)
      CALL FVAL
305   Y(I)=YVAL
C---- DETERMINE MAX XH
310   IH=1
      DO 350 J=1,NP1
      IF(Y(IH).GE.Y(J)) GOTO 350
      IH=J
350   CONTINUE
C---- DETERMINE SECOND MAX XS
      IS=1
      IF(IH.NE.1) GOTO 470
      IS=2
470   CONTINUE
      DO 420 J=1,NP1
      IF(J.EQ.IH) GOTO 420
      IF(Y(IS).GE.Y(J)) GOTO 420
      IS=J
420   CONTINUE
C---- DETERMINE MIN XL
      IL=1
      DO 480 J=1,NP1
      IF(Y(IL).LE.Y(J)) GOTO 480
      IL=J
480   CONTINUE
C---- COMPUTE CENTROID
      DO 510 J=1,N
510   SUMC(J)=0.0
      EN=N
      DO 570 J=1,N
      DO 560 I=1,NP1
      IF(I.EQ.IH) GOTO 560
      SUMC(J)=SUMC(J)+X(I,J)
560   CONTINUE
570   XC(J)=1.0/EN*SUMC(J)
      DO 573 J=1,N
573   XF(J)=XC(J)
      CALL FVAL
      YBAR=YVAL
      SUM=0.0
      DO 577 I=1,NP1
  577 SUM = SUM + ((Y(I)-YBAR)/YBAR)**2
      ICOUNT=ICOUNT+1
      ERROR=SQRT(SUM/EN)
      IQ=IQ-1
      IF(IQ.EQ.-1) CALL QADFIT
      IF(IFLAG.EQ.1) GOTO 1990
      IF(ERROR.LE.EPSI) GOTO 1990
      IF(ABS(LERROR-ERROR).LT.EPSI) GO TO 1990
      LERROR=ERROR
C---- DO A REFLECTION
      DO 600 J=1,N
600   XR(J)=(1.0+A)*XC(J)-A*X(IH,J)
      DO 610 J=1,N
610   XF(J)=XR(J)
      INDEX=N+2
      CALL FVAL
      YXR=YVAL
      IF(YXR.GE.Y(IL)) GOTO 750
C---- DO A EXPANSION
      DO 660 J=1,N
660   XE(J)=G*XR(J)+(1.0-G)*XC(J)
      DO 680 J=1,N
680   XF(J)=XE(J)
      INDEX=N+3
      CALL FVAL
      YXE=YVAL
      IF(YXE.GT.Y(IL)) GOTO 760
      DO 730 J=1,N
730   X(IH,J)=XE(J)
      Y(IH)=YXE
      NP3=N+3
      DO 735 K=1,M
735   F(IH,K)=F(NP3,K)
      GOTO 310
750   IF(YXR.GT.Y(IS)) GOTO 800
760   DO 780 J=1,N
780   X(IH,J)=XR(J)
      Y(IH)=YXR
      NP2=N+2
      DO 785 K=1,M
785   F(IH,K)=F(NP2,K)
      GOTO 310
800   IF(YXR.GT.Y(IH)) GOTO 830
      DO 820 J=1,N
820   X(IH,J)=XR(J)
C---- DO A CONTRACTION
830   DO 840 J=1,N
840   XCO(J)=B*X(IH,J)+(1.0-B)*XC(J)
      DO 860 J=1,N
860   XF(J)=XCO(J)
      INDEX=N+2
      CALL FVAL
      YXCO=YVAL
      IF(YXCO.GT.Y(IH)) GOTO 930
      DO 910 J=1,N
910   X(IH,J)=XCO(J)
      Y(IH)=YXCO
      NP2=N+2
      DO 915 K=1,M
915   F(IH,K)=F(NP2,K)
      GOTO 310
930   DO 960 I=1,NP1
      INDEX=I
      DO 955 J=1,N
950   X(I,J)=0.5*(X(I,J)+X(IL,J))
955   XF(J)=X(I,J)
      CALL FVAL
960   Y(I)=YVAL
C---- HAS A MIN BEEN REACHED?
      GOTO 310
1990  DO 1594 J=1,N
	PARS(J)=X(IL,J)
1594  XF(J)=X(IL,J)
      CALL FVAL
	DO 1595 I=1,NT
1595	BPARS(I)=BB(I)
      CALL INDEXD
	RETURN
      END
C---------------------------------------------------------------------
      SUBROUTINE MATIN
C---- DETERMINE INVERSE OF MATRIX
      COMMON /NLC/ EPSI,IFLAG,IL,IQ,INDEX,F(15,120),M,N
      COMMON /NLC/ SOLD,Y(20),YVAL,XF(11),X(11,11),V(120),YD(120,10)
      COMMON /NLC/ GG(11,11),GINV(11,11),EM(15,120),BB(5),NT
      COMMON /NLC/ GA(120,5),NN
      DIMENSION E(15,120),EN(20),T(20),Z(11,11),YY(20)
      EQUIVALENCE (EM(1,1),E(1,1))
      DO 20 I=1,N
      DO 20 J=1,N
      IF(I.EQ.J) GOTO 10
      Z(I,J)=0.0
      GOTO 20
10    Z(I,J)=1.0
20    CONTINUE
      DO 120 J0=1,N
      I0=J0
      DO 30 I=1,N
30    YY(I)=GG(I,J0)
      DO 40 I=1,N
      EN(I) = 0.
      T(I)=0.0
      DO 40 J=1,N
40    T(I)=T(I)+Z(I,J)*YY(J)
      IF(T(J0).EQ.0.) GO TO 65
      DO 60 J=1,N
      IF(J.EQ.J0) GOTO 50
      EN(J)=-T(J)/T(J0)
      GOTO 60
50    EN(J)=1./T(J0)
60    CONTINUE
   65 DO 80 I = 1,N
      DO 80 J=1,N
      IF (I.EQ.J) GOTO 70
      E(I,J)=0.0
      GOTO 80
70    E(I,J)=1.0
80    CONTINUE
      DO 90 J=1,N
90    E(J,J0)=EN(J)
      DO 100 K=1,N
      DO 100 I=1,N
      GINV(K,I)=0.0
      DO 100 J=1,N
100   GINV(K,I)=GINV(K,I)+E(K,J)*Z(J,I)
      DO 110 J=1,N
      DO 110 I=1,N
110   Z(I,J)=GINV(I,J)
120   CONTINUE
      RETURN
      END
C-------------------------------------------------------------------------
      SUBROUTINE QADFIT
      COMMON /NLC/ EPSI,IFLAG,IL,IQ,INDEX,F(15,120),M,N
      COMMON /NLC/ SOLD,Y(20),YVAL,XF(11),X(11,11),V(120),YD(120,10)
      COMMON /NLC/ GG(11,11),GINV(11,11),EM(15,120),BB(5),NT
      COMMON /NLC/ GA(120,5),NN
      DIMENSION A(11,11),DELX(20),E(20),F0(20)
      NP1=N+1
C---- QUADRATIC COEFFICIENTS
      II=0
      DO 30 K=1,M
      II=0
      DO 30 I=1,NP1
      IF(I.EQ.IL) GOTO 30
      II=II+1
      EM(II,K)=F(I,K)-F(IL,K)
30    CONTINUE
      DO 50 I=1,N
      F0(I)=0.0
      DO 50 K=1,M
50    F0(I)=F0(I)-F(IL,K)*EM(I,K)
C---- ELEMENTS OF THE MATRIX GAMMA,G
      DO 70 I=1,N
      DO 70 J=1,N
      GG(I,J)=0.0
      DO 70 K=1,M
70    GG(I,J)=GG(I,J)+EM(I,K)*EM(J,K)
      CALL MATIN
      DO 80 I=1,N
      E(I)=0.0
      DO 80 J=1,N
80    E(I)=E(I)+GINV(I,J)*F0(J)
C---- DEFINE THE SCALING MATRIX A
      II=0
      DO 101 I=1,NP1
      IF(I.EQ.IL) GOTO 101
      II=II+1
      DO 100 J=1,N
      A(II,J)=X(I,J)-X(IL,J)
100   CONTINUE
101   CONTINUE
C---- DETERMINE DEL X
      DO 110 I=1,N
      DELX(I)=0.0
      DO 110 J=1,N
110   DELX(I)=DELX(I)+A(J,I)*E(J)
      DO 120 J=1,N
120   XF(J)=X(IL,J)+DELX(J)
      INDEX=N+2
      CALL FVAL
      IF(Y(IL).LT.YVAL) GOTO 140
      TEMP=ABS(1-SOLD/YVAL)
      IF(TEMP.EQ.1) GOTO 150
      IF(TEMP.LE.EPSI) GOTO 150
      SOLD=YVAL
      DO 130 J=1,N
130   X(IL,J)=XF(J)
      NP2=N+2
      DO 135 K=1,M
135   F(IL,K)=F(NP2,K)
      IFLAG=2
      IQ=(3*N)/2
      GOTO 160
140   IFLAG=2
      IQ=3*N
      GOTO 160
150   IFLAG=1
      DO 155 J=1,N
155   X(IL,J)=XF(J)
      Y(IL)=YVAL
160   RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE  INDEXD
      COMMON /NLC/ EPSI,IFLAG,IL,IQ,INDEX,F(15,120),M,N
      COMMON /NLC/ SOLD,Y(20),YVAL,XF(11),X(11,11),V(120),YD(120,10)
      COMMON /NLC/ GG(11,11),GINV(11,11),EM(15,120),BB(5),NT
      COMMON /NLC/ GA(120,5),NN
	COMMON /NLCOUT/ FF(120),PARS(10),BPARS(10)
      SUM=0.0
      DO 200 I=1,M
200   SUM=SUM+V(I)
      XM=M
      YBAR=SUM/XM
      SST=0.0
      DO 240 I=1,M
240   SST=SST+(V(I)-YBAR)**2
      SSR=0.0
      DO 280 I=1,M
      FF(I)=0.0
      DO 260  J=1,NT
260   FF(I)=BB(J)*GA(I,J)+FF(I)
280   SSR=SSR+(FF(I)-V(I))**2
      XINDX=1-SSR/SST
      SIGMAR=SQRT(SSR/XM)
      DO 300 I=1,M
      DIFF=FF(I)-V(I)
      IF(V(I).EQ.0.) GO TO 295
      DIFF = DIFF*100./V(I)
      GO TO 300
295   DIFF=0.
300   CONTINUE
C
C----  WRITE(1) (FF(I),I=1,M)
C----  WRITE(1) (V(I),I=1,M)
      RETURN
      END
C---------------------------------------------------------------------------
      SUBROUTINE FVAL
      COMMON /NLC/ EPSI,IFLAG,IL,IQ,INDEX,F(15,120),M,N
      COMMON /NLC/ SOLD,Y(20),YVAL,XF(11),X(11,11),V(120),YD(120,10)
      COMMON /NLC/ GG(11,11),GINV(11,11),EM(15,120),BB(5),NT
      COMMON /NLC/ GA(120,5),NN
      DIMENSION GTGA(11,11),GT(5,120),GGG(5,120),B(5)
      DIMENSION G(120,5),A(11),TR(5),XP(11)
      EQUIVALENCE (GG(1,1),GTGA(1,1)),(BB(1),B(1)),(XF(1),A(1)),
     *(G(1,1),GA(1,1))
      DO 200 I=1,M
      DO 100 J=1,NN
100   XP(J)=YD(I,J)
C
C---- LOCATION OF TRANSFORMS
      CALL TRANS(TR,A,XP)
C
      DO 110 J=1,NT
110   GA(I,J)=TR(J)
200   CONTINUE
      DO 230 J=1,NT
      DO 230 I=1,M
230   GT(J,I)=GA(I,J)
      DO 280 K=1,NT
      DO 280 I=1,NT
      GTGA(K,I)=0.0
      DO 280 J=1,M
280   GTGA(K,I)=GTGA(K,I)+GT(K,J)*GA(J,I)
      HOLD=N
      N=NT
      CALL MATIN
      N=HOLD
      DO 350 K=1,NT
      DO 350 I=1,M
      GGG(K,I)=0.0
      DO 350 J=1,NT
350   GGG(K,I)=GGG(K,I)+GINV(K,J)*GT(J,I)
      DO 400 K=1,NT
      B(K)=0.0
      DO 400 J=1,M
400   B(K)=B(K)+GGG(K,J)*V(J)
      YVAL=0.0
      DO 460 I=1,M
      FF=0.0
      DO 240 J=1,NT
240   FF=B(J)*GA(I,J)+FF
      F(INDEX,I)=V(I)-FF
460   YVAL=(V(I)-FF)**2+YVAL
      RETURN
      END
