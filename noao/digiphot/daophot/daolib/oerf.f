      FUNCTION  ERF (XIN, XO, BETA, DFDXO, DFDBET)
C
C Numerically integrate a Gaussian function 
C
C          F = EXP {-0.5*[(x-XO)/BETA]**2] },
C
C from XIN-0.5 to XIN+0.5 using Simpson's 1/3 rule.  Also provide first
C derivative of the integral with respect to Xo and BETA.
C
C The number of intervals required to end up with an error less than
C ALPHA is greater than 
C
C   fourth root of [(fourth derivative of F w.r.t. x) / (180 * ALPHA)].
C
C Here I am using ALPHA = 0.00005.   N, the number of intervals, must 
C be an even number since the number of nodes, which equals the number
C of intervals plus one, must be odd.
C
C-----------------------------------------------------------------------
C
      BETASQ=BETA**2
C
C Estimate the number of intervals required by evaluating the fourth
C derivative of the Gaussian at XIN.
C
      X=((XIN-XO)/BETA)**2
      F=EXP(-0.5*X)
      N=MAX(2, IFIX( 3.247*((F*ABS(X*(X-6.)+3.))**0.25)/BETA )+1)
      IF (MOD(N,2) .NE. 0) N=N+1
      DX=1./FLOAT(N)
C
C Start with the lower end point (weight = 1).
C
      DELTAX=XIN-XO-0.5
      DXSQ=DELTAX**2
      F=EXP(-0.5*DXSQ/BETASQ)
      ERF=F
      DFDXO=F*DELTAX
      DFDBET=F*DXSQ
C
C Now include the end points of each subinterval except the last one.  
C If it is an odd-numbered subinterval, weight = 4.  If even, 
C weight = 2.
C
      DO 1010 I=1,N-1
      DELTAX=DELTAX+DX
      DXSQ=DELTAX**2
      F=EXP(-0.5*DXSQ/BETASQ)
      FWT=F*2.*FLOAT(1+MOD(I,2))
      ERF=ERF+FWT
      DFDXO=DFDXO+DELTAX*FWT
      DFDBET=DFDBET+DXSQ*FWT
 1010 CONTINUE
C
C Now add the upper end point (weight = 1) and multiply by DX/3.
C
      DELTAX=DELTAX+DX
      DXSQ=DELTAX**2
      F=EXP(-0.5*DXSQ/BETASQ)
      DX=DX/3.
      ERF=DX*(ERF+F)
      IF (ERF .LT. 1.E-19) ERF=0.0
      DFDXO=DX*(DFDXO+DELTAX*F)/BETASQ
      IF (ABS(DFDXO) .LT. 1.E-19) DFDXO=0.0
      DFDBET=DX*(DFDBET+F*DXSQ)/(BETASQ*BETA)
      IF (ABS(DFDBET) .LT. 1.E-19) DFDBET=0.0
C
      RETURN
      END
