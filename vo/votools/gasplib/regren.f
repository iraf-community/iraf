c The following is the IDL version of the regression routine by Bevington
c Some modification have been made to the book version in regard to the input
c X array and the data type of some of the arrays.

c FUNCTION REGRESS,X,Y,W,YFIT,A0,SIGMA,FTEST,R,RMUL,CHISQ
c;
c;+
c; NAME:
c;	REGRESS
c; PURPOSE:
c;	Multiple linear regression fit.
c;	Fit the function:
c;	Y(i) = A0 + A(0)*X(0,i) + A(1)*X(1,i) + ... + 
c;		A(Nterms-1)*X(Nterms-1,i)
c; CATEGORY:
c;	G2 - Correlation and regression analysis.
c; CALLING SEQUENCE:
c;	Coeff = REGRESS(X,Y,W,YFIT,A0,SIGMA,FTEST,RMUL,CHISQ)
c; INPUTS:
c;	X = array of independent variable data.  X must 
c;		be dimensioned (Nterms, Npoints) where there are Nterms 
c;		coefficients to be found (independent variables) and 
c;		Npoints of samples.
c;	Y = vector of dependent variable points, must 
c;		have Npoints elements.
c;	W = vector of weights for each equation, must 
c;		be a Npoints elements vector.  For no 
c;		weighting, set w(i) = 1., for instrumental weighting 
c;		w(i) = 1/standard_deviation(Y(i)), for statistical 
c;		weighting w(i) = 1./Y(i)
c;
c; OUTPUTS:
c;	Function result = coefficients = vector of 
c;		Nterms elements.  Returned as a column 
c;		vector.
c;
c; OPTIONAL OUTPUT PARAMETERS:
c;	Yfit = array of calculated values of Y, Npoints 
c;		elements.
c;	A0 = Constant term.
c;	Sigma = Vector of standard deviations for 
c;		coefficients.
c;	Ftest = value of F for test of fit.
c;	Rmul = multiple linear correlation coefficient.
c;	R = Vector of linear correlation coefficient.
c;	Chisq = Reduced weighted chi squared.
c; COMMON BLOCKS:
c;	None.
c; SIDE EFFECTS:
c;	None.
c; RESTRICTIONS:
c;	None.
c; PROCEDURE:
c;	Adapted from the program REGRES, Page 172, 
c;		Bevington, Data Reduction and Error Analysis for the 
c;		Physical Sciences, 1969.
c;
c; MODIFICATION HISTORY:
c;	Written, DMS, RSI, September, 1982.
c;-
c;
c SY = SIZE(Y)		;GET DIMENSIONS OF X AND Y.
c SX = SIZE(X)
c IF (N_ELEMENTS(W) NE SY(1)) OR (SX(0) NE 2) OR (SY(1) NE SX(2)) THEN BEGIN
c	PRINT,'REGRESS - Incompatible arrays'
c	RETURN,0
c	ENDIF
c;
c NTERM = SX(1)		;# OF TERMS
c NPTS = SY(1)		;# OF OBSERVATIONS
c ;
c SW = TOTAL(W)		;SUM OF WEIGHTS
c YMEAN = TOTAL(Y*W)/SW	;Y MEAN
c XMEAN = (X * (REPLICATE(1.,NTERM) # W)) # REPLICATE(1./SW,NPTS)
c WMEAN = SW/NPTS
c WW = W/WMEAN
c ;
c NFREE = NPTS-1		;DEGS OF FREEDOM
c SIGMAY = SQRT(TOTAL(WW * (Y-YMEAN)^2)/NFREE) ;W*(Y(I)-YMEAN)
c XX = X- XMEAN # REPLICATE(1.,NPTS)	;X(J,I) - XMEAN(I)
c WX = REPLICATE(1.,NTERM) # WW * XX	;W(I)*(X(J,I)-XMEAN(I))
c SIGMAX = SQRT( XX*WX # REPLICATE(1./NFREE,NPTS)) ;W(I)*(X(J,I)-XM)*(X(K,I)-XM)
c R = WX #(Y - YMEAN) / (SIGMAX * SIGMAY * NFREE)
c ARRAY = INVERT((WX # TRANSPOSE(XX))/(NFREE * SIGMAX #SIGMAX))
c A = (R # ARRAY)*(SIGMAY/SIGMAX)		;GET COEFFICIENTS
c YFIT = A # X				;COMPUTE FIT
c A0 = YMEAN - TOTAL(A*XMEAN)		;CONSTANT TERM
c YFIT = YFIT + A0			;ADD IT IN
c FREEN = NPTS-NTERM-1 > 1		;DEGS OF FREEDOM, AT LEAST 1.
c CHISQ = TOTAL(WW*(Y-YFIT)^2)*WMEAN/FREEN ;WEIGHTED CHI SQUARED
c SIGMA = SQRT(ARRAY(INDGEN(NTERM)*(NTERM+1))/WMEAN/(NFREE*SIGMAX^2)) ;ERROR TERM
c RMUL = TOTAL(A*R*SIGMAX/SIGMAY)		;MULTIPLE LIN REG COEFF
c IF RMUL LT 1. THEN FTEST = RMUL/NTERM / ((1.-RMUL)/FREEN) ELSE FTEST=1.E6
c RMUL = SQRT(RMUL)
c RETURN,A
c END

      subroutine regren (x, ndim1, ndim2, y, weight, npts, nterms, yfit,
     *a0, a, sigmaa, chisqr)
      double precision sum, ymean, sigma, chisq
      integer npts, nterms
      double precision x(ndim1,ndim2),y(1),yfit(1)
      double precision r(20), array(20,20), sigmax(20), xmean(20)
      double precision chisqr, a0, a(1)
      real weight(1), sigmaa(1)
      real sigma0, ftest, freen, free1, rmul
      real fnpts, det, varnce, wmean, freej
      integer i, j, k
c
c initialize sums and arrays
c
11    sum=0.
      ymean=0.
      sigma=0.
      chisq=0.
      rmul=0.
      do 17 i=1,npts
17    yfit(i)=0.
21    do 28 j=1,nterms
      xmean(j)=0.
      sigmax(j)=0.
      r(j)=0.
      a(j)=0.
      sigmaa(j)=0.
      do 28 k=1,nterms
28    array(j,k)=0.
c
c accumulate weighted sums
c
30    do 50 i=1,npts
      sum=sum+weight(i)
      ymean=ymean+weight(i)*y(i)
      do 44 j=1,nterms
44    xmean(j)=xmean(j)+weight(i)*x(j,i)
50    continue
51    ymean=ymean/sum
      do 53 j=1,nterms
53    xmean(j)=xmean(j)/sum
      fnpts=npts
      wmean=sum/fnpts
      do 57 i=1,npts
57    weight(i)=weight(i)/wmean
c
c accumulate matrices r and array
c
61    do 67 i=1,npts
      sigma=sigma+weight(i)*(y(i)-ymean)**2
      do 67 j=1,nterms
      sigmax(j)=sigmax(j)+weight(i)*(x(j,i)-xmean(j))**2
      r(j)=r(j)+weight(i)*(x(j,i)-xmean(j))*(y(i)-ymean)
      do 67 k=1,j
67    array(j,k)=array(j,k)+weight(i)*(x(j,i)-xmean(j))*
     *(x(k,i)-xmean(k))
71    free1=npts-1
72    sigma=dsqrt(sigma/free1)
      do 78 j=1,nterms
74    sigmax(j)=dsqrt(sigmax(j)/free1)
      r(j)=r(j)/(free1*sigmax(j)*sigma)
      do 78 k=1,j
      array(j,k)=array(j,k)/(free1*sigmax(j)*sigmax(k))
78    array(k,j)=array(j,k)
c
c invert symmetric matrix
c
81    call minv20 (array,nterms,det)
      if (det) 101,91,101
91    a0=0.
      sigma0=0.
      rmul=0.
      chisqr=0.
      ftest=0.
      goto 150
c
c calculate coefficients, fit, and chi square
c
101   a0=ymean
102   do 108 j=1,nterms
      do 104 k=1,nterms
104   a(j)=a(j)+r(k)*array(j,k)
105   a(j)=a(j)*sigma/sigmax(j)
106   a0=a0-a(j)*xmean(j)
107   do 108 i=1,npts
108   yfit(i)=yfit(i)+a(j)*x(j,i)
111   do 113 i=1,npts
      yfit(i)=yfit(i)+a0
113   chisq=chisq+weight(i)*(y(i)-yfit(i))**2
      freen=npts-nterms-1
115   chisqr=chisq*wmean/freen
c
c calculate uncertainties
c
124   varnce=chisqr
131   do 133 j=1,nterms
132   sigmaa(j)=array(j,j)*varnce/(free1*sigmax(j)**2)
      sigmaa(j)=sqrt(sigmaa(j))
133   rmul=rmul+a(j)*r(j)*sigmax(j)/sigma
      freej=nterms
c +noao: When rmul = 1, the following division (stmt 135) would blow up.
c        It has been changed so ftest is set to -99999. in this case.
      if (1. - rmul) 135, 935, 135
135   ftest=(rmul/freej)/((1.-rmul)/freen)
      goto 136
935   ftest = -99999.
c -noao
136   rmul=sqrt(rmul)
141   sigma0=varnce/fnpts
      do 145 j=1,nterms
      do 145 k=1,nterms
145   sigma0=sigma0+varnce*xmean(j)*xmean(k)*array(j,k)/
     *(free1*sigmax(j)*sigmax(k))
146   sigma0=sqrt(sigma0)
150   return
      end
c subroutine matinv.f 
c
c source								       
c   bevington, pages 302-303.
c
c purpose
c   invert a symmetric matrix and calculate its determinant
c
c usage 
c   call matinv (array, norder, det)
c
c description of parameters
c   array  - input matrix which is replaced by its inverse
c   norder - degree of matrix (order of determinant)
c   det    - determinant of input matrix
c
c subroutines and function subprograms required 
c   none
c
c comment
c   dimension statement valid for norder up to 20
c
	subroutine minv20 (array,norder,det)
	double precision array,amax,save
	dimension array(20,20),ik(20),jk(20)
c
10	det=1.
11	do 100 k=1,norder
c
c find largest element array(i,j) in rest of matrix
c
	amax=0. 
21	do 30 i=k,norder
	do 30 j=k,norder
23	if (dabs(amax)-dabs(array(i,j))) 24,24,30
24	amax=array(i,j) 
	ik(k)=i 
	jk(k)=j 
30	continue
c
c interchange rows and columns to put amax in array(k,k)
c
31	if (amax) 41,32,41
32	det=0.
	goto 140
41	i=ik(k) 
	if (i-k) 21,51,43
43	do 50 j=1,norder
	save=array(k,j) 
	array(k,j)=array(i,j)
50	array(i,j)=-save
51	j=jk(k) 
	if (j-k) 21,61,53
53	do 60 i=1,norder
	save=array(i,k) 
	array(i,k)=array(i,j)
60	array(i,j)=-save
c
c accumulate elements of inverse matrix 
c
61	do 70 i=1,norder
	if (i-k) 63,70,63
63	array(i,k)=-array(i,k)/amax
70	continue
71	do 80 i=1,norder
	do 80 j=1,norder
	if (i-k) 74,80,74
74	if (j-k) 75,80,75
75	array(i,j)=array(i,j)+array(i,k)*array(k,j)
80	continue
81	do 90 j=1,norder
	if (j-k) 83,90,83
83	array(k,j)=array(k,j)/amax
90	continue
	array(k,k)=1./amax
100	det=det*amax
c
c restore ordering of matrix
c
101	do 130 l=1,norder
	k=norder-l+1
	j=ik(k) 
	if (j-k) 111,111,105
105	do 110 i=1,norder
	save=array(i,k) 
	array(i,k)=-array(i,j)
110	array(i,j)=save 
111	i=jk(k) 
	if (i-k) 130,130,113
113	do 120 j=1,norder
	save=array(k,j) 
	array(k,j)=-array(i,j)
120	array(i,j)=save 
130	continue
140	return
	end
