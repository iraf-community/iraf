c subroutine regres.f
c
c source
c   Bevington, pages 172-175.
c
c purpose
c   make a mulitple linear regression fit to data with a specified
c      function which is linear in coefficients
c
c usage
c   call regres (x, y, sigmay, npts, nterms, m, mode, yfit,
c      a0, a, sigma0, sigmaa, r, rmul, chisqr, ftest)
c
c description of parameters
c   x	   - array of points for independent variable
c   y	   - array of points for dependent variable
c   sigmay - array of standard deviations for y data points
c   npts   - number of pairs of data points
c   nterms - number of coefficients
c   m	   - array of inclusion/rejection criteria for fctn
c   mode   - determines method of weighting least-squares fit
c	     +1 (instrumental) weight(i) = 1./sigmay(i)**2
c	      0 (no weighting) weight(i) = 1.
c	     -1 (statistical)  weight(i) = 1./y(i)
c   yfit   - array of calculated values of y
c   a0	   - constant term
c   a	   - array of coefficients
c   sigma0 - standard deviation of a0
c   sigmaa - array of standard deviations for coefficients
c   r	   - array of linear correlation coefficients
c   rmul   - multiple linear correlation coefficient
c   chisqr - reduced chi square for fit
c   ftest  - value of f for test of fit
c
c subroutines and function subprograms required
c   fctn (x, i, j, m)
c      evaluates the function for the jth term and the ith data point
c      using the array m to specify terms in the function
c   matinv (array, nterms, det)
c      inverts a symmetric two-dimensional matrix of degree nterms
c      and calculates its determinant
c
c comments
c (dim npts changed 100->1000 21-may-84 dct)
c   dimension statement valid for npts up to 100 and nterms up to 10
c   sigmaag changed to sigmaa in statement following statement 132
c
      subroutine regres (x,y,sigmay,npts,nterms,m,mode,yfit,
     *a0,a,sigma0,sigmaa,r,rmul,chisqr,ftest)
      double precision array,sum,ymean,sigma,chisq,xmean,sigmax
      dimension x(1),y(1),sigmay(1),m(1),yfit(1),a(1),sigmaa(1),
     *r(1)
      dimension weight(1000),xmean(10),sigmax(10),array(10,10)
      REAL FCTN
      EXTERNAL FCTN

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
31    if (mode) 32,37,39
32    if (y(i)) 35,37,33
33    weight(i)=1./y(i)
      goto 41
35    weight(i)=1./(-y(i))
      goto 41
37    weight(i)=1.
      goto 41
39    weight(i)=1./sigmay(i)**2
41    sum=sum+weight(i)
      ymean=ymean+weight(i)*y(i)
      do 44 j=1,nterms
44    xmean(j)=xmean(j)+weight(i)*fctn(x,i,j,m)
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
      sigmax(j)=sigmax(j)+weight(i)*(fctn(x,i,j,m)-xmean(j))**2
      r(j)=r(j)+weight(i)*(fctn(x,i,j,m)-xmean(j))*(y(i)-ymean)
      do 67 k=1,j
67    array(j,k)=array(j,k)+weight(i)*(fctn(x,i,j,m)-xmean(j))*
     *(fctn(x,i,k,m)-xmean(k))
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
81    call matinv (array,nterms,det)
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
108   yfit(i)=yfit(i)+a(j)*fctn(x,i,j,m)
111   do 113 i=1,npts
      yfit(i)=yfit(i)+a0
113   chisq=chisq+weight(i)*(y(i)-yfit(i))**2
      freen=npts-nterms-1
115   chisqr=chisq*wmean/freen
c
c calculate uncertainties
c
121   if (mode) 122,124,122
122   varnce=1./wmean
      goto 131
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
