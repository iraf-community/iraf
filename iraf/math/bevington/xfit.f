c subroutine xfit.f
c
c source
c   Bevington, page 76.
c
c purpose
c   calculate the mean and estimated errors for a set of data points
c
c usage
c   call xfit (x, sigmax, npts, mode, xmean, sigmam, sigma)
c
c description of parameters
c   x      - array of data points
c   sigmax - array of standard deviations for data points
c   npts   - number of data points
c   mode   - determines method of weighting
c            +1 (instrumental) weight(i) = 1./sigmax(i)**2
c             0 (no weighting) weight(i) = 1.
c            -1 (statistical)  weight(i) = 1.
c   xmean  - weighted mean
c   sigmam - standard deviation of mean
c   sigma  - standard deviation of data
c
c subroutines and function subprograms required
c   none
c
	subroutine xfit (x,sigmax,npts,mode,xmean,sigmam,sigma)
	double precision sum,sumx,weight,free
	dimension x(1),sigmax(1)
c
c accumulate weighted sums
c
11	sum=0.
	sumx=0.
	sigma=0.
	sigmam=0.
20	do 32 i=1,npts
21	if (mode) 22,22,24
22	weight=1.
	goto 31
24	weight=1./sigmax(i)**2
31	sum=sum+weight
32	sumx=sumx+weight*x(i)
c
c evaluate mean and standard deviations
c
41	xmean=sumx/sum
51	do 52 i=1,npts
52	sigma=sigma+(x(i)-xmean)**2
	free=npts-1
54	sigma=dsqrt(sigma/free)
61	if (mode) 62,64,66
62	sigmam=dsqrt(xmean/sum)
	goto 70
64	sigmam=sigma/dsqrt(sum)
	goto 70
66	sigmam=dsqrt(1./sum)
70	return
	end
