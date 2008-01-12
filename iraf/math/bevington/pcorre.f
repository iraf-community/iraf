c function pcorre.f
c
c source
c   Bevington, pages 124-125.
c
c purpose
c   evaluate probability for no correlation between two variables
c
c usage
c   result = pcorre (r, npts)
c
c description of parameters
c   r      - linear correlation coefficient
c   npts   - number of data points
c
c subroutines and function subprograms required
c   gamma (x)
c      calculates gamma function for integers and half-integers
c
	function pcorre (r,npts)
	double precision r2,term,sum,fi,fnum,denom
c
c evaluate number of degrees of freedom
c
11	nfree=npts-2
	if (nfree) 13,13,15
13	pcorre=0.
	goto 60
15	r2=r**2
	if (1.-r2) 13,13,17
17	neven=2*(nfree/2)
	if (nfree-neven) 21,21,41
c
c number of degrees of freedom is even
c
21	imax=(nfree-2)/2
	free=nfree
23	term=abs(r)
	sum=term
	if (imax) 60,26,31
26	pcorre=1.-term
	goto 60
31	do 36 i=1,imax
	fi=i
	fnum=imax-i+1
	denom=2*i+1
	term=-term*r2*fnum/fi
36	sum=sum+term/denom
	pcorre=1.128379167*(gamma((free+1.)/2.)/gamma(free/2.))
	pcorre=1.-pcorre*sum
	goto 60
c
c number of degrees of freedom is odd
c
41	imax=(nfree-3)/2
42	term=abs(r)*dsqrt(1.-r2)
43	sum=datan(r2/term)
	if (imax) 57,45,51
45	sum=sum+term
	goto 57
51	sum=sum+term
52	do 56 i=1,imax
	fnum=2*i
	denom=2*i+1
	term=term*(1.-r2)*fnum/denom
56	sum=sum+term
57	pcorre=1.-0.6366197724*sum
60	return
	end
