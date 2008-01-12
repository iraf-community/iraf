c function pchisq.f
c
c source
c   Bevington, pages 192-193.
c
c purpose
c   evaluate probability for exceeding chi square
c
c usage
c   result = pchisq (chisqr, nfree)
c
c description of parameters
c   chisqr - comparison value of reduced chi square
c   nfree  - number of degrees of freedom
c
c subroutines and function subprograms required
c   gamma (x)
c      calculates gamma function for integers and half-integers
c
c comments
c   calculation is approximate for nfree odd and
c      chi square greater than 50
c
	function pchisq (chisqr,nfree)
	double precision z,term,sum
11	if (nfree) 12,12,14
12	pchisq=0.
	goto 60
14	free=nfree
	z=chisqr*free/2.
	neven=2*(nfree/2)
	if (nfree-neven) 21,21,41
c
c number of degrees of freedom is even
c
21	imax=nfree/2
	term=1.
	sum=0.
31	do 34 i=1,imax
	fi=i
	sum=sum+term
34	term=term*z/fi
35	pchisq=sum*dexp(z)
	goto 60
c
c number of degrees of freedom is odd
c
41	if (z-25) 44,44,42
42	z=chisqr*(free-1.)/2.
	goto 21
44	pwr=free/2.
	term=1.
	sum=term/pwr
51	do 56 i=1,1000
	fi=i
	term=-term*z/fi
	sum=sum+term/(pwr+fi)
55	if (dabs(term/sum)-0.00001) 57,57,56
56	continue
57	pchisq=1.-(z**pwr)*sum/gamma(pwr)
60	return
	end
