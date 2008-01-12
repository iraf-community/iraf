c function gamma.f
c
c source
c   Bevington, page 126.
c
c purpose
c   calculate the gamma function for integers and half-integers
c
c usage
c   result = gamma (x)
c
c description of parameters
c   x      - integer or half-integer
c
c subroutines or function subprograms required
c   factor (n)
c      calculates n factorial for integers
c
	function gamma (x)
	double precision prod,sum,fi
c
c integerize argument
c
11	n=x-0.25
	xn=n
13	if (x-xn-0.75) 31,31,21
c
c argument is integer
c
21	gamma=factor(n)
	goto 60
c
c argument is half-integer
c
31	prod=1.77245385
	if (n) 44,44,33
33	if (n-10) 41,41,51
41	do 43 i=1,n
	fi=i
43	prod=prod*(fi-0.5)
44	gamma=prod
	goto 60
51	sum=0.
	do 54 i=11,n
	fi=i
54	sum=sum+dlog(fi-0.5)
55	gamma=prod*639383.8623*dexp(sum)
60	return
	end
