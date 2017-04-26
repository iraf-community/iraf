c function factor.f
c
c source
c   Bevington, page 32.
c
c purpose
c   calculates factorial function for integers
c
c usage
c   result = factor (n)
c
c description of parameters
c   n      - integer argument
c
c subroutines and function subprograms required
c   none
c
	function factor (n)
	double precision fi,sum
11	factor=1.
	if (n-1) 40,40,13
13	if (n-10) 21,21,31
c
c n less than 11
c
21	do 23 i=2,n
	fi=i
23	factor=factor*fi
	goto 40
c
c n greater than 10
c
31	sum=0.
	do 34 i=11,n
	fi=i
34	sum=sum+dlog(fi)
35	factor=3628800.*dexp(sum)
40	return
	end
