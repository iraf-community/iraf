c function area.f
c
c source
c   Bevington, pages 272-273.
c
c purpose
c   integrate the area beneath a set of data points
c
c usage
c   result = area (x, y, npts, nterms)
c
c description of parameters
c   x	   - array of data points for independent variable
c   y	   - array of data points for dependent variable
c   npts   - number of pairs of data points
c   nterms - number of terms in fitting polynomial
c
c subroutines and function subprograms required
c   integ (x, y, nterms, i1, x1, x2 sum)
c      fits a polynomial with nterms starting at i1
c      and integrates area from x1 to x2
c
	function area (x,y,npts,nterms)
	double precision sum
	dimension x(1),y(1)
11	sum=0.
	if (npts-nterms) 21,21,13
13	neven=2*(nterms/2)
	idelta=nterms/2-1
	if (nterms-neven) 31,31,51
c
c fit all points with one curve
c
21	x1=x(1)
	x2=x(npts)
23	call integ (x,y,npts,1,x1,x2,sum)
	goto 71
c
c even number of terms
c
31	x1=x(1)
	j=nterms-idelta
	x2=x(j)
	call integ (x,y,nterms,1,x1,x2,sum)
	i1=npts-nterms+1
	j=i1+idelta
	x1=x(j)
	x2=x(npts)
39	call integ (x,y,nterms,i1,x1,x2,sum)
	if (i1-2) 71,71,41
41	imax=i1-1
	do 46 i=2,imax
	j=i+idelta
	x1=x(j)
	x2=x(j+1)
46	call integ (x,y,nterms,i,x1,x2,sum)
	goto 71
c
c odd number of terms
c
51	x1=x(1)
	j=nterms-idelta
	x2=(x(j)+x(j-1))/2.
	call integ (x,y,nterms,1,x1,x2,sum)
	i1=npts-nterms+1
	j=i1+idelta
	x1=(x(j)+x(j+1))/2.
	x2=x(npts)
59	call integ (x,y,nterms,i1,x1,x2,sum)
	if (i1-2) 71,71,61
61	imax=i1-1
	do 66 i=2,imax
	j=i+idelta
	x1=(x(j+1)+x(j))/2.
	x2=(x(j+2)+x(j+1))/2.
66	call integ (x,y,nterms,i,x1,x2,sum)
71	area=sum
	return
	end
