c subroutine integ.f
c
c source
c   Bevington, page 274.
c
c purpose
c   integrate the area beneath two data points
c
c usage
c   call integ (x, y, nterms, i1, x1, x2, sum)
c
c description of parameters
c   x	   - array of data points for independent variable
c   y	   - array of data points for dependent variable
c   nterms - number of terms in fitting polymonial
c   i1	   - first data point for fitting polynomial
c   x1	   - first value of x for integration
c   x2	   - final value of x for integration
c
c subroutines and function subprograms required
c   none
c
c comments
c   dimension statement valid for nterms up to 10
c
	subroutine integ (x,y,nterms,i1,x1,x2,sum)
	double precision xjk,array,a,denom,deltax,sum
	dimension x(1),y(1)
	dimension array(10,10)
c
c construct square matrix and invert
c
11	do 17 j=1,nterms
	i=j+i1-1
	deltax=x(i)-x(i1)
	xjk=1.
	do 17 k=1,nterms
	array(j,k)=xjk
17	xjk=xjk*deltax
21	call matinv (array,nterms,det)
	if (det) 31,23,31
23	imid=i1+nterms/2
	sum=sum+y(imid)*(x2-x1)
	goto 40
c
c evaluate coefficients and integrate
c
31	dx1=x1-x(i1)
	dx2=x2-x(i1)
33	do 39 j=1,nterms
	i=j+i1-1
	a=0.
	do 37 k=1,nterms
37	a=a+y(i)*array(j,k)
	denom=j
39	sum=sum+(a/denom)*(dx2**j-dx1**j)
40	return
	end
