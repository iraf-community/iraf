c subroutine smooth.f
c
c source
c   Bevington, page 260.
c
c purpose
c   smooth a set of data points by averaging adjacent channels
c
c usage
c   call smooth (y, npts)
c
c description of parameters
c   y	   - array of data points
c   npts   - number of data points
c
c subroutines and function subprograms required
c   none
c
	subroutine smooth (y,npts)
	dimension y(1)
11	imax=npts-1
	yi=y(1)
21	do 24 i=1,imax
	ynew=(yi+2.*y(i)+y(i+1))/4.
	yi=y(i)
24	y(i)=ynew
25	y(npts)=(yi+3.*y(npts))/4.
	return
	end
