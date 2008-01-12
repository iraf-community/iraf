C 	This routines was copied from the stsdas$pkg/analysis/gasp/gasplib/
C       directory. See the file stsdas$copyright.stsdas  for copyright
C	restrictions. 
	subroutine ksbsvd (u,w,v,m,n,b,x)
	parameter (nmax=1000)
	real*8 u(m,n),w(n),v(n,n),b(m),x(n),tmp(nmax)
	real*8 s

	do j=1,n
		s=0.
		if(w(j).ne.0.)then
			do i=1,m
				s=s+u(i,j)*b(i)
			enddo
			s=s/w(j)
		endif
		tmp(j)=s
	enddo
	do j=1,n
		s=0.
		do jj=1,n
			s=s+v(j,jj)*tmp(jj)
		enddo
		x(j)=s
	enddo
	return
	end
