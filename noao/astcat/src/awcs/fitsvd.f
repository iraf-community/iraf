C 	This routine was copied from the stsdas$pkg/analysis/gasp/gasplib/
C	directory. See stsdas$copyright.stsdas for copyright restrictions.
C
	subroutine fitsvd (x, y, wg, npts, coef, nterms,
     *			  u, v, w, chisq)
	parameter(nmax=1000,mmax=50,tol=1.d-14)

	real   wg(npts)
	real*8 x(npts,nterms), y(npts), coef(nterms), v(nterms,nterms),
     *		u(npts,nterms), w(nterms), b(nmax)
	real*8 wmax, thresh, chisq, sum

	do i=1,npts
		do j=1,nterms
			u(i,j)=x(i,j)*wg(i)
		enddo
		b(i)=y(i)*wg(i)
	enddo
	call dcmpsv (u,npts,nterms,w,v)
	wmax=0.
	do j=1,nterms
		if(w(j).gt.wmax) wmax=w(j)
	enddo
	thresh=tol*wmax
	do j=1,nterms
		if(w(j).lt.thresh) w(j)=0.
	enddo
	call ksbsvd (u, w, v, npts, nterms, b, coef)
	chisq=0.
	do i=1,npts
		sum=0.
		do j=1,nterms
			sum=sum+coef(j)*x(i,j)
		enddo
		chisq=chisq+((y(i)-sum)*wg(i))**2
	enddo
	return
	end
