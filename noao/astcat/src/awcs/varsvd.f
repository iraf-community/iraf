C 	This routine was copied from stsdas$pkg/analysis/gasp/gasplib/.
C	See stsdas$copyright.stsdas for copyright restrictions.
C 
	subroutine varsvd (v,ma,w,cvm,ncvm)
	parameter (mmax=20)
	real*8 v(ma,ma),w(ma),cvm(ncvm,ncvm),wti(mmax)
	real*8 sum

	do i=1,ma
		wti(i)=0.
		if(w(i).ne.0.0d0) wti(i)=1./(w(i)*w(i))
	enddo
	do i=1,ma
		do j=1,i
			sum=0.
			do k=1,ma
				sum=sum+v(i,k)*v(j,k)*wti(k)
			enddo
			cvm(i,j)=sum
			cvm(j,i)=sum
		enddo
	enddo
	return
	end
