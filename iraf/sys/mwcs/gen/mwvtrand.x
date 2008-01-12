# MW_VTRAN -- Transform an array of N-dimensional points, expressed as a
# 2D vector where v[1,i] is point I of vector V.

procedure mw_vtrand (ct, v1, v2, ndim, npts)

pointer	ct			#I pointer to CTRAN descriptor
double	v1[ndim,npts]		#I points to be transformed
double	v2[ndim,npts]		#O vector to get the transformed points
int	ndim			#I dimensionality of each point
int	npts			#I number of points

int	i
errchk	mw_ctrand

begin
	do i = 1, npts
	    call mw_ctrand (ct, v1[1,i], v2[1,i], ndim)
end
