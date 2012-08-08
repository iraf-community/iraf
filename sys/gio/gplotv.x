# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GPLOTV -- Plot a vector.  This routine is provided for the convenience of
# the user who does not need to exercise fine control over the details of
# how the plot is generated.

procedure gplotv (v, npts, x1, x2, title)

real	v[ARB]			# data vector
int	npts			# number of data points
real	x1, x2			# range of X in data vector
char	title[ARB]		# plot title

pointer	gp
pointer	gopen()
errchk	gopen, gploto

begin
	gp = gopen ("stdgraph", NEW_FILE, STDGRAPH)
	call gploto (gp, v, npts, x1, x2, title)
	call gclose (gp)
end
