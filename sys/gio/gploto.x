# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GPLOTV -- Plot a vector on an open graphics device.  This routine is
# provided for the convenience of the user who does not need to exercise
# fine control over the details of how the plot is generated, but who may
# wish to select an output device other than stdgraph or who may wish to
# leave the device open for annotation.

procedure gploto (gp, v, npts, x1, x2, title)

pointer	gp			# graphics descriptor
real	v[ARB]			# data vector
int	npts			# number of data points
real	x1, x2			# range of X in data vector
char	title[ARB]		# plot title
errchk	gswind, gascale, glabax

begin
	call gswind (gp, x1, x2, INDEF, INDEF)
	call gascale (gp, v, npts, 2)
	call glabax (gp, title, "", "")
	call gvline (gp, v, npts, x1, x2)
end
