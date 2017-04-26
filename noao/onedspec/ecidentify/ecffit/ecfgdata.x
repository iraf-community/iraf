include	<pkg/gtools.h>

# ECF_GDATA -- Get graph data for the specified axis type from the fitting data.

procedure ecf_gdata (ecf, type, x, y, z, r, data, npts)

pointer	ecf			# GSURFIT pointer
int	type			# Axis type
double	x[npts]			# X fit data
double	y[npts]			# Y fit data
double	z[npts]			# Z fit data
double	r[npts]			# Residuals
real	data[npts]		# Graph data
int	npts			# Number of points

pointer	sp, v
include	"ecffit.com"

begin
	switch (type) {
	case 'p':
	    call achtdr (x, data, npts)
	case 'o':
	    call achtdr (y, data, npts)
	case 'w':
	    call achtdr (z, data, npts)
	case 'r':
	    call achtdr (r, data, npts)
	case 'v':
	    call smark (sp)
	    call salloc (v, npts, TY_DOUBLE)
	    call adivd (r, z, Memd[v], npts)
	    call amulkd (Memd[v], 300000.D0, Memd[v], npts)
	    call achtdr (Memd[v], data, npts)
	    call sfree (sp)
	}
end
