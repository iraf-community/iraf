include	<pkg/gtools.h>

# ECF_GDATA -- Get graph data for the specified axis type from the fitting data.

procedure ecf_gdata (ecf, type, x, y, z, data, npts)

pointer	ecf			# GSURFIT pointer
int	type			# Axis type
double	x[npts]			# X fit data
double	y[npts]			# Y fit data
double	z[npts]			# Z fit data
real	data[npts]		# Graph data
int	npts			# Number of points

pointer	sp, y1, resid
include	"ecffit.com"

begin
	call smark (sp)

	switch (type) {
	case 'p':
	    call achtdr (x, data, npts)
	case 'o':
	    call achtdr (y, data, npts)
	case 'w':
	    call achtdr (z, data, npts)
	case 'r':
	    call salloc (y1, npts, TY_DOUBLE)
	    call salloc (resid, npts, TY_DOUBLE)
	    call altmd (y, Memd[y1], npts, double(slope), double(offset))
	    call dgsvector (ecf, x, Memd[y1], Memd[resid], npts)
	    call adivd (Memd[resid], Memd[y1], Memd[resid], npts)
	    call asubd (z, Memd[resid], Memd[resid], npts)
	    call achtdr (Memd[resid], data, npts)
	case 'v':
	    call salloc (y1, npts, TY_DOUBLE)
	    call salloc (resid, npts, TY_DOUBLE)
	    call altmd (y, Memd[y1], npts, double(slope), double(offset))
	    call dgsvector (ecf, x, Memd[y1], Memd[resid], npts)
	    call adivd (Memd[resid], Memd[y1], Memd[resid], npts)
	    call asubd (Memd[resid], z, Memd[resid], npts)
	    call adivd (Memd[resid], z, Memd[resid], npts)
	    call amulkd (Memd[resid], 300000.D0, Memd[resid], npts)
	    call achtdr (Memd[resid], data, npts)
	}

	call sfree (sp)
end
