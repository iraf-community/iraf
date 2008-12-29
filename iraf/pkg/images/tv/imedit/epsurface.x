include <mach.h>

define	DUMMY	6
 
# EP_SURFACE -- Draw a perspective view of a surface.  The altitude
# and azimuth of the viewing angle are variable.
 
procedure ep_surface(gp, data, ncols, nlines, angh, angv)
 
pointer	gp			# GIO pointer
real	data[ncols,nlines]	# Surface data to be plotted
size_t	ncols, nlines		# Dimensions of surface
real	angh, angv		# Orientation of surface (degrees)
 
int	wkid
int	i_val0, ival1
pointer	sp, work
 
int	first
real	vpx1, vpx2, vpy1, vpy2
common  /frstfg/ first
common  /noaovp/ vpx1, vpx2, vpy1, vpy2
 
begin
	call smark (sp)
	call salloc (work, 2 * ncols * nlines + ncols + nlines, TY_REAL)
 
	# Initialize surface common blocks
	first = 1
	call srfabd()
 
	# Define viewport.
	call ggview (gp, vpx1, vpx2, vpy1, vpy2) 
 
	# Link GKS to GIO
	wkid = 1
	call gopks (STDERR)
	call gopwk (wkid, DUMMY, gp)
	call gacwk (wkid)
 
	if ( ncols > MAX_INT ) {	# limited by sys/gio/ncarutil/srface.f
	    call error (0, "EP_SURFACE: Too large ncols (32-bit limit)")
	}
	if ( nlines > MAX_INT ) {	# limited by sys/gio/ncarutil/srface.f
	    call error (0, "EP_SURFACE: Too large nlines (32-bit limit)")
	}
	i_val0 = ncols
	i_val1 = nlines
	call ezsrfc (data, i_val0, i_val1, angh, angv, Memr[work])
 
	call gdawk (wkid)
	# We don't want to close the GIO pointer.
	#call gclwk (wkid)
	call gclks ()
 
	call sfree (sp)
end
