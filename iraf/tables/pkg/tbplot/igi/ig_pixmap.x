include <gset.h>
include "igi.h"

#  6/23/92  ZGL
## 6/25/92  Put entire raster to current WCS.  We depend on FITPIX and
#  LIMITS to adjust viewport and window to match pixmap.  We are working
#  in pixel space only for the time being.  gpcell() does not clip, I
#  believe.  ZGL
## 8/7/92  Include changeable colormap range for correctly rendering
#  "server" graphics.  ZGL
#  2/19/98  Trap invalid ZRANGE error before it causes floating point error
#	WJH

procedure ig_pixmap (igs)

pointer	igs		# igi parameters structure

begin
	call lcmdcat (igs, YES)
	call cmdcat  (igs, YES)

	if (MG_ZDATAP(PLOT_PARMS(igs)) == NULL) {
	    call eprintf ("No Z (pixmap) data \n")
	    return
	}

# Trap condition where ZRANGE is either not provided or is invalid
#   This avoids floating point error when mapping from ZRANGE to 0:255
#   19 Feb 1998  WJH
#
	if ( MG_ZMIN(PLOT_PARMS(igs)) == MG_ZMAX(PLOT_PARMS(igs)) ) {
	    call eprintf ("Invalid ZRANGE values for data \n")
	    return
	}
	call ii_pixmap (igs)
end


procedure ii_pixmap (igs)

pointer	igs		# igi parameters structure

int	igps
int	npts
int	gp
real	wl, wr, wb, wt
real	vl, vr, vb, vt
int	nx, ny
real	zmin, zmax
real	cmin, cmax
pointer	sp, rbuf, sbuf

begin
	gp = GIO_GP(igs)
	# Clip at viewport boundary
	call gseti (gp, G_CLIP, YES)

	igps = PLOT_PARMS(igs)

	# Size of pixmap raster
	nx = MG_ZNPTSX(igps) 
	ny = MG_ZNPTSY(igps)

	npts = nx * ny

	call smark (sp)
	call salloc (rbuf, npts, TY_REAL)
	call salloc (sbuf, npts, TY_SHORT)

	zmin = MG_ZMIN(igps)
	zmax = MG_ZMAX(igps)
	cmin = real (MG_CMMIN(igps))
	cmax = real (MG_CMMAX(igps))

	# Rescale the pixel values to fit in 0:255
	# or diminished range if including "server graphics"
	call amapr (Memr[MG_ZDATAP(igps)], Memr[rbuf], npts,
	    zmin, zmax, cmin, cmax)

	#  Make sure we are short
	call achtrs (Memr[rbuf], Mems[sbuf], npts)

	# Window (viewport in wcs)
	# Just draw whole image to whatever is the current WCS
	wl = 0.5
	wr = real (nx) + 0.5
	wb = 0.5
	wt = real (ny) + 0.5

	# Make sure window is on device
	vl = 0.0;  vr = 1.0
	vb = 0.0;  vr = 1.0
	call ndc_wcs (igs, 0.0, 0.0, vl, vb)
	call ndc_wcs (igs, 1.0, 1.0, vr, vt)

	wl = max (wl, min (wl, vr))
	wr = max (wr, min (wr, vr))
	wb = max (wb, min (wb, vt))
	wt = max (wt, min (wt, vt))

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Pixmap in window:  %f %f %f %f ")
		call pargr (wl)
		call pargr (wr)
		call pargr (wb)
		call pargr (wt)

	    call eprintf ("Z scale:  %f %f ")
		call pargr (zmin)
		call pargr (zmax)
	}

	# Draw the pixmap
	call gpcell (gp, Mems[sbuf], nx, ny, wl, wb, wr, wt)

	call sfree (sp)

	call gflush (gp)
end
