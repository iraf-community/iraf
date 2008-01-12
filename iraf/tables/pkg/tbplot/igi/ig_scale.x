include <gset.h>
include "igi.h"
include "commands.h"

#  IG_SCALE -- Define the viewport

#  8/20/91 Removed ^Ls. ZGL
## 6/24/92 Added FITPIX to adjust viewport for pixmap.  ZGL
## 7/21/92 Make LOCATION adjust current viewport for unity aspect.
##         Use get_real() in ig_scale().  ZGL

procedure ig_scale (cmd, igs)

int	cmd			# Command index
pointer	igs			# igi parameters structure

pointer	tokvals			# Token value structure
int	token
real	vpl, vpr, vpb, vpt	# Viewport edges on virtual page
real	gvl, gvr, gvb, gvt	# GIO viewport

int	gettok()
real	get_real()

errchk	get_real

begin
	call lcmdcat (igs, YES)

	tokvals = TOKEN_VALUE(igs)

	# Get the first argument
	token = gettok (igs)

	if (IS_NEWCOMMAND(token)) {
	    # No argument;  list the current viewport scale
	    call show_scale (STDOUT, igs)
	    return

	} else if (token == CONSTANT) {
	    # First argument
	    if (LOP_TYPE(tokvals) == TY_REAL)
		vpl = LOP_VALR(tokvals)

	    else
		vpl = real (LOP_VALI(tokvals))

	} else {
	    call eprintf ("Numeric value required ")
	    return
	}

	call lcmdcat (igs, NO)

	if (IS_INDEF(vpl)) {
	    #  INDEF args indicate using currently set viewport
	    #  but adjust aspect ratio

	    vpr = INDEF
	    vpb = INDEF
	    vpt = INDEF

	    call cmdcat  (igs, NO)

	    switch (cmd) {
	    case FITPIX:
		#  Preserve aspect ratio of image
		call ii_fitpix (igs, vpl, vpr, vpb, vpt)

	    case LOCATION:
		#  Change to unity aspect ratio
		call ii_location (igs, vpl, vpr, vpb, vpt)
	    }

	} else {
	    # Should have three remaining arguments
	    # Right edge
	    iferr (vpr = get_real (igs))
		return

	    # Bottom edge
	    iferr (vpb = get_real (igs))
		return

	    # Top edge
	    iferr (vpt = get_real (igs))
		return

	    call cmdcat  (igs, NO)

	    switch (cmd) {
	    case FITPIX:
		call ii_fitpix (igs, vpl, vpr, vpb, vpt)

	    case LOCATION:
		call ii_location (igs, vpl, vpr, vpb, vpt)

	    case PHYSICAL:
		call ii_physical (igs, vpl, vpr, vpb, vpt)

	    case VPAGE:
		call ii_vpage (igs, vpl, vpr, vpb, vpt)
	    }
	}

	if (DEBUG_OUTPUT(igs) == YES) {
	    call ggview (GIO_GP(igs), gvl, gvr, gvb, gvt)
	    call eprintf ("GIO Viewport:\t\t%.3f\t%.3f\t%.3f\t%.3f (NDC) ")
		call pargr (gvl)
		call pargr (gvr)
		call pargr (gvb)
		call pargr (gvt)
	}
end


procedure ii_location (igs, left, right, bottom, top)

pointer	igs				# igi parameters structure
real	left, right, bottom, top	# Viewport on virtual page

pointer	igps				# Plot parameters structure
real	vpl, vpr, vpb, vpt		# Viewport on virtual page
real	gvl, gvr, gvb, gvt		# Viewport in NDC
real	xr, yr
real	dasp
real	dl, dr, db, dt

real	ggetr()
bool	fp_equalr()

begin
	igps = PLOT_PARMS(igs)

	if (IS_INDEF(left)) {
	    # Start with current viewport but adjust for unity aspect on device
	    # These are in VPC here
	    vpl = MG_VIEWLEFT(igps)
	    vpr = MG_VIEWRIGHT(igps)
	    vpb = MG_VIEWBOTTOM(igps)
	    vpt = MG_VIEWTOP(igps)

	    # Convert VPC to NDC
	    call vpc_ndc (igs, vpl, vpb, gvl, gvb)
	    call vpc_ndc (igs, vpr, vpt, gvr, gvt)

	    # Device resolution
	    xr = ggetr (GIO_GP(igs), "xr")
	    yr = ggetr (GIO_GP(igs), "yr")

	    #  Viewport edges on device
	    dl = gvl * xr;  dr = gvr * xr
	    db = gvb * yr;  dt = gvt * yr

	    #  Aspect ratio of viewport on device
	    dasp = (dt - db) / (dr - dl)

	    #  Adjust viewport
	    if (dasp < 1.0) {
		# Pixmap taller than viewport
		dr = dt - db

		#  Adjusted viewport in NDC
		gvr = gvl + dr / xr

	    } else if (dasp > 1.0) {
		# Viewport taller than pixmap
		dt = dr - dl

		#  Adjusted viewport in NDC
		gvt = gvb + dt / yr
	    }

	    if (!fp_equalr (dasp, 1.0)) {
		# Convert adjusted viewport from NDC to VPC
		call ndc_vpc (igs, gvr, gvt, vpr, vpt)
		MG_VIEWRIGHT(igps) = vpr
		MG_VIEWTOP(igps)   = vpt
	    }

	    if (DEBUG_OUTPUT(igs) == YES) {
		call eprintf ("Adjusted viewport:  %f %f %f %f ")
		    call pargr (vpl)
		    call pargr (vpr)
		    call pargr (vpb)
		    call pargr (vpt)
	    }

	} else {
	    # Use input
	    # Clip to vpage edges
	    vpl = max (min (left,   1.0), 0.0)
	    vpr = max (min (right,  1.0), 0.0)
	    vpb = max (min (bottom, 1.0), 0.0)
	    vpt = max (min (top,    1.0), 0.0)

	    # Location of viewport on virtual page
	    MG_VIEWLEFT(igps)   = vpl
	    MG_VIEWRIGHT(igps)  = vpr
	    MG_VIEWBOTTOM(igps) = vpb
	    MG_VIEWTOP(igps)    = vpt
	}

	# Set the virtual page and viewport on the device
	call vpage (igs)
end


procedure ii_fitpix (igs, left, right, bottom, top)

#  Set the viewport on the virtual page like LOCATION, but modifie adjust the specified edges to match a pixmap raster previously read so it will be rendered at the natural aspect ratio, assuming square pixels.  The arguments are in virtual page coordinates (VPC), a fraction of the virtual page.

## 7/8/92  Added INDEF option to use curent viewport but adjust for
##         image aspect ratio.  ZGL
## 11/16/93 Fixed swapped X and Y raster sizes causing incorrect scaling.

pointer	igs				# igi parameters structure
real	left, right, bottom, top	# Viewport on virtual page

pointer	igps				# Plot parameters structure
real	vl, vr, vb, vt			# Viewport in VPC
real	gvl, gvr, gvb, gvt		# Viewport in NDC
real	dl, dr, db, dt
real	xr, yr
int	nx, ny
real	pasp, dasp

real	ggetr()

begin
	igps = PLOT_PARMS(igs)

	if (MG_ZDATAP(igps) == NULL) {
	    call eprintf ("No Z data ")
	    return
	}

	if (IS_INDEF(left)) {
	    # Start with current viewport but adjust for aspect of image
	    # These are in VPC here
	    vl = MG_VIEWLEFT(igps)
	    vr = MG_VIEWRIGHT(igps)
	    vb = MG_VIEWBOTTOM(igps)
	    vt = MG_VIEWTOP(igps)

	} else {
	    # Set viewport from arguments
	    # Clip to edges of vpage
	    vl = max (min (left,   1.0), 0.0)
	    vr = max (min (right,  1.0), 0.0)
	    vb = max (min (bottom, 1.0), 0.0)
	    vt = max (min (top,    1.0), 0.0)

	    MG_VIEWLEFT(igps)   = vl
	    MG_VIEWRIGHT(igps)  = vr
	    MG_VIEWBOTTOM(igps) = vb
	    MG_VIEWTOP(igps)    = vt
	}

	# Convert VPC to NDC
	call vpc_ndc (igs, vl, vb, gvl, gvb)
	call vpc_ndc (igs, vr, vt, gvr, gvt)

	# Device resolution
	xr = ggetr (GIO_GP(igs), "xr")
	yr = ggetr (GIO_GP(igs), "yr")

	#  Viewport edges on device
	dl = gvl * xr;  dr = gvr * xr
	db = gvb * yr;  dt = gvt * yr

	#  Aspect ratio of viewport on device
	dasp = (dt - db) / (dr - dl)

	# Size of pixmap raster
	nx = MG_ZNPTSX(igps) 
	ny = MG_ZNPTSY(igps)

	#  Aspect ratio of pixmap raster
	pasp = real (ny) / real (nx)

	#  Adjust viewport
	if (pasp > dasp) {
	    # Pixmap taller than viewport
	    dr = (dt - db) / pasp

	    #  Adjusted viewport in NDC
	    gvr = gvl + dr / xr

	} else if (dasp > pasp) {
	    # Viewport taller than pixmap
	    dt = (dr - dl) * pasp

	    #  Adjusted viewport in NDC
	    gvt = gvb + dt / yr
	}

	# Set the GIO viewport (NDC)
	call gsview (GIO_GP(igs), gvl, gvr, gvb, gvt)

	if (pasp != dasp) {
	    # Convert adjusted viewport from NDC to VPC
	    call ndc_vpc (igs, gvr, gvt, vr, vt)
	    MG_VIEWRIGHT(igps) = vr
	    MG_VIEWTOP(igps)   = vt
	}

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Adjusted viewport:  %f %f %f %f ")
		call pargr (MG_VIEWLEFT(igps))
		call pargr (MG_VIEWRIGHT(igps))
		call pargr (MG_VIEWBOTTOM(igps))
		call pargr (MG_VIEWTOP(igps))
	}
end


procedure ndc_vpc (igs, gvx, gvy, vpx, vpy)

pointer	igs			# igi parameters structure
real	vpx, vpy		# Virtual page coordinates
real	gvx, gvy		# GIO viewport (NDC)

pointer	igps			# Plot parameters structure

begin
	igps = PLOT_PARMS(igs)

	vpx = (gvx - MG_PAGELEFT(igps)) /
	      (MG_PAGERIGHT(igps) - MG_PAGELEFT(igps))

	vpy = (gvy - MG_PAGEBOTTOM(igps)) /
	      (MG_PAGETOP(igps) - MG_PAGEBOTTOM(igps))
end


procedure ii_physical (igs, left, right, bottom, top)

pointer	igs				# igi parameters structure
real	left, right, bottom, top	# Viewport

pointer	igps				# Plot parameters structure
real	vpl, vpr, vpb, vpt		# Virtual page
real	xs, ys

real	ggetr()

begin
	igps = PLOT_PARMS(igs)

	# Physical device scale in inches
	xs = 39.37 * ggetr (GIO_GP(igs), "xs")
	ys = 39.37 * ggetr (GIO_GP(igs), "ys")

	# Location of virtual page in physical device coordinates (inches)
	vpl = left / xs
	vpr = right / xs
	vpb = bottom / ys
	vpt = top / ys

	# Clip to physical edges
	vpl = max (min (vpl, 1.0), 0.0)
	vpr = max (min (vpr, 1.0), 0.0)
	vpb = max (min (vpb, 1.0), 0.0)
	vpt = max (min (vpt, 1.0), 0.0)

	# Location of virtual page in NDC
	MG_PAGELEFT(igps)   = vpl
	MG_PAGERIGHT(igps)  = vpr
	MG_PAGEBOTTOM(igps) = vpb
	MG_PAGETOP(igps)    = vpt

	# Go back to one pane
	call gseti (GIO_GP(igs), G_WCS, 1)
	MG_NXPANE(igps) = 1
	MG_NYPANE(igps) = 1
	MG_PANE(igps)   = 1

	# Set the virtual page and viewport on the device
	call vpage (igs)
end


procedure ii_vpage (igs, left, right, bottom, top)

pointer	igs				# igi parameters structure
real	left, right, bottom, top	# Virtual page in NDC

pointer	igps				# Plot parameters structure
real	vpl, vpr, vpb, vpt		# Virtual page

begin
	igps = PLOT_PARMS(igs)

	# Clip to physical edges
	vpl = max (min (left,   1.0), 0.0)
	vpr = max (min (right,  1.0), 0.0)
	vpb = max (min (bottom, 1.0), 0.0)
	vpt = max (min (top,    1.0), 0.0)

	# Location of virtual page in NDC
	MG_PAGELEFT(igps)   = vpl
	MG_PAGERIGHT(igps)  = vpr
	MG_PAGEBOTTOM(igps) = vpb
	MG_PAGETOP(igps)    = vpt

	# Go back to one pane
	call gseti (GIO_GP(igs), G_WCS, 1)
	MG_NXPANE(igps) = 1
	MG_NYPANE(igps) = 1
	MG_PANE(igps)   = 1

	# Set the virtual page and viewport on the device
	call vpage (igs)
end
