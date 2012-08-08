include <gset.h>

# DP_EOMARK -- Procedure to mark center, fitsky and phot parameters on the
# display.

procedure dp_eomark (id, xcenter, ycenter, srin, srout, apr, mkcenter, mksky,
	mkapert)

pointer	id		# pointer to image display stream
real	xcenter		# the center x coordinate
real	ycenter		# the center y coordinate
real	srin		# the inner radius of the sky annulus
real	srout		# the outer radius of the sky annulus
real	apr		# the maximum photometry aperture radius
int	mkcenter	# mark the computed center
int	mksky		# mark the sky annulus
int	mkapert		# mark the aperture(s)

real	rad
int	marktype
int	gstati()

errchk	greactivate, gdeactivate

begin
	if (id == NULL)
	    return
	if (mkcenter == NO && mksky == NO && mkapert == NO)
	    return
	iferr {
	    call greactivate (id, 0)
	} then {
	    return
	}

	# Save old mark type.
	marktype = gstati (id, G_PMLTYPE)

	# Mark the center and shift on the display.
	if (mkcenter == YES) {
	    call gseti (id, G_PMLTYPE, GL_SOLID)
	    call gmark (id, xcenter, xcenter, GM_PLUS, -2.0, -2.0)
	}

	# Draw the sky annuli on the display.
	if (mksky == YES) {
	    call gseti (id, G_PMLTYPE, GL_DASHED)
	    rad = 2.0 * srin
	    call gmark (id, xcenter, ycenter, GM_CIRCLE, -rad, -rad)
	    rad = 2.0 * srout
	    call gmark (id, xcenter, ycenter, GM_CIRCLE, -rad, -rad)
	}

	# Draw the apertures on the display.
	if (mkapert == YES) {
	    call gseti (id, G_PMLTYPE, GL_DASHED)
	    rad = 2.0 * apr
	    call gmark (id, xcenter, ycenter, GM_CIRCLE, -rad, -rad)
	}

	# Restore the mark type.
	call gseti (id, G_PMLTYPE, marktype)

	iferr {
	    call gdeactivate (id, 0)
	} then
	    return
end
