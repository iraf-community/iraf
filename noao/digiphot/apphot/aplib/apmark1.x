include <gset.h>
include "../lib/apphot.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"
include "../lib/polyphot.h"
include "../lib/radprof.h"

# APMARK -- Procedure to mark center, fitsky and phot parameters on the display.

procedure apmark (ap, id, mkcenter, mksky, mkapert)

pointer	ap		# apphot pointer
pointer	id		# pointer to image display stream
int	mkcenter	# mark the computed center
int	mksky		# mark the sky annulus
int	mkapert		# mark the aperture(s)

int	i, marktype
pointer	sp, temp
real	inner_sky, outer_sky, apert
int	apstati(), gstati()
real	apstatr()
errchk	greactivate, gdeactivate, gamove, gadraw, gmark

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

	marktype = gstati (id, G_PMLTYPE)

	# Mark the center and shift on the display.
	if (mkcenter == YES) {
	    iferr {
	        call gseti (id, G_PMLTYPE, GL_SOLID)
	        call gamove (id, (apstatr (ap, XCENTER) - apstatr (ap, XSHIFT)),
		    (apstatr (ap, YCENTER) - apstatr (ap, YSHIFT)))
	        call gadraw (id, apstatr (ap, XCENTER), apstatr (ap, YCENTER))
	        call gmark (id, apstatr (ap, XCENTER), apstatr (ap, YCENTER),
		    GM_PLUS, -2.0, -2.0)
	    } then
		;
	}

	# Draw the sky annuli on the display.
	if (mksky == YES) {
	    iferr {
	        call gseti (id, G_PMLTYPE, GL_DASHED)
	        call gmark (id, apstatr (ap, SXCUR), apstatr (ap, SYCUR),
		    GM_PLUS, -2.0, -2.0)
	        inner_sky = 2.0 * apstatr (ap, SCALE) * apstatr (ap, ANNULUS)
	        call gmark (id, apstatr (ap, SXCUR), apstatr (ap, SYCUR),
		    GM_CIRCLE, -inner_sky, -inner_sky)
	        outer_sky = 2.0 * apstatr (ap, SCALE) * (apstatr (ap,
		    ANNULUS) + apstatr (ap, DANNULUS))
	        call gmark (id, apstatr (ap, SXCUR), apstatr (ap, SYCUR),
		    GM_CIRCLE, -outer_sky, -outer_sky)
	    } then
		;
	}

	# Draw the apertures on the display.
	if (mkapert == YES) {
	    iferr {
		call smark (sp)
	        call salloc (temp, apstati (ap, NAPERTS), TY_REAL)
	        call ap_arrayr (ap, APERTS, Memr[temp])
	        call gseti (id, G_PMLTYPE, GL_DASHED)
	        call gmark (id, apstatr (ap, PXCUR), apstatr (ap, PYCUR),
		    GM_PLUS, -2.0, -2.0)
	        do i = 1, apstati (ap, NAPERTS) {
		    apert = 2.0 * apstatr (ap, SCALE) * Memr[temp+i-1]
		    call gmark (id, apstatr (ap, PXCUR), apstatr (ap, PYCUR),
		        GM_CIRCLE, -apert, -apert)
	        }
	        call sfree (sp)
	    } then 
		call sfree (sp)
	}

	# Restore the mark type.
	call gseti (id, G_PMLTYPE, marktype)

	iferr {
	    call gdeactivate (id, 0)
	} then
	    return
end

# APPYMARK -- Procedure to mark center, fitsky and polyphot parameters on the
# display.

procedure appymark (ap, id, x, y, nver, mkcenter, mksky, mkpolygon)

pointer	ap		# apphot pointer
pointer	id		# pointer to image display stream
real	x[ARB]		# coordinates of x vertices
real	y[ARB]		# coordinates of y vertices
int	nver		# number of vertices
int	mkcenter	# mark the computed center
int	mksky		# mark the sky annulus
int	mkpolygon	# mark the aperture(s)

int	marktype, linetype
real	inner_sky, outer_sky
int	gstati()
real	apstatr()
errchk	greactivate, gdeactivate, gamove, gadraw, gmark, gline

begin
	if (id == NULL)
	    return
	if (mkcenter == NO && mksky == NO && mkpolygon == NO)
	    return
	iferr {
	    call greactivate (id, 0)
	} then
	    return

	marktype = gstati (id, G_PMLTYPE)
	linetype = gstati (id, G_PLTYPE)

	if (mkcenter == YES) {
	    iferr {
	        call gseti (id, G_PMLTYPE, GL_SOLID)
	        call gamove (id, (apstatr (ap, XCENTER) - apstatr (ap, XSHIFT)),
		    (apstatr (ap, YCENTER) - apstatr (ap, YSHIFT)))
	        call gadraw (id, apstatr (ap, XCENTER), apstatr (ap, YCENTER))
	        call gmark (id, apstatr (ap, XCENTER), apstatr (ap, YCENTER),
		    GM_PLUS, -2.0, -2.0)
	    } then
		;
	}

	if (mksky == YES) {
	    iferr {
	        call gseti (id, G_PMLTYPE, GL_DASHED)
	        call gmark (id, apstatr (ap, SXCUR), apstatr (ap, SYCUR),
		    GM_PLUS, -2.0, -2.0)
	        inner_sky = 2.0 * apstatr (ap, SCALE) * apstatr (ap, ANNULUS)
	        call gmark (id, apstatr (ap, SXCUR), apstatr (ap, SYCUR),
		    GM_CIRCLE, -inner_sky, -inner_sky)
	        outer_sky = 2.0 * apstatr (ap, SCALE) * (apstatr (ap,
		    ANNULUS) + apstatr (ap, DANNULUS))
	        call gmark (id, apstatr (ap, SXCUR), apstatr (ap, SYCUR),
		    GM_CIRCLE, -outer_sky, -outer_sky)
	    } then
		;
	}

	if (mkpolygon == YES) {
	    iferr {
	        call gseti (id, G_PLTYPE, GL_DASHED)
	        call gmark (id, apstatr (ap, PYCX), apstatr (ap, PYCY),
		    GM_PLUS, -2.0, -2.0)
	        call gpline (id, x, y, nver)
	    } then
		;
	}

	call gseti (id, G_PMLTYPE, marktype) 
	call gseti (id, G_PLTYPE, linetype) 

	iferr (call gdeactivate (id, 0))
	    return
end


# APRMARK -- Procedure to mark center, fitsky and radprof parameters on the
# display.

procedure aprmark (ap, id, mkcenter, mksky, mkapert)

pointer	ap		# apphot pointer
pointer	id		# pointer to image display stream
int	mkcenter	# mark the computed center
int	mksky		# mark the sky annulus
int	mkapert		# mark the aperture(s)

int	i, marktype
pointer	sp, temp
real	inner_sky, outer_sky, apert, radius, xc, yc
int	apstati(), gstati()
real	apstatr()
errchk	greactivate, gdeactivate, gamove, gadraw, gmark

begin
	if (id == NULL)
	    return
	if (mkcenter == NO && mksky == NO && mkapert == NO)
	    return

	iferr {
	    call greactivate (id, 0)
	} then
	    return

	marktype = gstati (id, G_PMLTYPE)

	# Mark the center and shift on the display.
	if (mkcenter == YES) {
	    iferr {
	        call gseti (id, G_PMLTYPE, GL_SOLID)
	        call gamove (id, (apstatr (ap, XCENTER) - apstatr (ap, XSHIFT)),
		    (apstatr (ap, YCENTER) - apstatr (ap, YSHIFT)))
	        call gadraw (id, apstatr (ap, XCENTER), apstatr (ap, YCENTER))
	        call gmark (id, apstatr (ap, XCENTER), apstatr (ap, YCENTER),
		    GM_PLUS, -2.0, -2.0)
		call gflush (id)
	    } then
		;
	}

	# Draw the sky annuli on the display.
	if (mksky == YES) {
	    iferr {
	        call gseti (id, G_PMLTYPE, GL_DASHED)
	        call gmark (id, apstatr (ap, SXCUR), apstatr (ap, SYCUR),
		    GM_PLUS, -2.0, -2.0)
	        inner_sky = 2.0 * apstatr (ap, SCALE) * apstatr (ap, ANNULUS)
	        call gmark (id, apstatr (ap, SXCUR), apstatr (ap, SYCUR),
		    GM_CIRCLE, -inner_sky, -inner_sky)
	        outer_sky = 2.0 * apstatr (ap, SCALE) * (apstatr (ap,
		    ANNULUS) + apstatr (ap, DANNULUS))
	        call gmark (id, apstatr (ap, SXCUR), apstatr (ap, SYCUR),
		    GM_CIRCLE, -outer_sky, -outer_sky)
		call gflush (id)
	    } then
		;
	}

	# Draw the apertures on the display.
	if (mkapert == YES) {
	    iferr {
	        call smark (sp)
	        call salloc (temp, apstati (ap, NAPERTS), TY_REAL)
	        call gseti (id, G_PMLTYPE, GL_DASHED)
	        call gmark (id, apstatr (ap, XCENTER), apstatr (ap, YCENTER),
		    GM_PLUS, -2.0, -2.0)
	        call ap_arrayr (ap, APERTS, Memr[temp])
	        do i = 1, apstati (ap, NAPERTS) {
		    apert = 2.0 * apstatr (ap, SCALE) * Memr[temp+i-1]
		    call gmark (id, apstatr (ap, XCENTER), apstatr (ap,
		        YCENTER), GM_CIRCLE, -apert, -apert)
	        }
	        xc = apstatr (ap, XCENTER)
	        yc = apstatr (ap, YCENTER)
	        radius = apstatr (ap, SCALE) * apstatr (ap, RPRADIUS)
	        call gamove (id, xc - radius, yc - radius)
	        call gadraw (id, xc + radius, yc - radius)
	        call gadraw (id, xc + radius, yc + radius)
	        call gadraw (id, xc - radius, yc + radius)
	        call gadraw (id, xc - radius, yc - radius)
	        call sfree (sp)
	    } then
		call sfree (sp)
	}

	call gseti (id, G_PMLTYPE, marktype) 

	iferr (call gdeactivate (id, 0))
	    return
end
