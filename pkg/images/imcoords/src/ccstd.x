include <mach.h>
include <math.h>
include <math/gsurfit.h>
include <pkg/skywcs.h>

# CC_INIT_STD -- Get the parameter values relevant to the transformation from
# the cl or the database file. 
#
procedure cc_init_std (dt, record, geometry, lngunits, latunits, sx1,
	sy1, sx2, sy2, mw, coo)

pointer	dt			#I pointer to database file produced by geomap
char	record[ARB]		#I the name of the database record
int	geometry		#I the type of geometry to be computed
int	lngunits		#I the input ra / longitude units
int	latunits		#I the input dec / latitude units
pointer	sx1, sy1		#O pointers to the linear x and y surfaces
pointer	sx2, sy2		#O pointers to the x and y distortion surfaces
pointer	mw			#O pointer to the mwcs structure
pointer	coo			#O pointer to the coordinate structure

double	lngref, latref
int	recstat, proj
pointer	sp, projstr, projpars
int	cc_dtrecord(), strdic()
pointer	cc_celwcs()

begin
	call smark (sp)
	call salloc (projstr, SZ_FNAME, TY_CHAR)
	call salloc (projpars, SZ_LINE, TY_CHAR)

	if (dt == NULL) {

	    call cc_rinit (lngunits, latunits, sx1, sy1, mw, coo)
	    sx2 = NULL
	    sy2 = NULL

	} else {

	    recstat = cc_dtrecord (dt, record, geometry, coo, Memc[projpars],
	        lngref, latref, sx1, sy1, sx2, sy2)
	    if (recstat == ERR) {
		coo = NULL
		sx1 = NULL
		sy1 = NULL
		sx2 = NULL
		sy2 = NULL
    		mw = NULL
	    } else {
		call sscan (Memc[projpars])
		    call gargwrd (Memc[projstr], SZ_FNAME)
	        proj = strdic (Memc[projstr], Memc[projstr], SZ_FNAME,
		    WTYPE_LIST)
	        if (proj <= 0 || proj == WTYPE_LIN)
		    Memc[projpars] = EOS
		mw = cc_celwcs (coo, Memc[projpars], lngref, latref)
	    }
	}

	call sfree (sp)
end


# CC_FREE_STD -- Free the previously defined transformation.

procedure cc_free_std (sx1, sy1, sx2, sy2, mw, coo)

pointer	sx1, sy1		#U pointers to the linear x and y surfaces
pointer	sx2, sy2		#U pointers to the x and y distortion surfaces
pointer	mw			#U pointer to the mwcs structure
pointer	coo			#U pointer to the celestial coordinate structure

begin
	if (sx1 != NULL)
	    call dgsfree (sx1)
	if (sy1 != NULL)
	    call dgsfree (sy1)
	if (sx2 != NULL)
	    call dgsfree (sx2)
	if (sy2 != NULL)
	    call dgsfree (sy2)
	if (mw != NULL)
	    call mw_close (mw)
	if (coo != NULL)
	    call sk_close (coo)
end


# CC_RINIT -- Compute the required wcs structure from the input parameters.

procedure cc_rinit (lngunits, latunits, sx1, sy1, mw, coo)

int	lngunits	#I the input ra / longitude units
int	latunits	#I the input dec / latitude units
pointer	sx1		#O pointer to the linear x coordinate surface
pointer	sy1		#O pointer to the linear y coordinate surface
pointer	mw		#O pointer to the mwcs structure
pointer	coo		#O pointer to the celestial coordinate structure

double	xref, yref, xscale, yscale, xrot, yrot, lngref, latref
int	coostat, proj, tlngunits, tlatunits, pfd
pointer	sp, projstr
double	clgetd()
double	dgseval()
int	sk_decwcs(), sk_stati(), strdic(), open()
pointer	cc_celwcs(), cc_rdproj()
errchk	open()

begin
	# Allocate some workin space.
	call smark (sp)
	call salloc (projstr, SZ_LINE, TY_CHAR)

	# Get the reference point pixel coordinates.
	xref = clgetd ("xref")
	if (IS_INDEFD(xref))
	    xref = 0.0d0
	yref = clgetd ("yref")
	if (IS_INDEFD(yref))
	    yref = 0.0d0

	# Get the scale factors.
	xscale = clgetd ("xmag")
	if (IS_INDEFD(xscale))
	    xscale = 1.0d0
	yscale = clgetd ("ymag")
	if (IS_INDEFD(yscale))
	    yscale = 1.0d0

	# Get the rotation angles.
	xrot = clgetd ("xrotation")
	if (IS_INDEFD(xrot))
	    xrot = 0.0d0
	xrot = -DEGTORAD(xrot)
	yrot = clgetd ("yrotation")
	if (IS_INDEFD(yrot))
	    yrot = 0.0d0
	yrot = -DEGTORAD(yrot)

	# Initialize the linear part of the solution.
        call dgsinit (sx1, GS_POLYNOMIAL, 2, 2, NO, double (-MAX_REAL),
            double (MAX_REAL), double (-MAX_REAL), double (MAX_REAL))
        call dgsinit (sy1, GS_POLYNOMIAL, 2, 2, NO, double (-MAX_REAL),
            double (MAX_REAL), double (-MAX_REAL), double (MAX_REAL))
        call geo_rotmagd (sx1, sy1, xscale, yscale, xrot, yrot)
	call geo_xyshiftd (sx1, sy1, -dgseval (sx1, xref, yref),
	    -dgseval (sy1, xref, yref))

	lngref = clgetd ("lngref")
	if (IS_INDEFD(lngref))
	    lngref = 0.0d0
	latref = clgetd ("latref")
	if (IS_INDEFD(latref))
	    latref = 0.0d0

        coostat = sk_decwcs ("j2000", mw, coo, NULL)
        if (coostat == ERR || mw != NULL) {
            if (mw != NULL)
                call mw_close (mw)
        }
        if (lngunits <= 0)
            tlngunits = sk_stati (coo, S_NLNGUNITS)
	else
	    tlngunits = lngunits
        call sk_seti (coo, S_NLNGUNITS, tlngunits)
        if (latunits <= 0)
            tlatunits = sk_stati (coo, S_NLATUNITS)
	else
	    tlatunits = latunits
        call sk_seti (coo, S_NLATUNITS, tlatunits)

	call clgstr ("projection", Memc[projstr], SZ_LINE)
	iferr {
	    pfd = open (Memc[projstr], READ_ONLY, TEXT_FILE)
	} then {
	    proj = strdic (Memc[projstr], Memc[projstr], SZ_LINE, WTYPE_LIST)
	    if (proj <= 0 || proj == WTYPE_LIN)
	        Memc[projstr] = EOS
	} else {
	    proj = cc_rdproj (pfd, Memc[projstr], SZ_LINE)
	    call close (pfd)
	}
	mw = cc_celwcs (coo, Memc[projstr], lngref, latref) 

	call sfree (sp)
end


define  MAX_NITER       20

# CC_DO_STD -- Transform the coordinates using the full transformation
# computed by CCMAP.

procedure cc_do_std (x, y, xt, yt, sx1, sy1, sx2, sy2, forward)

double  x, y                    #I initial positions
double  xt, yt                  #O transformed positions
pointer sx1, sy1                #I pointer to linear surfaces
pointer sx2, sy2                #I pointer to distortion surfaces
bool    forward                 #I forward transform

double  f, fx, fy, g, gx, gy, denom, dx, dy
int     niter
pointer newsx, newsy
double  dgseval()

begin

        if (forward) {

            xt = dgseval (sx1, x, y)
            if (sx2 != NULL)
                xt = xt + dgseval (sx2, x, y)
            yt = dgseval (sy1, x, y)
            if (sy2 != NULL)
                yt = yt + dgseval (sy2, x, y)

        } else {

            xt = x / 1.0
            yt = y / 1.0

            call dgsadd (sx1, sx2, newsx)
            call dgsadd (sy1, sy2, newsy)
            niter = 0
            repeat {

                f = dgseval (newsx, xt, yt) - x
                call dgsder (newsx, xt, yt, fx, 1, 1, 0)
                call dgsder (newsx, xt, yt, fy, 1, 0, 1)

                g = dgseval (newsy, xt, yt) - y
                call dgsder (newsy, xt, yt, gx, 1, 1, 0)
                call dgsder (newsy, xt, yt, gy, 1, 0, 1)

                denom = fx * gy - fy * gx
                dx = (-f * gy + g * fy) / denom
                dy = (-g * fx + f * gx) / denom
                xt = xt + dx
                yt = yt + dy
                if (max (abs (dx), abs (dy), abs(f), abs(g)) < 1.0e-5)
                    break

                niter = niter + 1

            } until (niter >= MAX_NITER)

            call dgsfree (newsx)
            call dgsfree (newsy)
        }
end
