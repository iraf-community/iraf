include <imhdr.h>
include <math.h>
include <mwset.h>
include <pkg/skywcs.h>


# RG_CELTOSTD - Convert the longitude / latitude coordinates to standard
# coordinates given the position of the reference point and the form of
# the projection.

procedure rg_celtostd (projection, lngref, latref, xi, eta, npts, reflng,
        reflat, lngunits, latunits)

char    projection[ARB]         #I the projection type
double  lngref[ARB]             #I the input ra / longitude coordinates
double  latref[ARB]             #I the input dec / latitude coordinates
double  xi[ARB]                 #O the output ra / longitude std coordinates
double  eta[ARB]                #O the output dec / latitude std coordinates
int     npts                    #I the number of data points
double  reflng                  #I the ra / longitude reference point
double  reflat                  #I the dec / latitude reference point
int     lngunits                #I the ra / longitude units
int     latunits                #I the dec / latitude units


double  tlngref, tlatref
int     i
pointer mw, ct
pointer rg_projwcs(),  mw_sctran()
errchk  mw_sctran()

begin
        # Initialize the projection transformation.
        mw = rg_projwcs (projection, reflng, reflat, lngunits, latunits)

        # Compile the transformation.
        ct = mw_sctran (mw, "world", "logical", 03B)

        # Evaluate the standard coordinates.
        do i = 1, npts {

            switch (lngunits) {
            case SKY_DEGREES:
                tlngref = lngref[i]
            case SKY_RADIANS:
                tlngref = RADTODEG(lngref[i])
            case SKY_HOURS:
                tlngref = 15.0d0 * lngref[i]
            default:
                tlngref = lngref[i]
            }

            switch (latunits) {
            case SKY_DEGREES:
                tlatref = latref[i]
            case SKY_RADIANS:
                tlatref = RADTODEG(latref[i])
            case SKY_HOURS:
                tlatref = 15.0d0 * latref[i]
            default:
                tlatref = latref[i]
            }

            call mw_c2trand (ct, tlngref, tlatref, xi[i], eta[i])
        }

        call mw_close (mw)

end


# RG_STDTOCEL - Convert the longitude / latitude coordinates to standard
# coordinates given the position of the reference point and the form of
# the projection.

procedure rg_stdtocel (projection, xi, eta, lngfit, latfit, npts, reflng,
        reflat, lngunits, latunits)

char    projection[ARB]         #I the sky projection geometry
double  xi[ARB]                 #I the output ra / longitude std coordinates
double  eta[ARB]                #I the output dec / latitude std coordinates
double  lngfit[ARB]             #O the input ra / longitude coordinates
double  latfit[ARB]             #O the input dec / latitude coordinates
int     npts                    #I the number of data points
double  reflng                  #I the ra / longitude reference point
double  reflat                  #I the dec / latitude reference point
int     lngunits                #I the ra / longitude units
int     latunits                #I the dec / latitude units

double  tlngref, tlatref
int     i
pointer mw, ct
pointer rg_projwcs(), mw_sctran()
errchk  mw_sctran()

begin
        # Initialize the projection transformation.
        mw = rg_projwcs (projection, reflng, reflat, lngunits, latunits)

        # Compile the transformation.
        ct = mw_sctran (mw, "logical", "world", 03B)

        # Evaluate the standard coordinates.
        do i = 1, npts {

            call mw_c2trand (ct, xi[i], eta[i], tlngref, tlatref)

            switch (lngunits) {
            case SKY_DEGREES:
                lngfit[i] = tlngref
            case SKY_RADIANS:
                lngfit[i] = DEGTORAD(tlngref)
            case SKY_HOURS:
                lngfit[i] = tlngref / 15.0d0
            default:
                lngfit[i] = tlngref
            }

            switch (latunits) {
            case SKY_DEGREES:
                latfit[i] = tlatref
            case SKY_RADIANS:
                latfit[i] = DEGTORAD(tlatref)
            case SKY_HOURS:
                latfit[i] = tlatref / 15.0d0
            default:
                latfit[i] = tlatref
            }

        }

        call mw_close (mw)
end


# RG_PROJWCS -- Set up a projection wcs given the projection type, the
# coordinates of the reference point, and the reference point units.

pointer procedure rg_projwcs (projection, reflng, reflat, lngunits, latunits)

char    projection[ARB]         #I the projection type
double  reflng                  #I the ra / longitude reference point
double  reflat                  #I the dec / latitude reference point
int     lngunits                #I the ra / longitude units
int     latunits                #I the dec / latitude units

int     ndim
pointer sp, projstr, projpars, wpars, ltm, ltv, cd, r, w, mw, axes
pointer mw_open()

begin
        ndim = 2

        # Allocate working space.
        call smark (sp)
        call salloc (projstr, SZ_FNAME, TY_CHAR)
        call salloc (projpars, SZ_LINE, TY_CHAR)
        call salloc (wpars, SZ_LINE, TY_CHAR)
        call salloc (ltm, ndim * ndim, TY_DOUBLE)
        call salloc (ltv, ndim, TY_DOUBLE)
        call salloc (cd, ndim * ndim, TY_DOUBLE)
        call salloc (r, ndim, TY_DOUBLE)
        call salloc (w, ndim, TY_DOUBLE)
        call salloc (axes, IM_MAXDIM, TY_INT)

        # Open the wcs.
        mw = mw_open (NULL, ndim)

        # Set the axes and projection type.
        Memi[axes] = 1
        Memi[axes+1] = 2
        if (projection[1] == EOS)
            call mw_swtype (mw, Memi[axes], ndim, "linear", "")
        else {
            call sscan (projection)
                call gargwrd (Memc[projstr], SZ_FNAME)
                call gargstr (Memc[projpars], SZ_LINE)
            call sprintf (Memc[wpars], SZ_LINE,
                "axis 1: axtype = ra %s axis 2: axtype = dec %s")
                call pargstr (Memc[projpars])
                call pargstr (Memc[projpars])
            call mw_swtype (mw, Memi[axes], ndim, Memc[projstr], Memc[wpars])
        }


        # Set the lterm.
        call mw_mkidmd (Memd[ltm], ndim)
        call aclrd (Memd[ltv], ndim)
        call mw_sltermd (mw, Memd[ltm], Memd[ltv], ndim)

        # Set the wterm.
        call mw_mkidmd (Memd[cd], ndim)
        call aclrd (Memd[r], ndim)
        switch (lngunits) {
        case SKY_DEGREES:
            Memd[w] = reflng
        case SKY_RADIANS:
            Memd[w] = RADTODEG(reflng)
        case SKY_HOURS:
            Memd[w] = 15.0d0 * reflng
        default:
            Memd[w] = reflng
        }
        switch (latunits) {
        case SKY_DEGREES:
            Memd[w+1] = reflat
        case SKY_RADIANS:
            Memd[w+1] = RADTODEG(reflat)
        case SKY_HOURS:
            Memd[w+1] = 15.0d0 * reflat
        default:
            Memd[w+1] = reflat
        }
        call mw_swtermd (mw, Memd[r], Memd[w], Memd[cd], ndim)


        call sfree (sp)

        return (mw)
end

