include <math/gsurfit.h>
include <smw.h>
include <units.h>
include "ecidentify.h"

# EC_DBREAD -- Read features data from the database.

procedure ec_dbread (ec, name, verbose)

pointer ec                              # ID pointer
char    name[SZ_LINE]
int     verbose

pointer dt
int     i, j, k, ncoeffs, rec, slope, offset, niterate
double  shift, low, high
pointer sp, coeffs, line, cluster, un

int     ec_line()
int     dtgeti(), dgsgeti(), dtlocate(), dtscan(), nscan()
real    dtgetr()
bool	un_compare()
double  dgsgetd(), smw_c1trand()
pointer dtmap1(), un_open()

errchk  dtmap1, dtlocate, dtgeti, dtgad, un_open

begin
        call smark (sp)
        call salloc (cluster, SZ_LINE, TY_CHAR)
        call salloc (line, SZ_LINE, TY_CHAR)

        call imgcluster (name, Memc[cluster], SZ_LINE)
        call sprintf (Memc[line], SZ_LINE, "ec%s")
            call pargstr (Memc[cluster])
        dt = dtmap1 (Memc[EC_DATABASE(ec)], Memc[line], READ_ONLY)

        call sprintf (Memc[line], SZ_LINE, "ecidentify %s")
            call pargstr (Memc[cluster])

        rec = dtlocate (dt, Memc[line])
        if (rec == EOF)
            call error (0, "Entry not found")

        i = dtgeti (dt, rec, "features")

        EC_NALLOC(ec) = i
        call realloc (EC_APNUM(ec), i, TY_INT)
        call realloc (EC_LINENUM(ec), i, TY_INT)
        call realloc (EC_ORD(ec), i, TY_INT)
        call realloc (EC_PIX(ec), i, TY_DOUBLE)
        call realloc (EC_FIT(ec), i, TY_DOUBLE)
        call realloc (EC_USER(ec), i, TY_DOUBLE)
        call realloc (EC_FWIDTHS(ec), i, TY_REAL)
        call realloc (EC_FTYPES(ec), i, TY_INT)

        j = 1
        do i = 1, EC_NALLOC(ec) {
            k = dtscan (dt)
            call gargi (APN(ec,j))
            call gargi (ORDER(ec,j))
            call gargd (PIX(ec,j))
            call gargd (FIT(ec,j))
            call gargd (USER(ec,j))
            call gargr (FWIDTH(ec,j))
            call gargi (FTYPE(ec,j))
            call gargi (k)
            if (nscan() == 8 && k == 0)
                FTYPE(ec,j) = -FTYPE(ec,j)
            iferr (LINE(ec,j) = ec_line (ec, APN(ec,j)))
                next
            shift = smw_c1trand (EC_PL(ec), PIX(ec,j))
            low = 0.5
            high = SN(SH(ec,LINE(ec,j))) + 0.5
            if (shift < low || shift > high)
                next
            j = j + 1
        }
        EC_NFEATURES(ec) = j - 1

        iferr (shift = dtgetr (dt, rec, "shift"))
            shift = 0.
        iferr (offset = dtgeti (dt, rec, "offset"))
            offset = 0
        iferr (slope = dtgeti (dt, rec, "slope"))
            slope = 1
        call ecf_setd ("shift", shift)
        call ecf_seti ("offset", offset)
        call ecf_seti ("slope", slope)

        iferr {
            ncoeffs = dtgeti (dt, rec, "coefficients")
            call salloc (coeffs, ncoeffs, TY_DOUBLE)
            call dtgad (dt, rec, "coefficients", Memd[coeffs], ncoeffs, ncoeffs)

            if (EC_ECF(ec) != NULL)
                call dgsfree (EC_ECF(ec))
            call dgsrestore (EC_ECF(ec), Memd[coeffs])

            call ecf_setd ("xmin", dgsgetd (EC_ECF(ec), GSXMIN))
            call ecf_setd ("xmax", dgsgetd (EC_ECF(ec), GSXMAX))
            call ecf_setd ("ymin", dgsgetd (EC_ECF(ec), GSYMIN))
            call ecf_setd ("ymax", dgsgetd (EC_ECF(ec), GSYMAX))
            call ecf_seti ("xorder", dgsgeti (EC_ECF(ec), GSXORDER))
            call ecf_seti ("yorder", dgsgeti (EC_ECF(ec), GSYORDER))

            switch (dgsgeti (EC_ECF(ec), GSTYPE)) {
            case GS_LEGENDRE:
                call ecf_sets ("function", "legendre")
            case GS_CHEBYSHEV:
                call ecf_sets ("function", "chebyshev")
            }

            ifnoerr (niterate = dtgeti (dt, rec, "niterate"))
                call ecf_seti ("niterate", niterate)
            ifnoerr (low = dtgetr (dt, rec, "lowreject"))
                call ecf_setd ("low", low)
            ifnoerr (high = dtgeti (dt, rec, "highreject"))
                call ecf_setd ("high", high)

            EC_NEWECF(ec) = YES
            EC_CURRENT(ec) = min (1, EC_NFEATURES(ec))
        } then
                ;

	ifnoerr (call dtgstr (dt, rec, "units", Memc[line], SZ_LINE)) {
	    if (EC_UN(ec) == NULL)
		EC_UN(ec) = un_open (Memc[line])
	    else {
		un = un_open (Memc[line])
		if (!un_compare (un, EC_UN(ec))) {
		    call ec_unitsll (ec, Memc[line])
		    call un_close (EC_UN(ec))
		    EC_UN(ec) = un
		} else
		    call un_close (un)
	    }
	}

        call dtunmap (dt)
        call sfree (sp)

        if (EC_NFEATURES(ec) > 0) {
            EC_NEWGRAPH(ec) = YES
            EC_NEWFEATURES(ec) = YES
            EC_CURRENT(ec) = 1
        } else
            EC_CURRENT(ec) = 0

        if (verbose == YES) {
            call printf ("ecidentify %s\n")
                call pargstr (Memc[cluster])
        }
end


# EC_DBWRITE -- Write features data to the database.

procedure ec_dbwrite (ec, name, verbose)

pointer ec                              # ID pointer
char    name[ARB]
int     verbose

int     i, ncoeffs
pointer dt, sp, coeffs, root, cluster

int     dgsgeti(), ecf_geti()
double  ecf_getd()
pointer dtmap1(), immap()

errchk  dtmap1, immap

begin
        call smark (sp)
        call salloc (cluster, SZ_FNAME, TY_CHAR)
        call salloc (root, SZ_FNAME, TY_CHAR)

        call imgcluster (name, Memc[cluster], SZ_FNAME)
        call sprintf (Memc[root], SZ_FNAME, "ec%s")
            call pargstr (Memc[cluster])
        dt = dtmap1 (Memc[EC_DATABASE(ec)], Memc[root], APPEND)

        call dtptime (dt)
        call dtput (dt, "begin\tecidentify %s\n")
            call pargstr (Memc[cluster])
        call dtput (dt, "\tid\t%s\n")
            call pargstr (Memc[cluster])
        call dtput (dt, "\ttask\tecidentify\n")
        call dtput (dt, "\timage\t%s\n")
            call pargstr (Memc[EC_IMAGE(ec)])

	if (EC_UN(ec) != NULL) {
	    call dtput (dt, "\tunits\t%s\n")
		call pargstr (UN_UNITS(EC_UN(ec)))
	}
        call dtput (dt, "\tfeatures\t%d\n")
            call pargi (EC_NFEATURES(ec))
        do i = 1, EC_NFEATURES(ec) {
            call dtput (dt,
                "\t\t%3d  %3d  %7.2f  %10.9g  %10.9g  %4.1f  %d  %d\n")
                call pargi (APN(ec,i))
                call pargi (ORDER(ec,i))
                call pargd (PIX(ec,i))
                call pargd (FIT(ec,i))
                call pargd (USER(ec,i))
                call pargr (FWIDTH(ec,i))
                call pargi (abs (FTYPE(ec,i)))
                if (FTYPE(ec,i) > 0)
                    call pargi (1)
                else
                    call pargi (0)
        }

        if (ecf_getd ("shift") != 0.) {
            call dtput (dt, "\tshift\t%g\n")
                call pargd (ecf_getd ("shift"))
        }
        if (ecf_geti ("offset") != 0) {
            call dtput (dt, "\toffset\t%d\n")
                call pargi (ecf_geti ("offset"))
        }
        if (ecf_geti ("slope") != 1) {
            call dtput (dt, "\tslope\t%d\n")
                call pargi (ecf_geti ("slope"))
        }

        if (EC_ECF(ec) != NULL) {
            call dtput (dt, "\tniterate %d\n")
                call  pargi (ecf_geti ("niterate"))
            call dtput (dt, "\tlowreject %g\n")
                call  pargd (ecf_getd ("low"))
            call dtput (dt, "\thighreject %g\n")
                call  pargd (ecf_getd ("high"))

            ncoeffs = dgsgeti (EC_ECF(ec), GSNSAVE)
            call salloc (coeffs, ncoeffs, TY_DOUBLE)
            call dgssave (EC_ECF(ec), Memd[coeffs])
            call dtput (dt, "\tcoefficients\t%d\n")
                call pargi (ncoeffs)
            do i = 1, ncoeffs {
                call dtput (dt, "\t\t%g\n")
                    call pargd (Memd[coeffs+i-1])
            }
        }

        call dtput (dt, "\n")
        call dtunmap (dt)

        EC_NEWFEATURES(ec) = NO
        EC_NEWECF(ec) = NO
        EC_NEWDBENTRY(ec) = NO

        if (verbose == YES) {
            call printf ("ecidentify %s\n")
                call pargstr (Memc[cluster])
        }

        # Enter reference spectrum name in image header.
        call imgcluster (Memc[EC_IMAGE(ec)], Memc[root], SZ_FNAME)
        dt = immap (Memc[root], READ_WRITE, 0)
        call imastr (dt, "REFSPEC1", Memc[cluster])
        iferr (call imdelf (dt, "REFSPEC2"))
            ;
        call imunmap (dt)

        call sfree (sp)
end
