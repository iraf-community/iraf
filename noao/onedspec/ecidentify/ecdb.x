include	<math/gsurfit.h>
include	"ecidentify.h"

# EC_DBREAD -- Read features data from the database.

procedure ec_dbread (ec, name, verbose)

pointer	ec				# ID pointer
char	name[SZ_LINE]
int	verbose

pointer	dt
int	i, j, ncoeffs, rec, slope, offset
double	shift
pointer	sp, coeffs, line

int	dtgeti(), dgsgeti(), dtlocate(), dtscan()
real	dtgetr()
double	dgsgetd()
pointer	dtmap1()

errchk	dtmap1(), dtlocate(), dtgeti(), dtgad()

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	call strcpy ("ec", Memc[line], SZ_LINE)
	call imgcluster (name, Memc[line+2], SZ_LINE)
	dt = dtmap1 (Memc[EC_DATABASE(ec)], Memc[line], READ_ONLY)

	call sprintf (Memc[line], SZ_LINE, "ecidentify %s")
	    call pargstr (name)

	rec = dtlocate (dt, Memc[line])
	if (rec == EOF)
	    call error (0, "Entry not found")

	i = dtgeti (dt, rec, "features")

	EC_NALLOC(ec) = i
	EC_NFEATURES(ec) = i
	EC_CURRENT(ec) = 0
	call realloc (EC_APNUM(ec), i, TY_INT)
	call realloc (EC_LINENUM(ec), i, TY_INT)
	call realloc (EC_ORD(ec), i, TY_INT)
	call realloc (EC_PIX(ec), i, TY_DOUBLE)
	call realloc (EC_FIT(ec), i, TY_DOUBLE)
	call realloc (EC_USER(ec), i, TY_DOUBLE)
	call realloc (EC_FWIDTHS(ec), i, TY_REAL)
	call realloc (EC_FTYPES(ec), i, TY_INT)

	do i = 1, EC_NFEATURES(ec) {
	    j = dtscan (dt)
	    call gargi (AP(ec,i))
	    call gargi (ORDER(ec,i))
	    call gargd (PIX(ec,i))
	    call gargd (FIT(ec,i))
	    call gargd (USER(ec,i))
	    call gargr (FWIDTH(ec,i))
	    call gargi (FTYPE(ec,i))
	}

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

	    EC_NEWECF(ec) = YES
	    EC_CURRENT(ec) = min (1, EC_NFEATURES(ec))
	} then
		;

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
		call pargstr (name)
	}
end


# EC_DBWRITE -- Write features data to the database.

procedure ec_dbwrite (ec, name, verbose)

pointer	ec				# ID pointer
char	name[ARB]
int	verbose

int	i, ncoeffs
pointer	dt, sp, coeffs, root

int	dgsgeti(), ecf_geti()
double	ecf_getd()
pointer	dtmap1(), immap()

errchk	dtmap1, immap

begin
	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)

	call strcpy ("ec", Memc[root], SZ_FNAME)
	call imgcluster (name, Memc[root+2], SZ_FNAME)
	dt = dtmap1 (Memc[EC_DATABASE(ec)], Memc[root], APPEND)

	call dtptime (dt)
	call dtput (dt, "begin\tecidentify %s\n")
	    call pargstr (name)
	call dtput (dt, "\tid\t%s\n")
	    call pargstr (name)
	call dtput (dt, "\ttask\tecidentify\n")
	call dtput (dt, "\timage\t%s\n")
	    call pargstr (Memc[EC_IMAGE(ec)])

	call dtput (dt, "\tfeatures\t%d\n")
	    call pargi (EC_NFEATURES(ec))
	do i = 1, EC_NFEATURES(ec) {
	    call dtput (dt, "\t\t%3d  %3d  %7.2f  %10.8g  %10.8g  %4.1f  %d\n")
		call pargi (AP(ec,i))
		call pargi (ORDER(ec,i))
		call pargd (PIX(ec,i))
		call pargd (FIT(ec,i))
		call pargd (USER(ec,i))
		call pargr (FWIDTH(ec,i))
		call pargi (FTYPE(ec,i))
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
		call pargstr (name)
	}

	call sfree (sp)

	# Enter reference spectrum name in image header.
	dt = immap (Memc[EC_IMAGE(ec)], READ_WRITE, 0)
	call imastr (dt, "REFSPEC1", Memc[EC_IMAGE(ec)])
	iferr (call imdelf (dt, "REFSPEC2"))
	    ;
	call imunmap (dt)
end
